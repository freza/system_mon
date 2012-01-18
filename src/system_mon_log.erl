%%% Copyright (c) 2011-2012 Jachym Holecek <freza@circlewave.net>
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright
%%%    notice, this list of conditions and the following disclaimer.
%%% 2. Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
%%% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
%%% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.

-module(system_mon_log).
-behaviour(gen_server).
-behaviour(system_mon).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_feed/1, handle_create/4, handle_update/5, handle_delete/3, stop_feed/2]).

-import(system_mon_lib, [get_env/3]).

%%%

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%% Generic server.

-record(state, {
	  name_tab, 		%% Cache preformatted CSV counter name rows. 		:: tid()
	  update_tmr 		%% Periodically log changed metrics. 			:: reference()
	 }).

init([]) ->
    %% NB initial value of Last_ts doesn't matter as all will go to handle_create/4 anyway.
    {ok, _} = system_mon_dif_sup:add_worker(system_mon_log, ?MODULE),
    Name_tab = ets:new(anon, [set, public, {read_concurrency, true}]),
    Timer = schedule_update(),
    {ok, #state{name_tab = Name_tab, update_tmr = Timer}}.

handle_call(_, _, State) ->
    {reply, {error, bad_request}, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({timeout, Ref, update}, #state{update_tmr = Ref, name_tab = Name_tab} = State) ->
    %% NB system_mon_dif won't run multiple instances of the feed at once as an overload protection measure.
    Log_ts = audit_log_lib:printable_date(calendar:local_time()),
    system_mon_dif:start_feed(system_mon_log, [Name_tab, Log_ts]),
    {noreply, State#state{update_tmr = schedule_update()}};
handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.

%%%

schedule_update() ->
    %% Align nicely to next multiple of log period, since midnight.
    Period = get_env(system_mon, log_period, 900),
    {_, {H, M, S}} = calendar:local_time(),
    Secs = H*3600 + M*60 + S,
    Next = ((Secs + Period) div Period) * Period,
    erlang:start_timer((Next - Secs) * 1000, self(), update).

%%% Sysmon feed.

-record(feed, {
	  name_tab, 		%% Cache preformatted counter name columns. 		:: tid()
	  log_ts 		%% Quoted printable timestamp of this feed. 		:: iolist()
	 }).

start_feed([Name_tab, Log_ts]) ->
    {ok, #feed{name_tab = Name_tab, log_ts = Log_ts}}.

handle_create(counter, Key, [Cur_val], #feed{name_tab = Name_tab, log_ts = Log_ts}) ->
    Row = [Log_ts, $,, insert_name(Name_tab, Key), $,, integer_to_list(Cur_val), $\n],
    audit_log:audit_msg(counter_log, Row);
handle_create(average, Key, [Cur_cnt, Cur_sum], #feed{name_tab = Name_tab, log_ts = Log_ts}) ->
    Row = [Log_ts, $,, insert_name(Name_tab, Key), $,, integer_to_list(round(Cur_sum / Cur_cnt)), $\n],
    audit_log:audit_msg(average_log, Row);
handle_create(density, Key, Cur_vals, #feed{name_tab = Name_tab, log_ts = Log_ts}) ->
    Row = [Log_ts, $,, insert_name(Name_tab, Key), [[$,, integer_to_list(N)] || N <- Cur_vals], $\n],
    audit_log:audit_msg(density_log, Row).

handle_update(counter, Key, [Cur_val], [Old_val], #feed{name_tab = Name_tab, log_ts = Log_ts}) ->
    Row = [Log_ts, $,, lookup_name(Name_tab, Key), $,, integer_to_list(Cur_val - Old_val), $\n],
    audit_log:audit_msg(counter_log, Row);
handle_update(average, Key, [Cur_cnt, Cur_sum], [Old_cnt, Old_sum], #feed{name_tab = Name_tab, log_ts = Log_ts}) ->
    Cnt = integer_to_list(Cur_cnt - Old_cnt),
    Sum = integer_to_list(Cur_sum - Old_sum),
    Row = [Log_ts, $,, lookup_name(Name_tab, Key), $,, Cnt, $,, Sum, $\n],
    audit_log:audit_msg(average_log, Row);
handle_update(density, Key, Cur_vals, Old_vals, #feed{name_tab = Name_tab, log_ts = Log_ts}) ->
    Avg = lists:zipwith(fun (Cur, Old) -> [$,, integer_to_list(Cur - Old)] end, Cur_vals, Old_vals),
    Row = [Log_ts, $,, lookup_name(Name_tab, Key), Avg, $\n],
    audit_log:audit_msg(density_log, Row).

handle_delete(_, Key, #feed{name_tab = Name_tab}) ->
    case ets:update_counter(Name_tab, Key, [{2, -1}]) of
	0 ->
	    ets:delete(Name_tab, Key);
	_ ->
	    ok
    end.

stop_feed(_, _) ->
    ok.

%%%

insert_name(Name_tab, {Tab, Scope, Inst} = Key) ->
    case ets:lookup(Name_tab, Key) of
	[{_, _, Desc}] ->
	    ets:update_counter(Name_tab, Key, [{2, 1}]),
	    Desc;
	[] ->
	    case ets:insert_new(Name_tab, {Key, 1, Desc = format_desc(Tab, Scope, Inst)}) of
		false ->
		    ets:update_counter(Name_tab, Key, [{2, 1}]);
		_ ->
		    ok
	    end,
	    Desc
    end.

lookup_name(Name_tab, Key) ->
    [{_, _, Desc}] = ets:lookup(Name_tab, Key),
    Desc.

format_desc(Tab, Scope, Inst) ->
    iolist_to_binary([q(Tab), $,, q(Scope), $,, q(Inst)]).

q(Term) ->
    %% RFC4180-compatible quoting of double-quote characters.
    [$", re:replace(io_lib:format("~1000p", [Term]), [$"], [$", $"], [global]), $"].
