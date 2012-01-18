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

-module(system_mon_dif).
-behaviour(gen_server).

-export([start_link/2, start_feed/2, abort_feed/1, set_owner/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-import(system_mon_lib, [strip_key/1]).

%%%

start_link(Feed, Mod) ->
    gen_server:start_link({local, Feed}, ?MODULE, [Mod, self()], []).

start_feed(Feed, Args) ->
    gen_server:call(Feed, {start_feed, Args}).

abort_feed(Feed) ->
    gen_server:call(Feed, abort_feed).

set_owner(Feed, Pid) ->
    gen_server:call(Feed, {set_owner, Pid}).

%%%

worker_crash(Pid) ->
    gen_server:cast(Pid, worker_crash).

worker_done(Pid) ->
    gen_server:call(Pid, {worker_done, self()}).

%%%

-record(state, {
	  cnt, 			%% Shadow counters. 				:: tid()
	  avg, 			%% Shadow averages. 				:: tid()
	  hst, 			%% Shadow histograms. 				:: tid()
	  workers, 		%% Active workers. 				:: ordsets(pid())
	  mod, 			%% Callback module. 				:: atom()
	  impl, 		%% Callback state. 				:: term() | nil
	  status, 		%% Activity status. 				:: normal | failed | aborted
	  owner 		%% Owner process. 				:: pid()
	 }).

init([Mod, Owner]) ->
    link(Owner),
    %% Shadow tables storing previous values.
    Aux_cnt = ets:new(anon, [ordered_set, public]),
    Aux_avg = ets:new(anon, [ordered_set, public]),
    Aux_hst = ets:new(anon, [ordered_set, public]),
    {ok, #state{cnt = Aux_cnt, avg = Aux_avg, hst = Aux_hst, workers = [],
		mod = Mod, impl = nil, status = nil, owner = Owner}}.

handle_call({start_feed, Args}, _, #state{cnt = Cnt, avg = Avg, hst = Hst, workers = Workers, mod = Mod} = State) ->
    case Workers of
	[] ->
	    {ok, Impl} = Mod:start_feed(Args),
	    Parent = self(),
	    Cnt_pid = proc_lib:spawn_link(fun () -> worker(Parent, system_mon_cnt, Cnt, counter, Mod, Impl) end),
	    Avg_pid = proc_lib:spawn_link(fun () -> worker(Parent, system_mon_avg, Avg, average, Mod, Impl) end),
	    Hst_pid = proc_lib:spawn_link(fun () -> worker(Parent, system_mon_hst, Hst, density, Mod, Impl) end),
	    Workers2 = ordsets:from_list([Cnt_pid, Avg_pid, Hst_pid]),
	    {reply, ok, State#state{workers = Workers2, impl = Impl, status = normal}};
	_ ->
	    {reply, {error, busy}, State}
    end;
handle_call(abort_feed, _, #state{workers = Workers} = State) ->
    case Workers of
	[_ | _] ->
	    [(catch Worker ! worker_stop) || Worker <- Workers],
	    {reply, ok, State#state{status = aborted}};
	[] ->
	    {reply, {error, not_running}, State}
    end;
handle_call({worker_done, Pid}, _, #state{workers = Workers, mod = Mod, impl = Impl, status = Status} = State) ->
    case ordsets:del_element(Pid, Workers) of
	[] ->
	    Mod:stop_feed(Status, Impl),
	    {reply, ok, State#state{workers = [], impl = nil, status = nil}};
	L ->
	    {reply, ok, State#state{workers = L}}
    end;
handle_call(worker_crash, _, #state{workers = Workers} = State) ->
    [(catch Worker ! worker_stop) || Worker <- Workers],
    {reply, ok, State#state{status = failed}};
handle_call({set_owner, New}, _, #state{owner = Old} = State) ->
    %% NB ordered so that we're always linked to someone.
    if New /= Old ->
	    link(New),
	    unlink(Old);
       true ->
	    ok
    end,
    {reply, ok, State#state{owner = New}};
handle_call(_, _, State) ->
    {reply, {error, bad_request}, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.

%%% Worker logic propagating changes to callback module.

worker(Parent, Cur_tab, Aux_tab, Kind, Mod, Impl) ->
    try
	Begin_ts = now(),
	table_diff(Cur_tab, ets:first(Cur_tab), Aux_tab, ets:first(Aux_tab), Kind, Mod, Impl),
	Finish_ts = now(),
	density:rec({system_mon, {Kind, Mod}, duration}, timer:now_diff(Finish_ts, Begin_ts) / 1000000),
	worker_done(Parent)
    catch
	throw : {callback_crash, Exn, Rsn, Stack} ->
	    error_logger:error_report([{"Sysmon Module", Mod}, {"Sysmon State", Impl}, {"Sysmon Table", Kind},
				       {"Class", Exn}, {"Reason", Rsn}, {"Stacktrace", Stack}]),
	    worker_crash(Parent),
	    worker_done(Parent);
	throw : worker_stop ->
	    worker_done(Parent)
    end.

%% Zip the two tables handling differences on the go.
table_diff(_, '$end_of_table', _, '$end_of_table', _, _, _) ->
    ok;

table_diff(_, '$end_of_table', Aux_tab, Aux_key, Kind, Mod, Impl) ->
    delete_rest(Aux_tab, Aux_key, Kind, Mod, Impl);

table_diff(Cur_tab, Cur_key, Aux_tab, '$end_of_table', Kind, Mod, Impl) ->
    create_rest(Cur_tab, Cur_key, Aux_tab, Kind, Mod, Impl);

table_diff(Cur_tab, Cur_key, Aux_tab, Aux_key, Kind, Mod, Impl) when Cur_key == Aux_key ->
    case ets:lookup(Cur_tab, Cur_key) of
	[Cur_row] ->
	    case ets:lookup(Aux_tab, Aux_key) of
	        [Aux_row] when Aux_row == Cur_row->
	            ok;
	        [Aux_row] ->
	            callback(Mod, handle_update, [Kind, Cur_key, strip_key(Cur_row), strip_key(Aux_row), Impl])
            end;
	_ ->
	    ok
    end,
    table_diff(Cur_tab, ets:next(Cur_tab, Cur_key), Aux_tab, ets:next(Aux_tab, Aux_key), Kind, Mod, Impl);

table_diff(Cur_tab, Cur_key, Aux_tab, Aux_key, Kind, Mod, Impl) when Cur_key < Aux_key ->
    case ets:lookup(Cur_tab, Cur_key) of
	[Cur_row] ->
	    callback(Mod, handle_create, [Kind, Cur_key, strip_key(Cur_row), Impl]),
	    ets:insert(Aux_tab, Cur_row);
	_ ->
	    ok
    end,
    table_diff(Cur_tab, ets:next(Cur_tab, Cur_key), Aux_tab, Aux_key, Kind, Mod, Impl);

table_diff(Cur_tab, Cur_key, Aux_tab, Aux_key, Kind, Mod, Impl) when Cur_key > Aux_key ->
    callback(Mod, handle_delete, [Kind, Aux_key, Impl]),
    ets:delete(Aux_tab, Aux_key),
    table_diff(Cur_tab, Cur_key, Aux_tab, ets:next(Aux_tab, Aux_key), Kind, Mod, Impl).

%% Delete excess entries at the end of Aux_tab.
delete_rest(_, '$end_of_table', _, _, _) ->
    ok;
delete_rest(Aux_tab, Key, Kind, Mod, Impl) ->
    callback(Mod, handle_delete, [Kind, Key, Impl]),
    ets:delete(Aux_tab, Key),
    delete_rest(Aux_tab, ets:next(Aux_tab, Key), Kind, Mod, Impl).

%% Copy excess entries from tail of Cur_tab to Aux_tab.
create_rest(_, '$end_of_table', _, _, _, _) ->
    ok;
create_rest(Cur_tab, Cur_key, Aux_tab, Kind, Mod, Impl) ->
    case ets:lookup(Cur_tab, Cur_key) of
	[Cur_row] ->
	    callback(Mod, handle_create, [Kind, Cur_key, strip_key(Cur_row), Impl]),
	    ets:insert(Aux_tab, Cur_row);
	_ ->
	    ok
    end,
    create_rest(Cur_tab, ets:next(Cur_tab, Cur_key), Aux_tab, Kind, Mod, Impl).

callback(Mod, Fun, Args) ->
    try apply(Mod, Fun, Args) catch
	Exn : Rsn ->
            Arity = length(Args),
            case erlang:get_stacktrace() of
                [{Mod, Fun, Arity} | L] ->
                    Stack = [{Mod, Fun, Args} | L];
                L ->
                    Stack = L
            end,
	    throw({callback_crash, Exn, Rsn, Stack})
    end,
    receive
	worker_stop ->
	    throw(worker_stop)
    after 0 ->
	    ok
    end.
