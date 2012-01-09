%%% Copyright (c) 2011 Jachym Holecek <freza@circlewave.net>
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

-module(sysmon_sys).
-vsn(' $Id$ ').
-url(' $URL$ ').
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-import(sysmon_lib, [get_env/3, get_value/3]).
-import(lists, [foldl/3, usort/1]).

%%%

start_link() ->
    gen_server:start_link(?MODULE, [], []).

done(Pid) ->
    gen_server:call(Pid, {done, self()}).

%%%

-record(state, {
	  update_tmr, 		%% Update timer reference. 			:: reference()
	  workers 		%% Workers we're waiting for. 			:: ordsets(pid())
	 }).

init([]) ->
    {ok, #state{update_tmr = schedule_update(), workers = []}}.

handle_call({done, Pid}, _, #state{workers = Pids} = State) ->
    {reply, ok, State#state{workers = ordsets:del_element(Pid, Pids)}};
handle_call(_, _, State) ->
    {reply, {error, bad_request}, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({timeout, Ref, update}, #state{update_tmr = Ref, workers = Workers1} = State) ->
    %% Safety precaution: if it takes too long, don't make matters worse.
    case Workers1 of
	[] ->
	    Parent = self(),
	    Leaders = dict:from_list(ets:select(ac_tab, [{{{application_master, '$1'}, '$2'}, [], [{{'$2', '$1'}}]}])),
	    Apps = usort(['Other' | [App || {App, _, _} <- application:loaded_applications()]]),
	    Pid_worker = proc_lib:spawn_link(fun () -> pid_worker(Parent, Apps, Leaders) end),
	    Tid_worker = proc_lib:spawn_link(fun () -> tid_worker(Parent, Apps, Leaders) end),
	    Sys_worker = proc_lib:spawn_link(fun () -> sys_worker(Parent) end),
	    Workers2 = ordsets:from_list([Pid_worker, Tid_worker, Sys_worker]);
	_ ->
	    Workers2 = Workers1
    end,
    {noreply, State#state{update_tmr = schedule_update(), workers = Workers2}};
handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.

%%% Synthesis of application metrics: processes.

pid_worker(Parent, Apps, Leaders) ->
    [pid_write(Pid_stats) || Pid_stats <- pid_stats(Apps, Leaders)],
    done(Parent).

pid_write({App, {Count, Memory, Mqueue}}) ->
    average:rec({application, App, proc_cnt}, Count),
    average:rec({application, App, proc_mem}, Memory),
    average:rec({application, App, proc_msg}, Mqueue).

pid_stats(Apps, Leaders) ->
    dict:to_list(foldl(fun (Pid, Acc) -> pid_fold(Pid, Acc, Leaders) end,
		       dict:from_list([{App, {0, 0, 0}} || App <- Apps]),
		       processes())).

pid_fold(Pid, Acc, Leaders) ->
    case pid_to_app(Pid, Leaders, 'Other') of
	{ok, App} ->
	    case dict:is_key(App, Acc) of
		true ->
		    dict:update(App, fun (Total) -> pid_merge(pid_info(Pid), Total) end, Acc);
		_ ->
		    Acc
	    end;
	_ ->
	    Acc
    end.

pid_merge(TVL, {Total_cnt, Total_heap, Total_msgs}) ->
    HS = words_to_bytes(get_value(total_heap_size, TVL, 0)),
    ML = get_value(message_queue_len, TVL, 0),
    {Total_cnt + 1, Total_heap + HS, Total_msgs + ML}.

pid_info(Pid) ->
    case process_info(Pid, [total_heap_size, message_queue_len]) of
	Val when is_list(Val) ->
	    Val;
	undefined ->
	    []
    end.

%%% Synthesis of application metrics: ETS tables.

tid_worker(Parent, Apps, Leaders) ->
    [tid_write(Tid_stats) || Tid_stats <- tid_stats(Apps, Leaders)],
    done(Parent).

tid_write({App, {Count, Items, Memory}}) ->
    average:rec({application, App, tab_cnt}, Count),
    average:rec({application, App, tab_len}, Items),
    average:rec({application, App, tab_mem}, Memory).

tid_stats(Apps, Leaders) ->
    dict:to_list(foldl(fun (Tid, Acc) -> tid_fold(Tid, Acc, Leaders) end,
		       dict:from_list([{App, {0, 0, 0}} || App <- Apps]),
		       ets:all())).

tid_fold(Tid, Acc, Leaders) ->
    case ets:info(Tid) of
	TVL when is_list(TVL) ->
	    case dict:is_key(App = tid_to_app(TVL, Leaders), Acc) of
		true ->
		    dict:update(App, fun (Total) -> tid_merge(TVL, Total) end, Acc);
		_ ->
		    Acc
	    end;
	_ ->
	    Acc
    end.

tid_merge(TVL, {Table_cnt, Items_cnt, Memory}) ->
    Mem = words_to_bytes(get_value(memory, TVL, 0)),
    Len = get_value(size, TVL, 0),
    {Table_cnt + 1, Items_cnt + Len, Memory + Mem}.

tid_to_app(TVL, Leaders) ->
    case pid_to_app(get_value(owner, TVL, nil), Leaders, Def = 'Other') of
	zombie ->
	    case pid_to_app(get_value(heir, TVL, nil), Leaders, Def) of
		{ok, App} ->
		    App;
		_ ->
		    Def
	    end;
	{ok, App} ->
	    App
    end.

%%% Synthesis of system metrics.

sys_worker(Parent) ->
    mnesia_cnt(),
    erts_cnt(),
    erts_avg(),
    host_avg(),
    done(Parent).

mnesia_cnt() ->
    counter:set({mnesia, transaction, failures}, mnesia_info(transaction_failures)),
    counter:set({mnesia, transaction, commits}, mnesia_info(transaction_commits)),
    counter:set({mnesia, transaction, restarts}, mnesia_info(transaction_restarts)),
    counter:set({mnesia, transaction, logwrites}, mnesia_info(transaction_log_writes)).

erts_cnt() ->
    {{input, Ior}, {output, Iow}} = erlang:statistics(io),
    {Gc_cnt, Gc_freed, _} = erlang:statistics(garbage_collection),
    {Context_sw, _} = erlang:statistics(context_switches),
    {Reds, _} = erlang:statistics(reductions),
    counter:set({erts, system, port_rx}, Ior),
    counter:set({erts, system, port_tx}, Iow),
    counter:set({erts, system, gc_count}, Gc_cnt),
    counter:set({erts, system, gc_freed}, Gc_freed),
    counter:set({erts, system, reductions}, Reds),
    counter:set({erts, system, proc_sched}, Context_sw).

erts_avg() ->
    Memory = erlang:memory(),
    Runq = erlang:statistics(run_queue),
    average:rec({erts, memory, atom}, get_value(atom, Memory, 0)),
    average:rec({erts, memory, binary}, get_value(binary, Memory, 0)),
    average:rec({erts, memory, code}, get_value(code, Memory, 0)),
    average:rec({erts, memory, ets}, get_value(ets, Memory, 0)),
    average:rec({erts, memory, processes}, get_value(processes, Memory, 0)),
    average:rec({erts, memory, total}, get_value(total, Memory, 0)),
    average:rec({erts, system, proc_cnt}, erlang:system_info(process_count)),
    average:rec({erts, system, run_queue}, Runq).

host_avg() ->
    {Used_mem, Free_mem, Used_swap, Free_swap} = host_memory(),
    {Proc_cnt, Load_avg} = host_load(),
    average:rec({os, system, used_mem}, Used_mem),
    average:rec({os, system, free_mem}, Free_mem),
    average:rec({os, system, used_swap}, Used_swap),
    average:rec({os, system, free_swap}, Free_swap),
    average:rec({os, system, proc_cnt}, Proc_cnt),
    average:rec({os, system, load_avg}, Load_avg).

host_memory() ->
    try memsup:get_system_memory_data() of
	Memory ->
	    Total_mem = get_value(total_memory, Memory, 0),
	    Free_mem = get_value(free_memory, Memory, 0),
	    Total_swap = get_value(total_swap, Memory, 0),
	    Free_swap = get_value(free_swap, Memory, 0),
	    {Total_mem - Free_mem, Free_mem, Total_swap - Free_swap, Free_swap}
    catch
	_ : _ ->
	    {0, 0, 0, 0}
    end.

host_load() ->
    try
	Count = cpu_sup:nprocs(),
	Load = cpu_sup:avg1(),
	{Count, Load}
    catch
	_ : _ ->
	    {0, 0}
    end.

%%% Implementation.

schedule_update() ->
    erlang:start_timer(get_env(sysmon, systat_period, 300) * 1000, self(), update).

mnesia_info(Key) ->
    try mnesia:system_info(Key) catch
	_ : _ ->
	    0
    end.

pid_to_app(Nil, _, _) when is_atom(Nil) ->
    zombie;
pid_to_app(Pid, Leaders, Def) ->
    case process_info(Pid, group_leader) of
	{_, Leader} ->
	    case dict:find(Leader, Leaders) of
		{ok, _} = Ok ->
		    Ok;
		error ->
		    {ok, Def}
	    end;
	undefined ->
	    zombie
    end.

words_to_bytes(Words) ->
    Words * erlang:system_info(wordsize).
