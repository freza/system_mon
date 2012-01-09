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

-module(average).

-export([rec/2, del/1, read/1, read_all/0, read_all/1, read_sel/1]).

-import(lists, [foldl/3]).

%%% Samples averaged over reporting period.

rec({_, _, _} = Key, Val) when is_integer(Val), Val >= 0 ->
    case update(Key, Val) of
	not_found ->
	    case ets:insert_new(sysmon_avg, {Key, 1, Val}) of
		false ->
		    update(Key, Val);
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end.

del({_, _, _} = Key) ->
    ets:delete(sysmon_avg, Key).

read({Tab, Scope, Inst}) ->
    case ets:lookup(sysmon_avg, {Tab, Scope, Inst}) of
	[Item] ->
	    {ok, average(Item)};
	[] ->
	    not_found
    end.

read_all() ->
    read_sel({'_', '_', '_'}).

read_all(Tab) ->
    read_sel({Tab, '_', '_'}).

read_sel(Head) ->
    ets:safe_fixtable(sysmon_avg, true),
    try
	read_sel(ets:select(sysmon_avg, [{{Head, '_', '_'}, [], ['$_']}], 100), [])
    after
	ets:safe_fixtable(sysmon_avg, false)
    end.

%%%

read_sel({Items, Cont}, Acc) ->
    read_sel(ets:select(Cont), foldl(fun ({K, _, _} = X, A) -> [{K, average(X)} | A] end, Acc, Items));
read_sel('$end_of_table', Acc) ->
    Acc.

update(Key, Val) ->
    try
	ets:update_counter(sysmon_avg, Key, [{2, 1}, {3, Val}]),
	ok
    catch
	error : badarg ->
	    not_found
    end.

average({_, Cnt, Sum}) ->
    round(Sum / Cnt).
