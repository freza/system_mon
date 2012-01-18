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

-module(counter).

-export([inc/1, inc/2, set/2, del/1, read/1, read_all/0, match/1]).

%%% Event counters are integer values not decreasing in time.

inc(Key) ->
    inc(Key, 1).

inc({_, _, _} = Key, Inc) when is_integer(Inc), Inc >= 0 ->
    try ets:update_counter(system_mon_cnt, Key, Inc) catch
	error : badarg ->
	    case ets:insert_new(system_mon_cnt, {Key, Inc}) of
		false ->
		    ets:update_counter(system_mon_cnt, Key, Inc);
		_ ->
		    Inc
	    end
    end.

set({_, _, _} = Key, Val) when is_integer(Val), Val >= 0 ->
    ets:insert(system_mon_cnt, {Key, Val}).

del({_, _, _} = Key) ->
    ets:delete(system_mon_cnt, Key).

read({_, _, _} = Key) ->
    case ets:lookup(system_mon_cnt, Key) of
	[Item] ->
	    {ok, Item};
	[] ->
	    not_found
    end.

read_all() ->
    match({'_', '_'}).

match(Head) ->
    ets:safe_fixtable(system_mon_cnt, true),
    try
	ets:select(system_mon_cnt, [{{Head, '_'}, [], ['$_']}])
    after
	ets:safe_fixtable(system_mon_cnt, false)
    end.
