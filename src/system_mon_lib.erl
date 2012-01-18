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

-module(system_mon_lib).

-export([get_env/3, get_value/2, get_value/3, logarithm/2, strip_key/1]).

%%%

%% Abstract away application evironment and also offer saner interface.
get_env(A, K, D) ->
    case application:get_env(A, K) of
	{ok, V} ->
	    V;
	_ ->
	    D
    end.

%% Return value of TVL item or default if absent.
get_value(K, L, D) ->
    case lists:keysearch(K, 1, L) of
	{value, {_, V}} ->
	    V;
	false ->
	    D
    end.

%% Return value of TVL item or explode if absent.
get_value(K, L) ->
    case lists:keysearch(K, 1, L) of
	{value, {_, V}} ->
	    V;
	false ->
	    exit({key_missing, K})
    end.

%% Calculate arbitrary logarithm making use of the identity log_N(X) = log_M(X)/log_M(N).
logarithm(Base, Value) ->
    math:log(Value) / math:log(Base).

%% Return row as a list without leading key element.
strip_key(Row) ->
    tl(tuple_to_list(Row)).
