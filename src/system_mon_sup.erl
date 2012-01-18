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

-module(system_mon_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%

init([]) ->
    ets:new(system_mon_cnt, [ordered_set, named_table, public]),
    ets:new(system_mon_avg, [ordered_set, named_table, public]),
    ets:new(system_mon_hst, [ordered_set, named_table, public]),
    {ok, {{one_for_one, 1, 10}, children()}}.

%%%

children() ->
    [{system_mon_dif_sup, {system_mon_dif_sup, start_link, []}, permanent, 60000, supervisor, [system_mon_dif_sup]},
     {system_mon_sys, {system_mon_sys, start_link, []}, permanent, 60000, worker, [system_mon_sys]},
     {system_mon_log, {system_mon_log, start_link, []}, permanent, 60000, worker, [system_mon_log]}].
