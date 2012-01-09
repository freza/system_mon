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

-module(sysmon).

-export([add_counter/3, add_average/3, add_histogram/3]).
-export([del_counter/2, del_average/2, del_histogram/2]).
-export([create_db/0, create_db/1]).
-export([behaviour_info/1]).

-include("sysmon_db.hrl").

%%% Interface for sysmon_feed callbacks.

behaviour_info(callbacks) ->
    [{start_feed, 1}, 		%% Mod:start_feed(Args) -> {ok, Impl}
     {handle_create, 4}, 	%% Mod:handle_create(Kind, Key, Cur_val, State) -> _
     {handle_update, 5}, 	%% Mod:handle_update(Kind, Key, Cur_val, Old_val, State) -> _
     {handle_delete, 3}, 	%% Mod:handle_delete(Kind, Key, State) -> _
     {stop_feed, 2}]; 		%% Mod:stop_feed(Reason, State) -> _
behaviour_info(_) ->
    undefined.

%%% Management interface.

add_counter(Tab, Inst, Opts) ->
    xxx.

add_average(Tab, Inst, Opts) ->
    xxx.

%% XXX histograms have nontrivial options, the rest just wants to define units... worth the hassle?
add_histogram(Tab, Inst, Opts) ->
    xxx.

del_counter(Tab, Inst) ->
    xxx.

del_average(Tab, Inst) ->
    xxx.

del_histogram(Tab, Inst) ->
    xxx.

%%% System interface. Normally not needed, but allow creating configuration table manually.

create_db() ->
    create_db([{disc_copies, [node()]}, {local_content, true}]).

create_db(Opts) ->
    mnesia:create_table(density_conf, [{attributes, record_info(fields, density_conf)},
                                         {type, set} | Opts]).
