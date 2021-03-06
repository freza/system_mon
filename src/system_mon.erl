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

-module(system_mon).

-export([add_density/2]).
-export([create_db/0, create_db/1]).
-export([behaviour_info/1]).

-import(system_mon_lib, [get_value/2, get_value/3]).

-include("system_mon_db.hrl").

%%% Interface for system_mon_dif callbacks.

behaviour_info(callbacks) ->
    [{start_feed, 1}, 		%% Mod:start_feed(Args) -> {ok, Impl}
     {handle_create, 4}, 	%% Mod:handle_create(Kind, Key, Cur_val, State) -> _
     {handle_update, 5}, 	%% Mod:handle_update(Kind, Key, Cur_val, Old_val, State) -> _
     {handle_delete, 3}, 	%% Mod:handle_delete(Kind, Key, State) -> _
     {stop_feed, 2}]; 		%% Mod:stop_feed(Reason, State) -> _
behaviour_info(_) ->
    undefined.

%%% Management interface.

add_density({Tab, '_', Inst}, Opts) ->
    case get_value(scale, Opts) of
	lin ->
	    add_linear(Tab, Inst, Opts);
	log ->
	    add_logarithmic(Tab, Inst, Opts)
    end.

create_db() ->
    create_db([{disc_copies, [node()]}, {local_content, true}]).

create_db(Opts) ->
    mnesia:create_table(density_conf, [{attributes, record_info(fields, density_conf)},
				       {type, set} | Opts]).

%%% Implementation.

%% Linear histogram: [{mult, Num}, {min_y, Num}, {max_y, Num}, {count, N}].
add_linear(Tab, Inst, Os) ->
    Mul = get_value(mult, Os, 1),
    Min = get_value(min_y, Os),
    Max = get_value(max_y, Os),
    Cnt = get_value(count, Os),
    mnesia:dirty_write(#density_conf{key = {Tab, Inst}, scale = lin, slope = Mul, min = Min, max = Max, cnt = Cnt}).

%% Logarithmic histogram: [{base, Num}, {min_exp, Num}, {max_exp, Num}, {count, N}].
add_logarithmic(Tab, Inst, Os) ->
    Log = get_value(base, Os, 10),
    Min = get_value(min_exp, Os),
    Max = get_value(max_exp, Os),
    Cnt = get_value(count, Os),
    mnesia:dirty_write(#density_conf{key = {Tab, Inst}, scale = log, slope = Log, min = Min, max = Max, cnt = Cnt}).

%% XXXtodo Want a safe way to ensure all stats config from *.app is loaded.
%%
%% Ensure all stats mentioned in *.app files of loaded applications are provisioned in Mnesia.
%% scan_all_stats() ->
%%     [scan_app_stats(App) || {App, _, _} <- application:loaded_applications()],
%%     ok.
%% 
%% scan_app_stats(App) ->
%%     [edit_hst(Name, Opts) || {Name, Opts} <- get_value(density, get_key(App, system_mon, []), [])],
%%     ok.
%% 
%% get_key(App, Key, Def) ->
%%     case application:get_key(App, Key) of
%% 	{ok, Val} ->
%% 	    Val;
%% 	undefined ->
%% 	    Def
%%     end.
%% 
%% edit_hst(Tab, Inst, Scale, Slope, Min, Max, Cnt, Do_force) ->
%%     case mnesia:dirty_read(density_conf, {Tab, Inst}) of
%% 	[#density_conf{}] when Do_force == false ->
%% 	    %% Just ensuring existence.
%% 	    ok;
%% 	[#density_conf{scale = Scale, slope = Slope, min = Min, max = Max, cnt = Cnt}] ->
%% 	    %% No change.
%% 	    ok;
%% 	[#density_conf{}] when Do_force == true ->
%% 	    %% XXX remove 
%% 	    %% XXX destroy existing instances.
%% 	    mnesia:write(#density_conf{});
%% 		  [] ->
%% 
