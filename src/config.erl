%%%-------------------------------------------------------------------
%%% File    : tibia_config.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 25 Nov 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(config).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0,
	 start/1, start_link/1]).

-export([get_value/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(config, {file = "config.hrl",
		 ip = {127,0,0,1},
		 sprite_size = 32}).



-define(SERVER, ?MODULE).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, "config.hrl", []).

start(File) ->
    gen_server:start({local, ?SERVER}, ?MODULE, File, []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, "config.hrl", []).

start_link(File) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, File, []).

init(File) ->
    {ok, _Config} = load(File).

handle_call(sprite_size, _From, State) ->
    {reply, State#config.sprite_size, State};
handle_call(ip, _From, State) ->
    {reply, State#config.ip, State};
handle_call(reload, _From, State) ->
    {Reply, State2} = load(State#config.file),
    {reply, Reply, State2};
handle_call(_Request, _From, State) ->
    {reply, undefined, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(reload, State) ->
    case load(State#config.file) of
	{ok, State2} ->
	    State2;
	{Error, State2} ->
	    io:format("~p\n", [Error]),
	    State2
    end,
    {noreply, State2};
handle_info(stop, State) ->
    {stop, shutdown, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

load(File) ->
    case file:consult(File) of
	{ok, Terms} ->
	    Config = set_config(Terms),
	    {ok, Config#config{file = File}};
	{error, Reason} ->
	    {{error, Reason}, #config{file = File}}
    end.


set_config(Terms) ->
    set_config(Terms, #config{}).

set_config([_|Terms], Conf) ->
    set_config(Terms, Conf);
set_config([], Conf) ->
    Conf.


get_value(Key) ->
    gen_server:call(?SERVER, Key).

