-module(lagrange).
-behaviour(gen_server).

-export([start_link/0, go/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%% api

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

go() ->
    gen_server:call(?SERVER, go).

%% gen_server

init([]) ->
    gen_event:add_sup_handler(lagrange_events, {proc_event_bridge, self()}, [self()]),
    io:format("lagrange ready~n", []),
    {ok, #state{}}.

handle_call(hello, _From, State) ->
    gen_event:notify(lagrange_events, {self(), hello}),
    {reply, hello, State};
handle_call(go, _From, State) ->
    wheels:motor({forward, 100}, {forward, 100}),
    {reply, ok, State};
handle_call(_Request, _From, State) -> {reply, meh, State}.


handle_info({_, hello}, State) ->
    io:format("lagrange got hello! ~n", []),
    {noreply, State};
handle_info({encoder, S1, S2}, State) ->
    case S1 > 60 andalso S2 > 60 of
        true -> wheels:stop();
        false -> keep_calm_carry_on
    end,
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% internal
