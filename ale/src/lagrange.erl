-module(lagrange).
-behaviour(gen_server).

-export([start_link/0, go/0, listen/0, listen_loop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {encoder_base, encoder_target, encoder_rate}).

%% api

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

go() ->
    gen_server:call(?SERVER, go).

listen() ->
    spawn_link(fun() ->
                  gen_event:add_sup_handler(lagrange_events, {proc_event_bridge, self()}, [self()]),
                  ?MODULE:listen_loop()
               end).

%% gen_server

init([]) ->
    gen_event:add_sup_handler(lagrange_events, {proc_event_bridge, self()}, [self()]),
    io:format("lagrange ready~n", []),
    {ok, #state{encoder_base=enil(), encoder_target=enil(), encoder_rate=rnil()}}.

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
handle_info(E = {encoder, S1, S2}, State = #state{encoder_target={encoder, T1, T2},
                                                  encoder_rate={rate, R1, R2}}) ->
    NRate = {rate, S1 - R1, S2 - R2},
    case S1 >= T1 andalso S2 >= T2 of
        true ->
            wheels:stop(),
            {noreply, State#state{encoder_base=E, encoder_rate=NRate}};
        false ->
            {noreply, State#state{encoder_rate=NRate}}
    end;
handle_info(_Info, State) -> {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% internal

enil() -> {encoder, 0, 0}.
rnil() -> {encoder, 0, 0}.

listen_loop() ->
    receive
        Event -> io:format("lagrange_events: ~p~n", [Event])
    end,
    ?MODULE:listen_loop().
