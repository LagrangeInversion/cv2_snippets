-module(lagrange).
-behaviour(gen_server).

-export([start_link/0, go/1, left/1, notify/1, listen/0, listen_loop/0]).
-export([pidcontrol/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {encoder_base, encoder_target, encoder_state, motion, pid}).

%% api

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

go(Holes) ->
    gen_server:call(?SERVER, {go, Holes}).

left(Degrees) ->
    gen_server:call(?SERVER, {left, Degrees}).

notify(N) ->
    gen_event:notify(lagrange_events, N).

listen() ->
    spawn_link(fun() ->
                  gen_event:add_sup_handler(lagrange_events, {proc_event_bridge, self()}, [self()]),
                  ?MODULE:listen_loop()
               end).

%% gen_server

init([]) ->
    gen_event:add_sup_handler(lagrange_events, {proc_event_bridge, self()}, [self()]),
    io:format("lagrange ready~n", []),
    {ok, #state{encoder_base=enil(),
                encoder_state=enil(),
                encoder_target=enil(),
                pid={0,0,0}}}.

handle_call(hello, _From, State) ->
    lagrange:notify(hello),
    {reply, hello, State};
handle_call({go, Holes}, _From, State) ->
    Motion = [{forward, 100}, {forward, 100}],
    apply(wheels, motor, Motion),
    {reply, ok, State#state{encoder_base=State#state.encoder_state,
                            encoder_target=bump_target(State#state.encoder_state,
                                                       Holes),
                            pid={0,0,0},
                            motion=Motion}};
handle_call({left, Degrees}, _From, State) ->
    Motion = [{forward, 100}, {forward, 0}],
    apply(wheels, motor, Motion),
    Base = State#state.encoder_state,
    {encoder, S1, S2} = Base,
    {reply, ok, State#state{encoder_base=Base,
                            encoder_target={encoder,
                                            S1,
                                            S2 + trunc(Degrees / (360 / 64))},
                            pid={0,0,0},
                            motion=Motion}};
handle_call(_Request, _From, State) -> {reply, meh, State}.

%% info

handle_info(hello, State) ->
    io:format("lagrange got hello! ~n", []),
    {noreply, State};

handle_info(E = {encoder, _, _}, State = #state{motion=false}) ->
    {noreply, State#state{encoder_state=E}};

handle_info(E = {encoder, S1, S2}, State = #state{encoder_base={encoder, B1, B2},
                                                  encoder_target={encoder, T1, T2},
                                                  motion=M}) ->

    NewE = {encoder, S1 - B1, S2 - B2},

    case S1 >= T1 andalso S2 >= T2 of
        true ->
            wheels:stop(),
            lagrange:notify({reached, {encoder, T1, T2}, E}),
            {noreply, State#state{encoder_base=E,
                                  encoder_state=E,
                                  pid={0,0,0},
                                  motion=false}};
        false ->
            {PID, NewMotion} = pidcontrol(State#state.pid, NewE, M),
            lagrange:notify({correction, PID, NewMotion, NewE}),
            apply(wheels, motor, NewMotion),

            {noreply, State#state{encoder_state=E,
                                  pid=PID}}
    end;

handle_info({reached, _}, State) -> {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

%%

handle_cast(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% internal

enil() -> {encoder, 0, 0}.
rnil() -> {rate, 0, 0}.
bump_target({encoder, E1, E2}, Value) ->
    {encoder, E1 + Value, E2 + Value}.

listen_loop() ->
    receive
        Event -> io:format("lagrange_events: ~p~n", [Event])
    end,
    ?MODULE:listen_loop().

% lagrange:listen(), lagrange:go(100).

pidcontrol({_, I, D}, {encoder, Left, Right}, [{Direction, LS}, {Direction, RS}]) ->
    Prop = Left - Right,
    Int = I + Prop,
    Diff = Prop - D,
    Correction = trunc(Prop + 0.5 * Int + Diff),
    {{Prop, Int, Diff}, [{Direction, LS - Correction}, {Direction, RS + Correction}]}.

millis() ->
    erlang:monotonic_time(milli_seconds).
