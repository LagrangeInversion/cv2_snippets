-module(compass).
-behaviour(gen_server).
-reference('https://www.adafruit.com/datasheets/HMC5883L_3-Axis_Digital_Compass_IC.pdf').

-mode(compile).
%%-on_load(start/0).

%% API
-export([start_link/0, read/1, start/0, start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(MODULE_, ?MODULE).
-define(SERVER, ?MODULE_).
-define(SCALE, 0.92).

-record(state, {io}).

%%% API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE_, [], []).

read(S) ->
    gen_server:call(S, read).

start() ->
    {ok, S} = ?MODULE_:start_link(),
    start(S).

start(S) -> %% TODO: gen_event
    receive _ -> stopping
    after 250 -> io:format("~p~n", [?MODULE_:read(S)])
    end,
    start(S).

%%% gen_server callbacks

init([]) ->
    {ok, Io} = i2c:start_link("i2c-1", 16#1e),

    i2c:write(Io, <<16#00, 16#71>>), % 8 samples at 15hz, self-test
    i2c:write(Io, <<16#01, 16#a0>>), % gain=5
    i2c:write(Io, <<16#02, 16#00>>), % continuous sampling
    receive after 6 -> ok end,
    _ = i2c:write_read(Io, <<16#3>>, 6),
    receive after 67 -> ok end,
    <<X:16/signed, Z:16/signed, Y:16/signed>> = i2c:write_read(Io, <<16#3>>, 6),
    receive after 67 -> ok end,
    <<X1:16/signed, Z1:16/signed, Y1:16/signed>> = i2c:write_read(Io, <<16#3>>, 6),
    io:format("SELF TEST: ~p ~p~n", [{X, Z, Y}, {X1, Z1, Y1}]),

    i2c:write(Io, <<16#00, 16#70>>), % 8 samples at 15hz
    i2c:write(Io, <<16#01, 16#20>>), % 1.3 gain LSb / Gauss 1090 (default)
    i2c:write(Io, <<16#02, 16#00>>), % continuous sampling
    {ok, #state{io=Io}}.

handle_call(read, _From, State = #state{io=Io}) ->
    _Status = i2c:write_read(Io, <<16#09>>, 1),
    %{XY_Gain, Z_Gain} = {1090, 980},
    <<X:16/signed, Z:16/signed, Y:16/signed>> = i2c:write_read(Io, <<16#3>>, 6),

    %{X1, Y1, _Z1} = {X * 100 / XY_, Y * 100 / XY_Gain, Z * 100 / Z_Gain},

    Heading = to_degrees(math:atan2(Y, X)),

    {reply, {compass, {heading, Heading}, X, Y, Z}, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

-define(TAU, 6.283185307179586).
-define(DEGS_IN_RAD, 57.29577951308232).

to_degrees(Rad) when Rad < 0 -> to_degrees(Rad + ?TAU);
to_degrees(Rad) when Rad > ?TAU -> to_degrees(Rad - ?TAU);
to_degrees(Rad) -> trunc(Rad * ?DEGS_IN_RAD).
