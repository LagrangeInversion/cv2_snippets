-module(wheels).
-behaviour(gen_server).

-export([start_link/0, voltage/0, motor/2, stop/0, encoder/0, test/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(GOPIGO, 16#08).
-gopigo_api_version(16).
-record(state, {io}).

%% api

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

voltage() ->
    gen_server:call(?SERVER, voltage).

motor(L, R) ->
    gen_server:call(?SERVER, {motor, L, R}).

stop() ->
    gen_server:call(?SERVER, stop).

encoder() ->
    gen_server:call(?SERVER, encoder).

test() ->
    {ok, S} = ?MODULE:start_link(),
    R = ?MODULE:voltage(),
    ok = gen_server:stop(S),
    R.

%% callbacks

init([]) ->
    {ok, Io} = i2c:start_link("i2c-1", ?GOPIGO),
    {ok, #state{io=Io}}.

handle_call(voltage, _From, State = #state{io=Io}) ->
    ok = (i2c_fire(118))(Io),
    ok = i2c:smbus_write_block(Io, 1, <<118, 0, 0, 0>>), % read voltage
    receive after 100 -> ok end,
    B1 = i2c:smbus_read_byte(Io),
    B2 = i2c:smbus_read_byte(Io),
    <<Volts:16>> = <<B1, B2>>,
    Volts1 = (5*Volts/1024)/0.4,
    {reply, {voltage, Volts1}, State};

handle_call({motor, {Left, LDir}, {Right, RDir}}, _From, State = #state{io=Io})
  when Left >= 0,
       Left =< 255,
       Right >= 0,
       Right =< 255,
       (LDir =:= forward orelse LDir =:= backward),
       (RDir =:= forward orelse RDir =:= backward) ->
    ok = (i2c_fire(111, Left, direction(LDir)))(Io),
    ok = (i2c_fire(112, Right, direction(RDir)))(Io),
    {reply, ok, State};

handle_call(stop, _From, State = #state{io=Io}) ->
    ok = (i2c_fire(120))(Io),
    {reply, ok, State};

handle_call(encoder, _From, State = #state{io=Io}) ->
    ok = (i2c_fire(53))(Io),
    S1 = i2c:smbus_read_byte(Io),
    S2 = i2c:smbus_read_byte(Io),
    {reply, {encoder, S1, S2}, State};

handle_call(_, _, State) -> {reply, {error, badapi}, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% internal

i2c_fire(Command) ->
    i2c_fire(Command, 0, 0, 0).
i2c_fire(Command, Arg1) ->
    i2c_fire(Command, Arg1, 0, 0).
i2c_fire(Command, Arg1, Arg2) ->
    i2c_fire(Command, Arg1, Arg2, 0).
i2c_fire(Command, Arg1, Arg2, Arg3) ->
    fun(Io) -> i2c:smbus_write_block(Io, 1, <<Command, Arg1, Arg2, Arg3>>) end.

direction(forward) -> 1;
direction(backward) -> 2.
