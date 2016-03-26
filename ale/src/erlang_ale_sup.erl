-module(erlang_ale_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(WORKER(I, Args), {I, {I, start_link, Args}, permanent, 5000, worker, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [ ?WORKER(gen_event, [{local, lagrange_events}]),
              ?WORKER(lagrange, []),
              ?WORKER(wheels, []),
              ?WORKER(wheels_poll, [])
            ],
    % up to 50 restarts in 5 seconds
    {ok, {{one_for_one, 50, 5}, Procs}}.
