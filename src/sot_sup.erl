-module(sot_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Procs = [
        #{id    => pd_config_sup,
          start => {pd_config_sup, start_link, []},
          type  => supervisor
        }
    ],
  {ok, {{one_for_one, 1, 5}, Procs}}.
