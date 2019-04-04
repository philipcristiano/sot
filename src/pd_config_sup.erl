%%%-------------------------------------------------------------------
%%% @author $AUTHOR
%%% @copyright 2019 $OWNER
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(pd_config_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  FileBase = application:get_env(pd_config, path, "pd_config.dets"),

  Procs = [
        #{id    => pd_config,
          start => {pd_config, start_link, [FileBase]}
        }
    ],
  {ok, {{one_for_one, 1, 5}, Procs}}.
