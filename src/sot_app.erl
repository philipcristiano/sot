-module(sot_app).
-compile({parse_transform, lager_transform}).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  Status = sot_ra_kv:start(),
  lager:info("Cluster status ~p", [Status]),
  sot_sup:start_link().

stop(_State) ->
  ok.
