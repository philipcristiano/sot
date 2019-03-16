-module(sot_ra_kv).
-compile({parse_transform, lager_transform}).

-behaviour(ra_machine).
-export([init/1, apply/3, start/0]).
-export([write/2, read/1]).

% -opaque state() :: #{term() => term()}.
%
% -type ra_kv_command() :: {write, Key :: term(), Value :: term()} |
%                          {read, Key :: term()}.

-record(ra_kv_record, {k, v}).
-record(state, {name}).

init(Config) ->
  Name = maps:get(name, Config),
  Filename = filename:join(["tmp", "dets", erlang:atom_to_list(Name)]),
  ok = filelib:ensure_dir(Filename),
  {ok, _Name} = dets:open_file(Name, [{type, set},
                                        {keypos, #ra_kv_record.k},
                                        {file, Filename}]),

  lager:info("Config ~p", [Config]),
  #state{name=Name}.

apply(_Meta, {write, Key, Value}, State=#state{name=Name}) ->
    R = #ra_kv_record{k=Key, v=Value},
    ok = dets:insert(Name, R),
    ok = dets:sync(Name),
    {State, ok, []};
apply(_Meta, {read, Key}, State=#state{name=Name}) ->
    Objects = dets:lookup(Name, Key),
    {State, Objects, []}.

write(Key, Value) ->
    %% it would make sense to cache this to avoid redirection costs when this
    %% server happens not to be the current leader
    Server = ra_kv1,
    case ra:process_command(Server, {write, Key, Value}) of
        {ok, _, _} ->
            ok;
        Err ->
            Err
    end.

read(Key) ->
    Server = ra_kv1,
    case ra:process_command(Server, {read, Key}) of
        {ok, Value, _} ->
            {ok, Value};
        Err ->
            Err
    end.

start() ->
  %% the initial cluster members
  Members = [{ra_kv1, node()}, {ra_kv2, node()}, {ra_kv3, node()}],
  %% an arbitrary cluster name
  ClusterName = <<"ra_kv">>,
  %% the config passed to `init/1`, must be a `map`
  Config = #{},
  %% the machine configuration
  Machine = {module, ?MODULE, Config},
  %% ensure ra is started
  application:ensure_all_started(ra),
  %% start a cluster instance running the `ra_kv` machine
  ra:start_cluster(ClusterName, Machine, Members).
