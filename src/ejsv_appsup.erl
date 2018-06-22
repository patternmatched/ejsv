-module(ejsv_appsup).
-behaviour(application).

-export([ start/2, stop/1 ]).

start(_Type, _StartArgs) ->
  ok = ejsv_cache:install().

stop(_State) ->
  ok.
