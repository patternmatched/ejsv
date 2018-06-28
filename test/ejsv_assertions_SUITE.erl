-module(ejsv_assertions_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-define(mod, ejsv_assertions).

all() -> ejsv_testhelper:all(?MODULE).

maximum_test(_Config) ->
  Opts = #{ maximum => 3, exclusive => false },
  ?assert(?mod:maximum(1,Opts)),
  ?assert(?mod:maximum(3,Opts)),
  ?assertNot(?mod:maximum(3,Opts#{ exclusive := true })),
  ?assertNot(?mod:maximum(4,Opts)).
