-module(ejsv_schema_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("ejsv.hrl").
-compile(export_all).

-define(mod, ejsv_schema).

all() -> ejsv_testhelper:all(?MODULE).

compile_test(_Config) ->
  meck:new(ejsv_rules),
  meck:expect(ejsv_rules, for_schema, 1, [rule]),
  Check = {check, #{}},
  Type = json_schema,
  Version = {3,0},
  meck:expect(ejsv_rules, rule, 3, [Check]),
  Schema = #{ <<"maximum">> => 3 },
  Opts = #{ schema => Type, version => Version },
  ?assertMatch({ok, #schema{ rules = [Check],
                             type = Type,
                             version = Version }},
               ?mod:compile(Schema, Opts)),
  meck:unload().
