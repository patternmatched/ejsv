-module(ejsv_json_schema_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-import(ejsv_testhelper, [ run_schema_tests/2 ]).

-define(mod, ejsv).

all() -> ejsv_testhelper:all(?MODULE).

init_per_suite(Config) ->
  [ {schema, json_schema},
    {version, {3,0}},
    {base_schema, "http://json-schema.org/draft-03/schema#"},
    {test_dir, "tests/draft3"} | Config ].

init_per_testcase(_Case, Config) ->
  ok = ejsv_cache:install(),
  Config.

items_test(Config) ->
  run_schema_tests("items", Config).

additionalItems_test(Config) ->
  run_schema_tests("additionalItems", Config).

properties_test(Config) ->
  run_schema_tests("properties", Config).

additionalProperties_test(Config) ->
  run_schema_tests("additionalProperties", Config).

patternProperties_test(Config) ->
  run_schema_tests("patternProperties", Config).

type_test(Config) ->
  run_schema_tests("type", Config).

required_test(Config) ->
  run_schema_tests("required", Config).

minItems_test(Config) ->
  run_schema_tests("minItems", Config).

maxItems_test(Config) ->
  run_schema_tests("maxItems", Config).

minLength_test(Config) ->
  run_schema_tests("minLength", Config).

maxLength_test(Config) ->
  run_schema_tests("maxLength", Config).

uniqueItems_test(Config) ->
  run_schema_tests("uniqueItems", Config).

maximum_test(Config) ->
  run_schema_tests("maximum", Config).

minimum_test(Config) ->
  run_schema_tests("minimum", Config).
