-module(ejsv_json_schema_v3_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-import(ejsv_testhelper, [ run_schema_tests/2 ]).

-define(mod, ejsv).

all() -> ejsv_testhelper:all(?MODULE).

init_per_suite(Config) ->
  [ {base_schema, "http://json-schema.org/draft-03/schema#"},
    {schema, json_schema},
    {version, {3,0}} | Config ].

init_per_testcase(_Case, Config) ->
  ok = ejsv_cache:install(),
  Config.

enum_test(Config) ->
  run_schema_tests("enum", Config).

items_test(Config) ->
  run_schema_tests("items", Config).

additionalItems_test(Config) ->
  run_schema_tests("additionalItems", Config).

properties_test(Config) ->
  run_schema_tests("properties", Config).

% TODO just why?
% extends_test(Config) ->
%   run_schema_tests("extends", Config).

additionalProperties_test(Config) ->
  run_schema_tests("additionalProperties", Config).

patternProperties_test(Config) ->
  run_schema_tests("patternProperties", Config).

type_test(Config) ->
  run_schema_tests("type", Config).

pattern_test(Config) ->
  run_schema_tests("pattern", Config).

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

divisibleBy_test(Config) ->
  run_schema_tests("divisibleBy", Config).

default_test(Config) ->
  run_schema_tests("default", Config).

% dependencies_test(Config) ->
%   run_schema_tests("dependencies", Config).

% ref_test(Config) ->
%   run_schema_tests("ref", Config).

% refRemote_test(Config) ->
%   run_schema_tests("refRemote", Config).
