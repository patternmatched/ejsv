-module(ejsv_json_schema_v4_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-import(ejsv_testhelper, [ run_schema_tests/2 ]).

-define(mod, ejsv).

all() -> ejsv_testhelper:all(?MODULE).

init_per_suite(Config) ->
  [ {base_schema, "http://json-schema.org/draft-04/schema#"},
    {schema, json_schema},
    {version, {4,0}} | Config ].

init_per_testcase(_Case, Config) ->
  ok = ejsv_cache:install(),
  Config.

% minItems_test(Config) ->
%   run_schema_tests("minItems", Config).

% maxItems_test(Config) ->
%   run_schema_tests("maxItems", Config).

% minLength_test(Config) ->
%   run_schema_tests("minLength", Config).

% maxLength_test(Config) ->
%   run_schema_tests("maxLength", Config).

% uniqueItems_test(Config) ->
%   run_schema_tests("uniqueItems", Config).

% maximum_test(Config) ->
%   run_schema_tests("maximum", Config).

minimum_test(Config) ->
  run_schema_tests("minimum", Config).
