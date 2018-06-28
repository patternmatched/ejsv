-module(ejsv_transform_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-define(mod, ejsv_transform).

all() -> ejsv_testhelper:all(?MODULE).

value_test(Config) ->
  Version = {json_schema, {3,0}},
  run_test(Version, Config, "boolean", true),
  run_test(Version, Config, "date", {2017,1,1}),
  run_test(Version, Config, "dateTime", {{2017,1,1},{10,10,11}}),
  run_test(Version, Config, "integer", 12).

object_test(Config) ->
  Version = {json_schema, {3,0}},
  run_test(Version, Config, "basicObject",
           #{ "bool" => true,
              "string" => "value",
              "date" => {2017,1,1},
              "datetime" => {{2017,1,1},{10,10,11}},
              "integer" => 12 }).

list_test(Config) ->
  Version = {json_schema, {3,0}},
  run_test(Version, Config, "tuple",
           [ true,
             "value",
             {2017,1,1},
             {{2017,1,1},{10,10,11}},
             12,
             false ]),
  run_test(Version, Config, "list",
           [ "value",
             "2017-01-01",
             "2017-01-01T10:10:11" ]).

% TODO default_test(_Config) ->

run_test(Version, Config, TestFilename, Expected) ->
  DataDir = proplists:get_value(data_dir, Config),
  TestFile = filename:join([DataDir,TestFilename++".json"]),
  {ok,Bin} = file:read_file(TestFile),
  [Schema, Test] = jiffy:decode(Bin, [return_maps]),
  TransformFun = ?mod:for_schema(Version, Schema),
  ?assertEqual(Expected, TransformFun(Test)).
