-module(ejsv_transform_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-import(ejsv_testhelper, [ fixture/1 ]).

-define(mod, ejsv_transform).

value_test() ->
  Version = {json_schema, {3,0}},
  run_test(Version, "boolean", true),
  run_test(Version, "date", {2017,1,1}),
  run_test(Version, "dateTime", {{2017,1,1},{10,10,11}}),
  run_test(Version, "integer", 12).

object_test() ->
  Version = {json_schema, {3,0}},
  run_test(Version, "basicObject",
           #{ "bool" => true,
              "string" => "value",
              "date" => {2017,1,1},
              "datetime" => {{2017,1,1},{10,10,11}},
              "integer" => 12 }).

list_test() ->
  Version = {json_schema, {3,0}},
  run_test(Version, "tuple",
           [ true,
             "value",
             {2017,1,1},
             {{2017,1,1},{10,10,11}},
             12,
             false ]),
  run_test(Version, "list",
           [ "value",
             "2017-01-01",
             "2017-01-01T10:10:11" ]).

% TODO default_test(_Config) ->

run_test(Version, TestFilename, Expected) ->
  TestFile = fixture("transforms/"++TestFilename++".json"),
  {ok,Bin} = file:read_file(TestFile),
  [Schema, Test] = jiffy:decode(Bin, [return_maps]),
  TransformFun = ?mod:for_schema(Version, Schema),
  ?assertEqual(Expected, TransformFun(Test)).
