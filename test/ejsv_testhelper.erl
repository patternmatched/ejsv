-module(ejsv_testhelper).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

run_schema_tests(Rule, Config) ->
  DataDir = proplists:get_value(data_dir, Config),
  TestFile = filename:join([DataDir,Rule++".json"]),
  {ok,Bin} = file:read_file(TestFile),
  Tests = jiffy:decode(Bin, [return_maps]),
  lists:foreach(fun(Test) -> run_schema_test(Test, Config) end, Tests).

run_schema_test(TestSuite, Config) ->
  _DefaultSchema = proplists:get_value(base_schema, Config),
  SchemaType = proplists:get_value(schema, Config),
  SchemaVersion = proplists:get_value(version, Config),
  SchemaOpts = #{ schema => SchemaType, version => SchemaVersion },
  #{ <<"description">> := Description,
     <<"schema">> := Json,
     <<"tests">> := Tests } = TestSuite,
  ct:pal("SUITE: ~s~nSCHEMA: ~p", [Description, Json]),
  {ok, Schema} = ejsv_schema:compile(Json, SchemaOpts),
  ct:pal("COMPILED: ~p~n", [Schema]),
  {ok, _} = ejsv_cache:set_schema(Description, Schema),
  lists:foreach(fun(Test) -> test_validation(Test, Description) end, Tests).

test_validation(TestCase, Ref) ->
  #{ <<"description">> := Description,
     <<"data">> := Data,
     <<"valid">> := Valid } = TestCase,
  ct:pal("CASE: ~s~nDATA: ~p~nVALID: ~p", [Description, Data, Valid]),
  case Valid of
    true -> ?assertMatch(true, ejsv:validate(Ref, Data));
    false -> ?assertMatch({false, _}, ejsv:validate(Ref, Data))
  end.

%% --

all(Mod) ->
  lists:filtermap(fun is_test/1, Mod:module_info(exports)).

is_test({Fun, Arity}) when is_atom(Fun) ->
  is_test({Fun, lists:reverse(atom_to_list(Fun)), Arity});
is_test({Fun, "tset_" ++ _, 1}) ->
  {true, Fun};
is_test(_) ->
  false.

