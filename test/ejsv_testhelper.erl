-module(ejsv_testhelper).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

fixture(FilePath) ->
  {ok, Cwd} = file:get_cwd(),
  Cwd ++ "/test/fixtures/" ++ FilePath.

%% --

run_schema_tests(Opts, Keywords) ->
  {setup,
   fun () -> ok = ejsv_cache:install() end,
   fun (_) -> ok = ejsv_cache:delete() end,
   fun (_) ->
       lists:map(fun(Keyword) ->
                     run_schema_suites(Keyword, Opts)
                 end, Keywords)
   end}.

run_schema_suites(Keyword, Opts) ->
  TestDir = maps:get(test_dir, Opts),
  SuiteFile = fixture(TestDir ++ atom_to_list(Keyword) ++ ".json"),
  {ok,Bin} = file:read_file(SuiteFile),
  Suites = jiffy:decode(Bin, [return_maps]),
  {
   atom_to_list(Keyword),
   lists:map(fun(Suite) -> run_schema_suite(Suite, Opts) end, Suites)
  }.

run_schema_suite(Suite, Opts) ->
  SchemaOpts = maps:with([schema, version], Opts),
  #{ <<"description">> := Description,
     <<"schema">> := Json,
     <<"tests">> := Tests } = Suite,
  {ok, Schema} = ejsv_schema:compile(Json, SchemaOpts),
  {ok, _} = ejsv_cache:set_schema(Description, Schema),
  {
   Description,
   lists:map(fun(Test) -> run_schema_test(Test, Description, Schema) end, Tests)
  }.

run_schema_test(Test, Ref, Schema) ->
  #{ <<"description">> := Description,
     <<"data">> := Data,
     <<"valid">> := Valid } = Test,
  {Description,
   ?_test(begin
            io:format("Data: ~p~nSchema: ~p", [Data, Schema]),
            case Valid of
              true -> ?assertMatch(true, ejsv:validate(Ref, Data));
              false -> ?assertMatch({false, _}, ejsv:validate(Ref, Data))
            end
          end)}.

%% --

match_funs(Mod, Pattern) ->
  lists:filtermap(
    fun({Fun, Arity}) ->
        case re:run(atom_to_list(Fun), Pattern) of
          _ when Arity =/= 0 -> false;
          {match,_} -> {true, fun Mod:Fun/0};
          nomatch -> false
        end
    end, Mod:module_info(exports)).
