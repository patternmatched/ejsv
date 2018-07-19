-module(ejsv_schema_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include("ejsv.hrl").
-compile(export_all).

-define(mod, ejsv_schema).

all() -> ejsv_testhelper:all(?MODULE).

end_per_testcase(_Config) ->
  meck:unload().

compile_test(_Config) ->
  meck:new(ejsv_keywords),
  meck:expect(ejsv_keywords, for_schema, 1, ["keyword"]),
  SchemaVersion = {json_schema, {3,0}},
  Keyword = #keyword{ name = "keyword",
                      function = assertion,
                      options = #{},
                      schema = SchemaVersion,
                      params = #{}},
  Definition = {Keyword#keyword.function, Keyword#keyword.params},
  meck:expect(ejsv_keywords, define, 3, [Definition]),
  Schema = #{ <<"maximum">> => 3 },
  Opts = #{ schema => SchemaVersion },
  ?assertMatch({ok, #schema{ keywords = [Keyword],
                             version = SchemaVersion }},
               ?mod:compile(Schema, Opts)).

assert_test(_Config) ->
  Keyword = #keyword{ name = "atom",
                      function = existing_atom,
                      options = #{ "opt" => "diff" },
                      params = #{ "param" => "diff" }},
  Schema =  #schema{ keywords = [Keyword] },
  Valid = #{ <<"library">> => <<"eunit">> },
  meck:new(ejsv_assertions, [non_strict]),
  meck:expect(ejsv_assertions, existing_atom, 2, true),
  ?assertEqual(true, ?mod:assert(Valid, Schema)),
  Invalid = #{ <<"library">> => <<"unkown_atom">> },
  ErrorMsg = "atom does not exist",
  Error = #{keyword => "atom",
            message => ErrorMsg,
            params => #{ "param" => "diff" },
            value => Invalid},
  meck:expect(ejsv_assertions, existing_atom, 2, {false,ErrorMsg}),
  ?assertEqual({false, [Error]}, ?mod:assert(Invalid, Schema)).
