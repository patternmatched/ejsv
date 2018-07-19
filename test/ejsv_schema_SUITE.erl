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
  meck:expect(ejsv_keywords, for_schema, 1, [rule]),
  Check = {check, #{}},
  Type = json_schema,
  Version = {3,0},
  meck:expect(ejsv_keywords, define, 3, [Check]),
  Schema = #{ <<"maximum">> => 3 },
  Opts = #{ schema => Type, version => Version },
  ?assertMatch({ok, #schema{ keywords = [Check],
                             type = Type,
                             version = Version }},
               ?mod:compile(Schema, Opts)).

assert_test(_Config) ->
  Keyword = {existing_atom, #{}},
  Schema =  #schema{ keywords = [Keyword] },
  Valid = #{ <<"library">> => <<"eunit">> },
  meck:new(ejsv_assertions, [non_strict]),
  meck:expect(ejsv_assertions, existing_atom, 2, true),
  ?assertEqual(true, ?mod:assert(Valid, Schema)),
  Invalid = #{ <<"library">> => <<"unkown_atom">> },
  ErrorMsg = "atom does not exist",
  Error = #{keyword => existing_atom,
            message => ErrorMsg,
            props => #{},
            value => Invalid},
  meck:expect(ejsv_assertions, existing_atom, 2, {false,ErrorMsg}),
  ?assertEqual({false, [Error]}, ?mod:assert(Invalid, Schema)).
