-module(ejsv_schema_tests).
-include_lib("eunit/include/eunit.hrl").
-include("ejsv.hrl").
-import(ejsv_testhelper, [ match_funs/2 ]).
-compile(export_all).

-define(mod, ejsv_schema).

meck_test_() ->
  {setup,
   fun() ->
       ok = meck:new(ejsv_keywords),
       ok = meck:new(ejsv_assertions, [non_strict])
   end,
   fun(_) -> meck:unload() end,
   match_funs(?MODULE, "^test_")}.

test_compile() ->
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

test_assert() ->
  Keyword = {existing_atom, #{}},
  Schema =  #schema{ keywords = [Keyword] },
  Valid = #{ <<"library">> => <<"eunit">> },
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
