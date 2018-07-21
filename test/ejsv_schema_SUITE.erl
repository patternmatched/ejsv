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

error_format_test(Config) ->
  DataDir = proplists:get_value(data_dir, Config),
  SchemaFile = filename:join([DataDir,"complex.json"]),
  Opts = #{ schema => {json_schema, {7,0}} },
  {ok, SchemaJson} = ejsv_utils:json_file(SchemaFile),
  {ok, Schema} = ejsv_schema:compile(SchemaJson, Opts),
  Test = fun(Errors, Data) ->
             Result = ejsv_schema:assert(Data, Schema),
             ?assertEqual({false, Errors}, Result)
         end,
  % Test([#{ schemaPath => "#/properties/name",
  %          dataPath => "#/name",
  %          message => "property type not valid" }],
  %      #{ <<"name">> => 10 }).
  Test([#{causes => [#{keyword => "type",
                       message => "items failed validation",
                       params => #{type => <<"string">>},
                       value => 10}],
          keyword => "properties"}],
       #{ <<"name">> => 10 }).

% TODO in principle we json transcodable errors
% error_transcodable_test(Config) ->
%   ErrorsJson = #{},
%   {false, Errors} = ejsv_schema:assert(Data, Schema),
%   ErrorsJsonified = jiffy:decode(jiffy:encode(Errors), [return_maps]),
%   ?assertEqual(ErrorsJson, ErrorsJsonified).
%   ok.
