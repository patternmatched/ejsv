-module(ejsv_json_schema_v3_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-import(ejsv_testhelper, [ run_schema_tests/2 ]).

-define(mod, ejsv).

setup_test_() ->
  run_schema_tests(#{ test_dir => "json_schema_v3/",
                      base_schema => "http://json-schema.org/draft-03/schema#",
                      schema => json_schema,
                      version => {3,0} }, keywords()).

keywords() ->
  [
   enum,
   items,
   additionalItems,
   properties,
   additionalProperties,
   patternProperties,
   type,
   pattern,
   required,
   minItems,
   maxItems,
   minLength,
   maxLength,
   uniqueItems,
   maximum,
   minimum,
   divisibleBy,
   default
   % TODO
   % dependencies,
   % ref,
   % refRemote,
   % XXX just why?
   % extends,
  ].

