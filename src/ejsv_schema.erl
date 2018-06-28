-module(ejsv_schema).
-include("ejsv.hrl").

-export([ compile/2 ]).

compile(JsonMap, Opts) when is_map(JsonMap) ->
  SchemaType = maps:get(schema, Opts, json_schema),
  SchemaVersion = maps:get(version, Opts, {3,0}),
  % TODO JsonPathed = ejsv_utils:add_path(JsonMap, "/"),
  % probably in ejsv_assertions:execute/2
  SchemaTag = {SchemaType, SchemaVersion},
  Keywords = ejsv_keywords:for_schema(SchemaTag),
  Transform = ejsv_transform:for_schema(SchemaTag, JsonMap),
  ReduceKeyword = fun(Keyword) -> reduce_keywords(SchemaTag, JsonMap, Keyword) end,
  SchemaKeywords = lists:concat(lists:map(ReduceKeyword, Keywords)),
  {ok, #schema{ type = SchemaType,
                version = SchemaVersion,
                transform = Transform,
                keywords = SchemaKeywords }}.

reduce_keywords(Version, Schema, Keyword) ->
  Keywords = ejsv_keywords:define(Version, Keyword, Schema),
  lists:flatten([Keywords]).
