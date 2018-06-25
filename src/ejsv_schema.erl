-module(ejsv_schema).
-include("ejsv.hrl").

-export([ compile/2 ]).

compile(JsonMap, Opts) when is_map(JsonMap) ->
  SchemaType = maps:get(schema, Opts, json_schema),
  SchemaVersion = maps:get(version, Opts, {3,0}),
  % TODO JsonPathed = ejsv_utils:add_path(JsonMap, "/"),
  % probably in ejsv_jobs:process/2
  SchemaTag = {SchemaType, SchemaVersion},
  Rules = ejsv_rules:for_schema(SchemaTag),
  ReduceRule = fun(Rule) -> reduce_rules(SchemaTag, JsonMap, Rule) end,
  SchemaRules = lists:concat(lists:map(ReduceRule, Rules)),
  {ok, #schema{ type = SchemaType,
                version = SchemaVersion,
                rules = SchemaRules }}.

reduce_rules(Version, Schema, Rule) ->
  RuleS = ejsv_rules:rule(Version, Rule, Schema),
  lists:flatten([RuleS]).
