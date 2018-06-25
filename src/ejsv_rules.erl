-module(ejsv_rules).

-export([ for_schema/1, rule/3 ]).

% XXX try do as much work as possble in rules
% this only gets done at compile time

for_schema({json_schema, {3,_}}) ->
  [
   prop_schemas,
   add_props,
   add_prop_schemas,
   pattern_prop_schemas,
   item_schemas,
   add_items,
   type,
   required,
   min_items,
   max_items,
   min_length,
   max_length,
   unique_items,
   maximum,
   minimum
  ];
for_schema({json_schema, {4,_}}) ->
  [
   min_items,
   max_items,
   min_length,
   max_length,
   unique_items,
   maximum,
   minimum
  ];
for_schema({open_api, _}) ->
  [
   properties
  ].

rule(_, item_schemas, #{ <<"items">> := [] }) ->
  [];
rule(Version, item_schemas, #{ <<"items">> := Items }) ->
  {items, #{ schema => compile(Items, Version) }};

%% XXX example of dynamic rules
rule(_, add_items, #{ <<"additionalItems">> := true }) ->
  [];
rule(Version, add_items, #{ <<"additionalItems">> := Adds } = Schema) ->
  case maps:get(<<"items">>, Schema, #{}) of
    Items when is_map(Items) -> [];
    Items when is_list(Items) ->
      {add_items, #{ item_count => length(Items),
                     schema => compile(Adds, Version) }}
  end;

rule(Version, prop_schemas, #{ <<"properties">> := Props }) ->
  true = is_map(Props),
  PropSchemas = maps:map(fun(_Key, Prop) -> compile(Prop, Version) end, Props),
  {properties, #{ properties => PropSchemas }};

rule(_, add_props, #{ <<"additionalProperties">> := Allowed } = Schema) when is_boolean(Allowed) ->
  Patterns = maps:keys(maps:get(<<"patternProperties">>, Schema, #{})),
  Props = maps:keys(maps:get(<<"properties">>, Schema, #{})),
  {add_props, #{ properties => Props,
                 patterns => Patterns,
                 allowed => Allowed }};

rule(Version, add_prop_schemas, #{ <<"additionalProperties">> := Adds } = Schema) when is_map(Adds) ->
  AddSchema = compile(Adds, Version),
  Patterns = maps:keys(maps:get(<<"patternProperties">>, Schema, #{})),
  Props = maps:keys(maps:get(<<"properties">>, Schema, #{})),
  {adds_schema, #{ properties => Props,
                   patterns => Patterns,
                   schema => AddSchema }};

rule(Version, pattern_prop_schemas, #{ <<"patternProperties">> := Patterns }) ->
  true = is_map(Patterns),
  PatternSchemas = maps:map(fun(_Key, Prop) -> compile(Prop, Version) end, Patterns),
  {patterns_schema, #{ patterns => PatternSchemas }};

rule(Version, type, #{ <<"type">> := Types }) when is_list(Types) ->
  {one_of_type, #{ types => compile(Types, Version) }};
rule(_, type, #{ <<"type">> := Type })  ->
  {of_type, #{ type => Type }};

rule(_, required, #{ <<"properties">> := Props }) ->
  Pred = fun(_K,V) -> maps:get(<<"required">>, V, false) end,
  Required = maps:keys(maps:filter(Pred,Props)),
  {required, #{ required => Required }};

rule(_, min_items, #{ <<"minItems">> := Min }) ->
  {min_items, #{ min => Min }};

rule(_, max_items, #{ <<"maxItems">> := Max }) ->
  {max_items, #{ max => Max }};

rule(_, min_length, #{ <<"minLength">> := Min }) ->
  {min_length, #{ min => Min }};

rule(_, max_length, #{ <<"maxLength">> := Max }) ->
  {max_length, #{ max => Max }};

rule(_, unique_items, #{ <<"uniqueItems">> := true }) ->
  {unique_items, #{}};

rule(_, maximum, #{ <<"maximum">> := Max } = Schema)  ->
  Exclusive = maps:get(<<"exclusiveMaximum">>, Schema, false),
  {maximum, #{ maximum => Max, exclusive => Exclusive }};

rule(_, minimum, #{ <<"minimum">> := Min } = Schema)  ->
  Exclusive = maps:get(<<"exclusiveMinimum">>, Schema, false),
  {minimum, #{ minimum => Min, exclusive => Exclusive }};

rule(_Ver, _Rule, _Schema) ->
  [].

%% --

compile(Json, {Type, Version}) when is_map(Json) ->
  Opts = #{ schema => Type, version => Version },
  {ok, Schema} = ejsv_schema:compile(Json, Opts),
  Schema;
compile(List, Version) when is_list(List) ->
  lists:map(fun(Item) -> compile(Item, Version) end, List);
compile(Other, _Version) ->
  Other.
