-module(ejsv_keywords).

-export([ for_schema/1, define/3 ]).

% XXX try do as much work as possble in here
% this only gets done at compile time

for_schema({json_schema, {3,_}}) ->
  for_schema({json_schema, {4,0}}) ++
  [
   "divisibleBy"
  ];
for_schema({json_schema, {4,_}}) ->
  [
   "enum",
   "properties",
   % TODO these dont match, need to refactor how errors are generated
   "add_props",
   "add_prop_schemas",
   "patternProperties",
   "items",
   "additionalItems",
   "type",
   "pattern",
   "required",
   "minItems",
   "maxItems",
   "minLength",
   "maxLength",
   "uniqueItems",
   "maximum",
   "minimum"
  ];
for_schema({json_schema, {7,_}}) ->
  for_schema({json_schema, {4,0}}) ++
  [
   "oneOf"
  ].

define(_, "enum", #{ <<"enum">> := Enums }) ->
  {enum, #{ match => Enums }};

define(_, "items", #{ <<"items">> := [] }) ->
  [];
define(Version, "items", #{ <<"items">> := Items }) ->
  {items, #{ schema => compile(Items, Version) }};

%% XXX example of dynamic keywords
define(_, "additionalItems", #{ <<"additionalItems">> := true }) ->
  [];
define(Version, "additionalItems", #{ <<"additionalItems">> := Adds } = Schema) ->
  case maps:get(<<"items">>, Schema, #{}) of
    Items when is_map(Items) -> [];
    Items when is_list(Items) ->
      {add_items, #{ item_count => length(Items),
                     schema => compile(Adds, Version) }}
  end;

define(Version, "properties", #{ <<"properties">> := Props }) ->
  true = is_map(Props),
  PropSchemas = maps:map(fun(_Key, Prop) -> compile(Prop, Version) end, Props),
  {properties, #{ properties => PropSchemas }};

define(_, "add_props", #{ <<"additionalProperties">> := Allowed } = Schema) when is_boolean(Allowed) ->
  Patterns = maps:keys(maps:get(<<"patternProperties">>, Schema, #{})),
  Props = maps:keys(maps:get(<<"properties">>, Schema, #{})),
  {add_props, #{ properties => Props,
                 patterns => Patterns,
                 allowed => Allowed }};

define(Version, "add_prop_schemas", #{ <<"additionalProperties">> := Adds } = Schema) when is_map(Adds) ->
  AddSchema = compile(Adds, Version),
  Patterns = maps:keys(maps:get(<<"patternProperties">>, Schema, #{})),
  Props = maps:keys(maps:get(<<"properties">>, Schema, #{})),
  {adds_schema, #{ properties => Props,
                   patterns => Patterns,
                   schema => AddSchema }};

define(Version, "patternProperties", #{ <<"patternProperties">> := Patterns }) ->
  true = is_map(Patterns),
  PatternSchemas = maps:map(fun(_Key, Prop) -> compile(Prop, Version) end, Patterns),
  {patterns_schema, #{ patterns => PatternSchemas }};

define(Version, "type", #{ <<"type">> := Types }) when is_list(Types) ->
  {one_of_type, #{ types => compile(Types, Version) }};
define(_, "type", #{ <<"type">> := Type })  ->
  {of_type, #{ type => Type }};

define(_, "pattern", #{ <<"pattern">> := Pattern }) ->
  {pattern, #{ regex => Pattern }};

define({json_schema, {3,_}}, "required", #{ <<"properties">> := Props }) ->
  Pred = fun(_K,V) -> maps:get(<<"required">>, V, false) end,
  Required = maps:keys(maps:filter(Pred,Props)),
  {required, #{ required => Required }};

define(_, "required", #{ <<"required">> := Required }) ->
  {required, #{ required => Required }};

define(_, "minItems", #{ <<"minItems">> := Min }) ->
  {min_items, #{ min => Min }};

define(_, "maxItems", #{ <<"maxItems">> := Max }) ->
  {max_items, #{ max => Max }};

define(_, "minLength", #{ <<"minLength">> := Min }) ->
  {min_length, #{ min => Min }};

define(_, "maxLength", #{ <<"maxLength">> := Max }) ->
  {max_length, #{ max => Max }};

define(_, "uniqueItems", #{ <<"uniqueItems">> := true }) ->
  {unique_items, #{}};

define(_, "maximum", #{ <<"maximum">> := Max } = Schema)  ->
  Exclusive = maps:get(<<"exclusiveMaximum">>, Schema, false),
  {maximum, #{ maximum => Max, exclusive => Exclusive }};

define(_, "minimum", #{ <<"minimum">> := Min } = Schema)  ->
  Exclusive = maps:get(<<"exclusiveMinimum">>, Schema, false),
  {minimum, #{ minimum => Min, exclusive => Exclusive }};

define(_, "divisibleBy", #{ <<"divisibleBy">> := Factor })  ->
  {div_by, #{ factor => Factor }};

define(Version, "oneOf", #{ <<"oneOf">> := Schemas })  ->
  {one_of, #{ schemas => compile(Schemas, Version) }};

define(_Ver, _Keyword, _Schema) ->
  [].

%% --

compile(Json, SchemaVersion) when is_map(Json) ->
  Opts = #{ schema => SchemaVersion },
  {ok, Schema} = ejsv_schema:compile(Json, Opts),
  Schema;
compile(List, SchemaVersion) when is_list(List) ->
  lists:map(fun(Item) -> compile(Item, SchemaVersion) end, List);
compile(Other, _Version) ->
  Other.
