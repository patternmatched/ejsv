-module(ejsv_transform).
-export([ for_schema/2 ]).

-import(ejsv_utils, [ maybe_binary_to_list/1 ]).

-record(value, { name,
                 type,
                 format }).

-record(object, { name,
                  properties }).

-record(list, { name,
                type,
                types,
                other_type }).

% TODO allOf, anyOf, oneOf

for_schema(SchemaTag, Schema) when is_map(Schema) ->
  Rules = extract(SchemaTag, Schema),
  fun(Json) -> transform(Json, Rules) end.

% primitive types
% https://github.com/OAI/OpenAPI-Specification/blob/OpenAPI.next/versions/3.0.0.md#dataTypes
% https://spacetelescope.github.io/understanding-json-schema/reference/type.html

extract(_SchemaTag, #{ <<"type">> := <<"boolean">> }) ->
  #value{ type = boolean };
extract(_SchemaTag, #{ <<"type">> := <<"string">> } = Schema) ->
  Format = maps:get(<<"format">>, Schema, undefined),
  #value{ type = string, format = maybe_binary_to_list(Format) };
extract(_SchemaTag, #{ <<"type">> := <<"date">> }) ->
  #value{ type = string, format = "date" };
extract(_SchemaTag, #{ <<"type">> := <<"date-time">> }) ->
  #value{ type = string, format = "date-time" };
extract(_SchemaTag, #{ <<"type">> := <<"integer">> }) ->
  #value{ type = integer };
extract(_SchemaTag, #{ <<"type">> := <<"number">> }) ->
  #value{ type = integer };
extract(SchemaTag, #{ <<"type">> := <<"object">> } = Schema) ->
  Properties = maps:get(<<"properties">>, Schema, #{}),
  Props = maps:map(fun(Prop, PropSchema) ->
                       Rules = extract(SchemaTag, PropSchema),
                       set_name(Prop, Rules)
                   end, Properties),
  #object{ properties = Props };
extract(SchemaTag, #{ <<"properties">> := _ } = Schema) ->
  extract(SchemaTag, Schema#{ <<"type">> => <<"object">> });
extract(SchemaTag, #{ <<"type">> := <<"array">> } = Schema) ->
  case maps:get(<<"items">>, Schema, undefined) of
    ItemSchema when is_map(ItemSchema) ->
      Type = extract(SchemaTag, ItemSchema),
      #list{ type = Type };
    ItemSchemas when is_list(ItemSchemas) ->
      Types = lists:map(fun(ItemSchema) -> extract(SchemaTag, ItemSchema) end, ItemSchemas),
      AddItem = maps:get(<<"additionalItems">>, Schema, #{}),
      Type = extract(SchemaTag, AddItem),
      #list{ types = Types, other_type = Type };
    undefined ->
      undefined
  end;
extract(_SchemaTag, _Schema) ->
  undefined.

set_name(Name, #value{} = Rule) -> Rule#value{ name = Name };
set_name(Name, #object{} = Rule) -> Rule#object{ name = Name };
set_name(Name, #list{} = Rule) -> Rule#list{ name = Name };
set_name(_Name, undefined) -> undefined.

transform(Value, #value{ type = string, format = "date" }) ->
  try
    String = binary_to_list(Value),
    {ok,[YYYY,MM,DD],[]} = io_lib:fread("~d-~d-~d", String),
    true = calendar:valid_date(YYYY, MM, DD),
    {YYYY, MM, DD}
  catch
    _:_ -> Value
  end;
transform(Value, #value{ type = string, format = "date-time" }) ->
  try
    String = binary_to_list(Value),
    {ok,[YYYY,MM,DD,HH,NN,SS],[]} = io_lib:fread("~d-~d-~dT~d:~d:~d", String),
    true = calendar:valid_date(YYYY, MM, DD),
    {{YYYY, MM, DD}, {HH, NN, SS}}
  catch
    _:_ -> Value
  end;
transform(Value, #value{ type = string, format = "atom" }) ->
  try
    binary_to_existing_atom(Value, unicode)
  catch
    _:_ -> Value
  end;
transform(Value, #value{ type = string }) ->
  try
    binary_to_list(Value)
  catch
    _:_ -> Value
  end;
transform(Value, #value{ type = _Type }) ->
  Value;
transform(Value, #object{ properties = Props }) when is_map(Value) ->
  maps:fold(fun(Prop, V, Acc) ->
                case maps:get(Prop, Props, undefined) of
                  undefined -> Acc;
                  PropSchema ->
                    Transformed = transform(V, PropSchema),
                    maps:put(binary_to_list(Prop), Transformed, Acc)
                end
            end, #{}, Value);
transform([], _Schema) ->
  [];
transform(Value, #list{ type = Type }) when Type =/= undefined ->
  [ transform(V, Type) || V <- Value ];
transform(Value, #list{ types = Types, other_type = OtherType }) ->
  Split = min(length(Value), length(Types)),
  Tuple = [ transform(lists:nth(Idx, Value), lists:nth(Idx, Types))
            || Idx <- lists:seq(1, Split) ],
  Adds = [ transform(lists:nth(Idx, Value), OtherType)
           || Idx <- lists:seq(Split + 1, length(Value)) ],
  Tuple ++ Adds;
transform(Value, _Untransformable) ->
  Value.
