-module(ejsv_ref).
-export([resolve/1]).

resolve(Schema) ->
  resolve(Schema, Schema).

resolve(#{ <<"$ref">> := Ref }, Schema) ->
  case get_schema(Ref, Schema) of
    {ok, RefSchema} -> RefSchema;
    {error, _} -> throw({bad_ref, Ref})
  end;
resolve(SchemaMap, Schema) when is_map(SchemaMap) ->
  maps:map(fun(_, V) -> resolve(V, Schema) end, SchemaMap);
resolve(Value, _Schema) ->
  Value.

get_schema(<<"#">>, Schema) -> Schema.
