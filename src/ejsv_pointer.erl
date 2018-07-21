-module(ejsv_pointer).
-include("ejsv.hrl").
-import(ejsv_utils, [ lists_join/2 ]).
-export([ json_to_plist/1, json_to_plist/2 ]).

-ifdef(TEST).
-compile(export_all).
-endif.

json_to_plist(Json) ->
  json_to_plist(Json, undefined).
json_to_plist(Json, Transform) ->
  Reduced = reduce([], Transform, Json, []),
  Filter = fun(P) -> P#p.data =/= filter end,
  lists:sort(lists:filter(Filter, Reduced)).

reduce(BasePath, Transform, Object, Plist) when is_map(Object) ->
  lists:foldl(fun({Key, Value}, Acc) ->
                  Path = [Key|BasePath],
                  reduce(Path, Transform, Value, Acc)
              end, Plist, maps:to_list(Object));
reduce(BasePath, Transform, Array, Plist) when is_list(Array) ->
  WithIndex = lists:zip(lists:seq(0, length(Array)-1), Array),
  lists:foldr(fun({Idx, Item}, Acc) ->
                  Path = [Idx|BasePath],
                  reduce(Path, Transform, Item, Acc)
              end, Plist, WithIndex);
reduce(BasePath, Transform, Value, Plist) ->
  Path = lists:reverse(BasePath),
  [transform(Path, Transform, Value) | Plist].

transform(Abtract, Transform, Value) ->
  #p{ pointer = abstract_to_pointer(Abtract),
      abstract = Abtract,
      value = Value,
      data = apply_transform(Transform, Abtract, Value) }.

apply_transform(undefined, _A, _V) -> undefined;
apply_transform(Fun, Abtract, Value) ->
  case Fun(Abtract, Value) of
    {true, NewValue} -> NewValue;
    true -> undefined;
    false -> filter
  end.

%% --

abstract_to_pointer(Parts) ->
  Path = lists_join("/", ["#"|Parts]),
  abstract_to_pointer(Path, []).
abstract_to_pointer([], Pointer) ->
  Pointer;
abstract_to_pointer([Part|Parts], Pointer) ->
  PStr = case Part of
           Idx when is_integer(Idx) -> integer_to_list(Part);
           Bin when is_binary(Bin) -> encode(binary_to_list(Bin));
           Str when is_list(Str) -> Str
         end,
  abstract_to_pointer(Parts, Pointer ++ PStr).

pointer_to_abstract([$#|Uri]) ->
  Parts = string:tokens(http_uri:decode(Uri), "/"),
  lists:map(fun abstract_part/1, Parts).

abstract_part(Part) ->
  try
    list_to_integer(Part)
  catch
    _:_ ->
      list_to_binary(decode(Part))
  end.

encode(Chars) ->
  encode(Chars, []).
encode([], Encoded) ->
  http_uri:encode(lists:reverse(Encoded));
encode([$~|Chars], Encoded) ->
  encode(Chars, lists:append("0~", Encoded));
encode([$/|Chars], Encoded) ->
  encode(Chars, lists:append("1~", Encoded));
encode([Char|Chars], Encoded) ->
  encode(Chars, [Char|Encoded]).

decode(Chars) ->
  decode(Chars, []).
decode([], Encoded) ->
  lists:reverse(Encoded);
decode("\~0" ++ Chars, Encoded) ->
  decode(Chars, [$~, Encoded]);
decode("\~1" ++ Chars, Encoded) ->
  decode(Chars, [$/, Encoded]);
decode([Char|Chars], Encoded) ->
  decode(Chars, [Char|Encoded]).
