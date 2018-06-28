-module(ejsv_assertions).
-include("ejsv.hrl").
-compile(export_all).

% XXX checks should be as efficient as possible
% if possible do work in keywords module
% can break up a checks to take advantage of parallel execution

enum(error) -> "value does not match enum".
enum(V, #{ match := Enums }) ->
  lists:member(V, Enums).

%% --

properties(V, _Opts) when not is_map(V) ->
  true;
properties(V, #{ properties := PropSchemas }) ->
  true = is_map(V),
  true = is_map(PropSchemas),
  case maps:fold(fun check_prop/3, {V, []}, PropSchemas) of
    {V, []} -> true;
    {V, Errors} -> {false, Errors}
  end.

% XXX perhaps some of this should be done in keywords?
check_prop(Key, Schema, {V, Errors}) ->
  case maps:get(Key, V, undefined) of
    undefined -> {V, Errors};
    Prop ->
      case ejsv_schema:assert(Prop, Schema) of
        true -> {V, Errors};
        {false, MoreErrors} ->
          {V, Errors ++ MoreErrors}
      end
  end.

%% --

add_props(error) -> "additional property not allowed".
add_props(V, _Opts) when not is_map(V) ->
  true;
add_props(V, #{ properties := PropsAllowed, allowed := Allowed, patterns := Patterns }) ->
  Props = maps:keys(V),
  {Matched, Unmatched} = match_props_by_pattern(Props, Patterns),
  Adds = (Props -- PropsAllowed) -- Matched,
  HaveUnmatched = length(Unmatched -- Props) > 0,
  HaveAdds = length(Adds) > 0,
  if
    not HaveAdds -> true;
    HaveAdds and Allowed -> true;
    HaveUnmatched and not Allowed -> false;
    true -> false
  end.

adds_schema(error) -> "additional property does not match schema".
adds_schema(V, #{ properties := PropsAllowed,
                  schema := Schema,
                  patterns := Patterns }) ->
  Props = maps:keys(V),
  {Matched, _Unmatched} = match_props_by_pattern(Props, Patterns),
  Adds = (Props -- PropsAllowed) -- Matched,
  Validate = fun(Add) -> check_schema(maps:get(Add, V), Schema) end,
  lists:all(Validate, Adds).

patterns_schema(error) -> "pattern property does not match schema".
patterns_schema(V, _Opts) when not is_map(V) ->
  true;
patterns_schema(V, #{ patterns := PatternSchemas }) ->
  Props = maps:keys(V),
  lists:all(fun(Prop) ->
                MatchValidate = fun({Pattern, Schema}) ->
                                    case check_match(Prop, Pattern) of
                                      true -> check_schema(maps:get(Prop, V), Schema);
                                      false -> true
                                    end
                                end,
                lists:all(MatchValidate, maps:to_list(PatternSchemas))
            end, Props).

%% --

items(error) -> "items failed validation".
items(V, _Opts) when not is_list(V) ->
  true;
items(V, #{ schema := ItemSchemas }) when is_list(ItemSchemas) ->
  Split = min(length(V), length(ItemSchemas)),
  Checks = [ check_schema(lists:nth(Idx, V), lists:nth(Idx, ItemSchemas))
             || Idx <- lists:seq(1, Split) ],
  lists:all(fun(B) -> B =:= true end, Checks);
items(V, #{ schema := ItemSchema } = Opts) when is_record(ItemSchema, schema) ->
  ItemSchemas = lists:duplicate(length(V), ItemSchema),
  items(V, Opts#{ schema := ItemSchemas }).

add_items(error) -> "additional items not allowed".
add_items(V, _Opts) when not is_list(V) ->
  true;
add_items(V, #{ item_count := Items, schema := Schema }) ->
  case is_record(Schema, schema) of
    false -> length(V) =< Items;
    true ->
      Adds = lists:sublist(V, Items + 1, 10000),
      lists:all(fun(Item) ->
                    check_schema(Item, Schema)
                end, Adds)
  end.

%% --

required(error) -> "required property not found".
required(V, _Opts) when not is_map(V) ->
  true;
required(V, #{ required := Required }) ->
  length(Required -- maps:keys(V)) == 0.

of_type(error) -> "items failed validation".
of_type(V, #{ type := Type }) ->
  check_type(V, Type).

one_of_type(error) -> "items failed validation".
one_of_type(V, #{ types := Types }) ->
  lists:any(fun(Type) -> check_type(V, Type) end, Types).

pattern(error) -> "string does not match pattern".
pattern(V, _Opts) when not is_binary(V) ->
  true;
pattern(V, #{ regex := Pattern }) ->
  check_match(V, Pattern).

min_items(error) -> "number of items do not satisfy minimum".
min_items(V, _Opts) when not is_list(V) ->
  true;
min_items(V, #{ min := Min }) ->
  length(V) >= Min.

max_items(error) -> "number of items do not satisfy maximum".
max_items(V, _Opts) when not is_list(V) ->
  true;
max_items(V, #{ max := Max }) ->
  length(V) =< Max.

min_length(error) -> "length of string do not satisfy minimum".
min_length(V, _Opts) when not is_binary(V) ->
  true;
min_length(V, #{ min := Min }) ->
  length(unicode:characters_to_list(V)) >= Min.

max_length(error) -> "length of string do not satisfy maximum".
max_length(V, _Opts) when not is_binary(V) ->
  true;
max_length(V, #{ max := Max }) ->
  length(unicode:characters_to_list(V)) =< Max.

unique_items(error) -> "items are not unique".
unique_items(V, _Opts) when not is_list(V) ->
  true;
unique_items(V, #{}) ->
  DupsRemoved = ejsv_utils:remove_dups(V),
  length(V) == length(DupsRemoved).

maximum(error) -> "value is greater than maximum".
maximum(V, #{ maximum := Max, exclusive := Exclusive }) ->
  case is_number(V) of
    true when Exclusive -> V < Max;
    true -> V =< Max;
    false -> true
  end.

minimum(error) -> "value is less than minimum".
minimum(V, #{ minimum := Min, exclusive := Exclusive }) ->
  case is_number(V) of
    true when Exclusive -> V > Min;
    true -> V >= Min;
    false -> true
  end.

div_by(error) -> "value is not divisible by factor".
div_by(V, _Opts) when not is_number(V) ->
  true;
div_by(V, #{ factor := Factor }) ->
  Div = V / Factor,
  float(round(Div)) =:= Div.

%% helpers

check_match(Str, Pattern) ->
  match =:= re:run(Str, Pattern, [{capture, none}, unicode]).

match_props_by_pattern(Props, Patterns) ->
  Match = fun(Key) ->
              lists:any(fun(Pattern) ->
                            check_match(Key, Pattern)
                        end, Patterns)
          end,
  lists:partition(Match, Props).

check_type(V, <<"string">>)  -> is_binary(V);
check_type(V, <<"number">>)  -> is_number(V);
check_type(V, <<"integer">>) -> is_integer(V);
check_type(V, <<"boolean">>) -> is_boolean(V);
check_type(V, <<"object">>)  -> is_map(V);
check_type(V, <<"array">>)   -> is_list(V);
check_type(null, <<"null">>) -> true;
check_type(_V, <<"any">>)    -> true;
check_type(V, #schema{} = S) -> check_schema(V, S);
check_type(_V, _Type)        -> false.

check_schema(V, Schema) ->
  case ejsv_schema:assert(V, Schema) of
    true -> true;
    {false,_} -> false
  end.
