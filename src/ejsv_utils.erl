-module(ejsv_utils).
-compile(export_all).

json_from_ref(Ref) ->
  case ref_type(Ref) of
    uri -> json_uri(Ref);
    file -> json_file(Ref)
  end.

json_uri(Uri) ->
  case httpc:request(get,{Uri,[]},[],[{body_format,binary}]) of
    {error, Error} -> throw({json_uri,Error,Uri});
    {ok,{{_,200,_},_Headers,Json}} -> parse_json(Json);
    {ok,{{_,HttpError,_},_Headers,_Body}} -> throw({json_uri,{http_error, HttpError},Uri})
  end.

json_file(Filename) ->
  case file:read_file(Filename) of
    {error, Error} -> throw({json_file,Error,Filename});
    {ok, Json} -> parse_json(Json)
  end.

parse_json(Json) ->
  Library = find_parser(),
  try
    {ok, Library:decode(Json, [return_maps])}
  catch
    {throw,{error,_} = Error} -> Error;
    _:E -> {error,E}
  end.

find_parser() ->
  case application:get_env(?MODULE, library, undefined) of
    undefined ->
      Modules = [code:ensure_loaded(jiffy), code:ensure_loaded(jsx)],
      Library = proplists:get_value(module, Modules),
      ok = application:set_env(?MODULE, library, Library),
      Library;
    Library -> Library
  end.

is_json_object(Json) -> is_map(Json).

ref_type(Ref) ->
  case http_uri:parse(Ref) of
    {ok, _} -> uri;
    _ -> file
  end.

add_path(Map, Path) ->
  Parent = maps:get(path, Map, ""),
  maps:put(path, filename:join(Parent, Path), Map).

lists_search(Condition, [E | Rest]) ->
  case Condition(E) of
    true -> {value, E};
    false -> lists_search(Condition, Rest)
  end;
lists_search(_Cond, []) -> false.

remove_dups([]) ->
  [];
remove_dups([H|T]) ->
  [H | [X || X <- remove_dups(T), X /= H]].

merge_lists(Parent,Child) ->
  Combined = Parent ++ Child,
  Fun = fun(Key) ->
            [Value|_] = proplists:get_all_values(Key,Combined),
            {Key,Value}
        end,
  lists:map(Fun,proplists:get_keys(Combined)).

combine_lists(Parent,Child) ->
  Combined = Parent ++ Child,
  Fun = fun(Key) ->
            case proplists:get_all_values(Key,Combined) of
              [L1, L2] when is_list(L1), is_list(L2) -> {Key,L1 ++ L2};
              [L1, V2] when is_list(L1) -> {Key,L1 ++ [V2]};
              [V1, L2] when is_list(L2) -> {Key,[V1|L2]};
              [V1, V2] -> {Key, [V1, V2]};
              [V] -> {Key, V};
              Vs -> throw({additional_values_found, Key, Vs})
            end
        end,
  lists:map(Fun,proplists:get_keys(Combined)).
