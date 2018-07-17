-module(ejsv).

-export([ validate/2, validate/3 ]).
-export([ transform/2, transform/3 ]).

validate(Ref, Json) ->
  validate(Ref, Json, #{}).

validate(Ref, Json, Opts) ->
  case resolve(Ref, Opts) of
    {ok, Schema} ->
      ejsv_schema:assert(Json, Schema);
    {error, _} = Error -> Error
  end.

transform(Ref, Json) ->
  transform(Ref, Json, #{}).

transform(Ref, Json, Opts) ->
  Validate = maps:get(validate, Opts, false),
  % XXX literally the only reason we assert the json directly is
  % erlang cant distinguish strings from lists
  % otherwise its useful to be able to transform without asserting
  case resolve(Ref, Opts) of
    {ok, Schema} when Validate ->
      case ejsv_schema:assert(Json, Schema) of
        true -> ejsv_schema:transform(Json, Schema);
        {false, Errors} -> {error, Errors}
      end;
    {ok, Schema} ->
      ejsv_schema:transform(Json, Schema);
    {error, _} = Error -> Error
  end.

%% --

resolve(Ref, Opts) ->
  case ejsv_cache:get_schema(Ref) of
    {ok, _} = Cached -> Cached;
    {error, _Error} -> load_schema(Ref, Opts)
  end.

load_schema(Ref, Opts) ->
  case ejsv_utils:json_from_ref(Ref) of
    {error, _} = Error -> Error;
    {ok, Json} ->
      true = ejsv_utils:is_json_object(Json),
      {ok, Schema} = ejsv_schema:compile(Json, Opts),
      ejsv_cache:set_schema(Ref, Schema)
  end.
