-module(ejsv).
-include("ejsv.hrl").

-export([ validate/2 ]).

validate(Ref, Json) ->
  validate(Ref, Json, #{}).

validate(Ref, Json, Opts) ->
  case resolve(Ref, Opts) of
    {ok, Schema} -> ejsv_jobs:execute(Json, Schema);
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
