-module(ejsv_schema).
-include("ejsv.hrl").

-export([ compile/2, transform/2, assert/2 ]).

compile(JsonMap, Opts) when is_map(JsonMap) ->
  SchemaType = maps:get(schema, Opts, json_schema),
  SchemaVersion = maps:get(version, Opts, {3,0}),
  % TODO JsonPathed = ejsv_utils:add_path(JsonMap, "/"),
  % probably in ejsv_assertions:execute/2
  SchemaTag = {SchemaType, SchemaVersion},
  Keywords = ejsv_keywords:for_schema(SchemaTag),
  Transform = ejsv_transform:for_schema(SchemaTag, JsonMap),
  % TODO ResolvedSchema = ejsv_ref:resolve(JsonMap),
  ReduceKeyword = fun(Keyword) -> reduce_keywords(SchemaTag, JsonMap, Keyword) end,
  SchemaKeywords = lists:concat(lists:map(ReduceKeyword, Keywords)),
  {ok, #schema{ type = SchemaType,
                version = SchemaVersion,
                transform = Transform,
                keywords = SchemaKeywords }}.

reduce_keywords(Version, Schema, Keyword) ->
  Keywords = ejsv_keywords:define(Version, Keyword, Schema),
  lists:flatten([Keywords]).

transform(Json, #schema{ transform = Transform }) ->
  Transform(Json).

-record(job, { errors = [],
               data }).

assert(_Data, #schema{ keywords = [] }) ->
  true;
assert(Data, #schema{ keywords = Keywords }) ->
  case
    % TODO run jobs in parallel
    lists:foldl(fun run_job/2,
                #job{ data = Data },
                Keywords)
  of
    #job{ errors = [] } -> true;
    #job{ errors = Errors } -> {false, Errors}
  end.

run_job({Keyword, Opts}, #job{ data = Json } = St) ->
  Detail = #{ keyword => Keyword,
              value => Json,
              props => Opts },
  case ejsv_assertions:Keyword(Json, Opts) of
    true ->
      St;
    false ->
      Error = Detail#{ message => ejsv_assertions:Keyword(error) },
      St#job{ errors = [Error|St#job.errors] };
    {false, Msg} ->
      Error = Detail#{ message => Msg },
      St#job{ errors = [Error|St#job.errors] }
  end.

