-module(ejsv_schema).
-include("ejsv.hrl").

-export([ compile/2, transform/2, assert/2 ]).

compile(SchemaJson, Opts) when is_map(SchemaJson) ->
  SchemaVersion = maps:get(schema, Opts, {json_schema, {3,0}}),
  Keywords = ejsv_keywords:for_schema(SchemaVersion),
  Transform = ejsv_transform:for_schema(SchemaVersion, SchemaJson),
  ReduceKeyword = fun(Keyword) -> reduce_keywords(SchemaVersion, SchemaJson, Keyword) end,
  SchemaKeywords = lists:concat(lists:map(ReduceKeyword, Keywords)),
  {ok, #schema{ version = SchemaVersion,
                transform = Transform,
                keywords = SchemaKeywords }}.

  % TODO JsonPathed = ejsv_utils:add_path(SchemaJson, "/"),
  % probably in ejsv_assertions:execute/2
  % TODO ResolvedSchema = ejsv_ref:resolve(SchemaJson),

reduce_keywords(Version, Schema, Keyword) ->
  Keywords = ejsv_keywords:define(Version, Keyword, Schema),
  lists:map(fun(K) -> transform_keyword(K, Version, Keyword) end,
            lists:flatten([Keywords])).

transform_keyword({Fun, Params}, Version, Keyword) ->
  transform_keyword({Fun, Params, Params}, Version, Keyword);
transform_keyword({Fun, Opts, Params}, Version, Keyword) ->
  #keyword{ name = Keyword,
            % path,
            function = Fun,
            options = Opts,
            params = Params,
            schema = Version }.

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

run_job(Keyword, #job{ data = Json } = St) ->
  #keyword{ name = Name,
            function = Assert,
            options = Opts,
            params = Params } = Keyword,
  case ejsv_assertions:Assert(Json, Opts) of
    true ->
      St;
    false ->
      Msg = ejsv_assertions:Assert(error),
      Error = #{ keyword => Name,
                 message => Msg,
                 value => Json,
                 params => Params },
      St#job{ errors = [Error|St#job.errors] };
    {false, Msg} when is_integer(hd(Msg)) ->
      Error = #{ keyword => Name,
                 message => Msg,
                 value => Json,
                 params => Params },
      St#job{ errors = [Error|St#job.errors] };
    {false, Errors} when is_map(hd(Errors)) ->
      Error = #{ keyword => Name,
                 causes => Errors },
      St#job{ errors = [Error|St#job.errors] }
  end.

