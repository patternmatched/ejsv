-module(ejsv_cache).
-include("ejsv.hrl").

-export([ install/0, delete/0, clear/0 ]).
-export([ get_schema/1, set_schema/2 ]).

install() ->
  Opts = [ public, named_table,
           {keypos, #schema.id},
           {read_concurrency, true} ],
  ?MODULE = ets:new(?MODULE,Opts), ok.

delete() ->
  true = ets:delete(?MODULE), ok.

clear() ->
  true = ets:delete_all_objects(?MODULE), ok.

%% --

get_schema(Id) ->
  try ets:lookup(?MODULE,Id) of
    [Schema] -> {ok,Schema};
    [] -> {error, not_found}
  catch
    error:badarg -> {error, not_installed}
  end.

set_schema(Id,Schema) ->
  Value = Schema#schema{ id = Id },
  try
    true = ets:insert(?MODULE,Value),
    {ok, Value}
  catch
    error:badarg -> {ok, Value}
  end.
