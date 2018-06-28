-module(ejsv_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-define(mod, ejsv).

all() -> ejsv_testhelper:all(?MODULE).

validate_test(Config) ->
  DataDir = proplists:get_value(data_dir, Config),
  SchemaFile = filename:join([DataDir,"schema.json"]),
  ValidFile = filename:join([DataDir,"valid.json"]),
  InvalidFile = filename:join([DataDir,"invalid.json"]),
  Error = #{ keyword => maximum,
             value => 4,
             props => #{ maximum => 3, exclusive => false },
             message => "value is greater than maximum" },
  {ok, ValidJson} = ejsv_utils:json_file(ValidFile),
  {ok, InvalidJson} = ejsv_utils:json_file(InvalidFile),
  ?assert(?mod:validate(SchemaFile, ValidJson)),
  ?assertMatch({false, [Error]}, ?mod:validate(SchemaFile, InvalidJson)).
