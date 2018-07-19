-module(ejsv_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-import(ejsv_testhelper, [ fixture/1 ]).

-define(mod, ejsv).

validate_test() ->
  SchemaFile = fixture("schema.json"),
  ValidFile = fixture("valid.json"),
  InvalidFile = fixture("invalid.json"),
  Error = #{ keyword => maximum,
             value => 4,
             props => #{ maximum => 3, exclusive => false },
             message => "value is greater than maximum" },
  {ok, ValidJson} = ejsv_utils:json_file(ValidFile),
  {ok, InvalidJson} = ejsv_utils:json_file(InvalidFile),
  ?assert(?mod:validate(SchemaFile, ValidJson)),
  ?assertMatch({false, [Error]}, ?mod:validate(SchemaFile, InvalidJson)).
