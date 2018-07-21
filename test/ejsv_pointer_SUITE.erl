-module(ejsv_pointer_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include("ejsv.hrl").
-compile(export_all).

-define(mod, ejsv_pointer).

all() -> ejsv_testhelper:all(?MODULE).

abstract_pointer_transcoding_test(_Config) ->
  Pointer = "#/properties/%5Epatt(er)n%2B/\~0\~1encode/enum/6",
  Abtract = [<<"properties">>,
             <<"^patt(er)n+">>,
             <<"~/encode">>,
             <<"enum">>, 6],
  ?assertEqual(Pointer, ?mod:abstract_to_pointer(Abtract)),
  ?assertEqual(Abtract, ?mod:pointer_to_abstract(Pointer)).

transform_test(_Config) ->
  Transform = fun
                ([<<"unwanted">>], _V) -> false;
                (A, V) -> {true, {A, V}}
              end,
  Object = #{ <<"prop">> => <<"value">>,
              <<"unwanted">> => <<"value">> },
  ?assertMatch([#p{ pointer = "#/prop",
                    abstract = [<<"prop">>],
                    value = <<"value">>,
                    data = {[<<"prop">>], <<"value">>}}],
               ?mod:json_to_plist(Object, Transform)).

values_test(_Config) ->
  ?assertMatch([#p{ pointer = "#", value = <<"string">>}],
               ?mod:json_to_plist(<<"string">>)),
  ?assertMatch([#p{ pointer = "#", value = 1.0}],
               ?mod:json_to_plist(1.0)),
  ?assertMatch([#p{ pointer = "#", value = 1}],
               ?mod:json_to_plist(1)),
  ?assertMatch([#p{ pointer = "#", value = true}],
               ?mod:json_to_plist(true)),
  ?assertMatch([#p{ pointer = "#", value = null}],
               ?mod:json_to_plist(null)).

array_test(_Config) ->
  Array = [<<"string">>, 1.0, 1, true, null],
  ?assertMatch([#p{ pointer = "#/0", value = <<"string">>},
                #p{ pointer = "#/1", value = 1.0},
                #p{ pointer = "#/2", value = 1},
                #p{ pointer = "#/3", value = true},
                #p{ pointer = "#/4", value = null}],
               ?mod:json_to_plist(Array)).

object_test_skip(_Config) ->
  Object = #{ <<"string">> => <<"string">>,
              <<"float">> => 1.0,
              <<"integer">> => 1,
              <<"bool">> => true,
              <<"null">> => null },
  ?assertEqual([#p{ pointer = "#/bool", value = true },
                #p{ pointer = "#/float", value = 1.0 },
                #p{ pointer = "#/integer", value = 1 },
                #p{ pointer = "#/null", value = null },
                #p{ pointer = "#/string", value = <<"string">> }],
               ?mod:json_to_plist(Object)).

nested_ordered_test(_Config) ->
  Object = #{ <<"prop">> => <<"value">> },
  Array = lists:duplicate(3, Object),
  ObjWithArray = Object#{ <<"array">> => Array },
  ArrayOfObjArrays = lists:duplicate(3, ObjWithArray),
  ct:pal("ObjWithArray ~p~n", [ObjWithArray]),
  ?assertMatch([#p{ pointer = "#/array/0/prop", value = <<"value">> },
                #p{ pointer = "#/array/1/prop", value = <<"value">> },
                #p{ pointer = "#/array/2/prop", value = <<"value">> },
                #p{ pointer = "#/prop", value = <<"value">> }],
               ?mod:json_to_plist(ObjWithArray)),
  ?assertMatch([#p{ pointer = "#/0/array/0/prop", value = <<"value">> },
                #p{ pointer = "#/0/array/1/prop", value = <<"value">> },
                #p{ pointer = "#/0/array/2/prop", value = <<"value">> },
                #p{ pointer = "#/0/prop", value = <<"value">> },
                #p{ pointer = "#/1/array/0/prop", value = <<"value">> },
                #p{ pointer = "#/1/array/1/prop", value = <<"value">> },
                #p{ pointer = "#/1/array/2/prop", value = <<"value">> },
                #p{ pointer = "#/1/prop", value = <<"value">> },
                #p{ pointer = "#/2/array/0/prop", value = <<"value">> },
                #p{ pointer = "#/2/array/1/prop", value = <<"value">> },
                #p{ pointer = "#/2/array/2/prop", value = <<"value">> },
                #p{ pointer = "#/2/prop", value = <<"value">> }],
               ?mod:json_to_plist(ArrayOfObjArrays)).

complex_object_test_skip(_Config) ->
  % XXX requires internet connection
  {ok, Json} = ejsv_utils:json_uri("http://json-schema.org/draft-07/schema"),
  Plist = ?mod:json_to_plist(Json),
  ?assertMatch([#p{ pointer = "#/$id" },
                #p{ pointer = "#/$schema" },
                #p{ pointer = "#/default" },
                #p{ pointer = "#/definitions/nonNegativeInteger/minimum" },
                #p{ pointer = "#/definitions/nonNegativeInteger/type" },
                #p{ pointer = "#/definitions/nonNegativeIntegerDefault0/allOf/0/$ref" },
                #p{ pointer = "#/definitions/nonNegativeIntegerDefault0/allOf/1/default" },
                #p{ pointer = "#/definitions/schemaArray/items/$ref" },
                #p{ pointer = "#/definitions/schemaArray/minItems" },
                #p{ pointer = "#/definitions/schemaArray/type" },
                #p{ pointer = "#/definitions/simpleTypes/enum/0" },
                #p{ pointer = "#/definitions/simpleTypes/enum/1" },
                #p{ pointer = "#/definitions/simpleTypes/enum/2" },
                #p{ pointer = "#/definitions/simpleTypes/enum/3" },
                #p{ pointer = "#/definitions/simpleTypes/enum/4" },
                #p{ pointer = "#/definitions/simpleTypes/enum/5" },
                #p{ pointer = "#/definitions/simpleTypes/enum/6" },
                #p{ pointer = "#/definitions/stringArray/items/type" },
                #p{ pointer = "#/definitions/stringArray/type" },
                #p{ pointer = "#/definitions/stringArray/uniqueItems" },
                #p{ pointer = "#/properties/$comment/type" },
                #p{ pointer = "#/properties/$id/format" },
                #p{ pointer = "#/properties/$id/type" },
                #p{ pointer = "#/properties/$ref/format" },
                #p{ pointer = "#/properties/$ref/type" },
                #p{ pointer = "#/properties/$schema/format" },
                #p{ pointer = "#/properties/$schema/type" },
                #p{ pointer = "#/properties/additionalItems/$ref" },
                #p{ pointer = "#/properties/additionalProperties/$ref" },
                #p{ pointer = "#/properties/allOf/$ref" },
                #p{ pointer = "#/properties/anyOf/$ref" },
                #p{ pointer = "#/properties/const" },
                #p{ pointer = "#/properties/contains/$ref" },
                #p{ pointer = "#/properties/contentEncoding/type" },
                #p{ pointer = "#/properties/contentMediaType/type" },
                #p{ pointer = "#/properties/default" },
                #p{ pointer = "#/properties/definitions/additionalProperties/$ref" },
                #p{ pointer = "#/properties/definitions/type" },
                #p{ pointer = "#/properties/dependencies/additionalProperties/anyOf/0/$ref" },
                #p{ pointer = "#/properties/dependencies/additionalProperties/anyOf/1/$ref" },
                #p{ pointer = "#/properties/dependencies/type" },
                #p{ pointer = "#/properties/description/type" },
                #p{ pointer = "#/properties/else/$ref" },
                #p{ pointer = "#/properties/enum/items" },
                #p{ pointer = "#/properties/enum/minItems" },
                #p{ pointer = "#/properties/enum/type" },
                #p{ pointer = "#/properties/enum/uniqueItems" },
                #p{ pointer = "#/properties/examples/items" },
                #p{ pointer = "#/properties/examples/type" },
                #p{ pointer = "#/properties/exclusiveMaximum/type" },
                #p{ pointer = "#/properties/exclusiveMinimum/type" },
                #p{ pointer = "#/properties/format/type" },
                #p{ pointer = "#/properties/if/$ref" },
                #p{ pointer = "#/properties/items/anyOf/0/$ref" },
                #p{ pointer = "#/properties/items/anyOf/1/$ref" },
                #p{ pointer = "#/properties/items/default" },
                #p{ pointer = "#/properties/maxItems/$ref" },
                #p{ pointer = "#/properties/maxLength/$ref" },
                #p{ pointer = "#/properties/maxProperties/$ref" },
                #p{ pointer = "#/properties/maximum/type" },
                #p{ pointer = "#/properties/minItems/$ref" },
                #p{ pointer = "#/properties/minLength/$ref" },
                #p{ pointer = "#/properties/minProperties/$ref" },
                #p{ pointer = "#/properties/minimum/type" },
                #p{ pointer = "#/properties/multipleOf/exclusiveMinimum" },
                #p{ pointer = "#/properties/multipleOf/type" },
                #p{ pointer = "#/properties/not/$ref" },
                #p{ pointer = "#/properties/oneOf/$ref" },
                #p{ pointer = "#/properties/pattern/format" },
                #p{ pointer = "#/properties/pattern/type" },
                #p{ pointer = "#/properties/patternProperties/additionalProperties/$ref" },
                #p{ pointer = "#/properties/patternProperties/propertyNames/format" },
                #p{ pointer = "#/properties/patternProperties/type" },
                #p{ pointer = "#/properties/properties/additionalProperties/$ref" },
                #p{ pointer = "#/properties/properties/type" },
                #p{ pointer = "#/properties/propertyNames/$ref" },
                #p{ pointer = "#/properties/readOnly/default" },
                #p{ pointer = "#/properties/readOnly/type" },
                #p{ pointer = "#/properties/required/$ref" },
                #p{ pointer = "#/properties/then/$ref" },
                #p{ pointer = "#/properties/title/type" },
                #p{ pointer = "#/properties/type/anyOf/0/$ref" },
                #p{ pointer = "#/properties/type/anyOf/1/items/$ref" },
                #p{ pointer = "#/properties/type/anyOf/1/minItems" },
                #p{ pointer = "#/properties/type/anyOf/1/type" },
                #p{ pointer = "#/properties/type/anyOf/1/uniqueItems" },
                #p{ pointer = "#/properties/uniqueItems/default" },
                #p{ pointer = "#/properties/uniqueItems/type" },
                #p{ pointer = "#/title" },
                #p{ pointer = "#/type/0" },
                #p{ pointer = "#/type/1" }], Plist).