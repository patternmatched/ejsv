# Erlang Json Schema Validator

This library seeks to support all standards of schema validation on JSON objects.

Simplicity and completeness first, performance next.

Copyright 2018 www.patternmatched.com [MIT License][MIT]

#### Supported schema versions

- Json Schema v3
- Json Schema v4

#### Requirements of Library

- Prevent duplication of assertion logic between schema versions
- When invalid provide complete list of failures with context
- Cache resolved schema definitions for performance
- Assert in parallel where possible
- Can be built and tested with rebar, rebar3 and mix

# API

```erlang
-type schema_ref() :: uri_string:uri_string() |
  file:filename().

-type ejsv_error() ::
  #{ keyword => atom(),
     value => term(),
     props => map(),
     message => string() }.

-type ejsv_opts() ::
  #{ validate => boolean(),
     schema => json_schema,
     version => {integer(), integer()} }.

-spec ejsv:transform(ref :: schema_ref(), JSON :: ejsv_opts()) ->
  true |
  {error, list(ejsv_error())}.
```

#### Usage

```erlang
SchemaFilename = "./user-schema.json",
JsonMap = #{ field => "value" },
true = ejsv:transform(SchemaFilename, JsonMap, #{ validate => true }).
```

# Contribution

###### Next Steps

- [ ] Rebuild schema cache mechanism to allow dereferencing of `$refs` **URGENT**
- [ ] Expose errors from sub schema assertions
- [ ] Run assertions on transformed data
- [ ] Add build and test artifacts for rebar, rebar3 and mix as in [jsx](https://github.com/talentdeficit/jsx)
- [ ] Test and add travis test status to readme
- [ ] Complete validation for json schema v5 to v9
- [ ] Add Open API schema validation v1.2 to v3.0
- [ ] Add parelled excution of assertions
- [ ] Convert ct to eunit tests for speed
- [ ] Create or update erlang json validation benchmark repo

###### References

- [JSON Schema Test Suite](https://github.com/json-schema-org/JSON-Schema-Test-Suite.git) - validate json schema logic
- [JSON Schema Validation Spec](https://tools.ietf.org/html/draft-handrews-json-schema-validation-01) - guide project design to best describe spec
- [Swagger Spec Test Suite](https://github.com/Yelp/swagger_spec_validator) - validate swagger spec itself
