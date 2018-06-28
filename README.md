# Erlang Json Schema Validator

This library seeks to support all standards of schema validation on JSON objects.

#### Supported schema versions

- Json Schema v3

#### Requirements of Library

- Prevent duplication of validation logic between schema versions
- When invalid provide complete list of failures with context
- Cache resolved schema definitions for performance

# API

```erlang
-type uri().
-type filename().
-type schema_ref() -> uri() | filename().

-spec ejsv:validate(ref :: schema_ref(), JSON :: map()) ->
  true |
  {error, list(map())}.
```

#### Usage

```erlang
SchemaFilename = "./user-schema.json",
JsonMap = #{ field => "value" },
true = ejsv:validate(SchemaFilename, JsonMap).
```

# Contribution

Validate swagger spec itself using [Swagger Spec Test Suite](https://github.com/Yelp/swagger_spec_validator).
Validate json schema rules using [JSON Schema Test Suite](https://github.com/json-schema-org/JSON-Schema-Test-Suite.git).
How everyone must be written [JSON Schema Validation Spec](https://tools.ietf.org/html/draft-handrews-json-schema-validation-01).
