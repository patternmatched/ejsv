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
