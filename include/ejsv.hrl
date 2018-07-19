-record(keyword, { name,
                   path,
                   function,
                   options,
                   params,
                   schema,
                   data,
                   errors = [] }).

-record(schema, { id,
                  keywords,
                  transform,
                  version }).
