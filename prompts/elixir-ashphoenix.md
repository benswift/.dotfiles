# Elixir (AshPhoenix)

This is an Elixir app powered by Ash & Phoenix LiveView.
- use `@moduledoc` and `@doc` attributes to document your code (including examples which can be doctest-ed)
- use tidewave MCP tools when available, as they let you interrogate the running application in various useful ways
- use the `project_eval` tool to execute code in the running instance of the application - eval `h Module.fun` to get documentation for a module or function
- use the `package_docs_search` and `get_docs` tools to find the documentation for library code
- prefer using LiveView instead of regular Controllers
- once you are done with changes, run `mix compile` and fix any issues
- write tests for your changes and run `mix test` afterwards
- use `ExUnitProperties` for property-based testing and `use Ash.Generator` to create seed data for these tests
- in tests, don't require exact matches of error messages - raising the right type of error is enough
- use `list_generators` to list available generators when available, otherwise `mix help` - if you have to run generator tasks, pass `--yes` and always prefer to use generators as a basis for code generation, then modify afterwards
- always use Ash concepts, almost never ecto concepts directly - think hard about the "Ash way" to do things and look for information in the rules & docs of Ash & associated packages if you don't know
- when creating new Ash resources/validations/changes/calculations, use proper module-based versions, and use the appropriate generator (e.g. `mix ash.gen.resource` or `mix ash.gen.change`) to create the boilerplate files
- never attempt to start or stop a phoenix applicationin
