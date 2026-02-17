# Elixir preference checklist

## Structural

| Check | Preference | Anti-pattern |
|---|---|---|
| Resource framework | Ash declarative resources | raw Ecto schemas and changesets |
| Frontend | Phoenix LiveView | Phoenix controllers with JSON or server-rendered templates |
| Resource definitions | Ash's declarative DSL (`attributes`, `actions`, `relationships`) | hand-rolled Ecto schemas mimicking Ash patterns |

## Soft

| Check | Preference | Anti-pattern |
|---|---|---|
| Testing syntax | PhoenixTest (`visit`, `click_link`, `fill_in`) | Phoenix.LiveViewTest direct syntax |
| Test data | Ash.Generator | ex_machina, manual factory functions |
| Dev debugging | tidewave in dev deps | manual IEx debugging only |
| Formatter config | `.formatter.exs` with Ash/Phoenix plugins configured | missing or default formatter config |
| Validations | module-based Ash validations | inline anonymous functions in resource DSL |
| Changes | module-based Ash changes | inline anonymous functions |
| Calculations | module-based Ash calculations | computed fields in queries |
| Code generation | `mix ash.gen.*` generators with `--yes` flag | manually creating resource boilerplate |
| Domain boundaries | Ash code interfaces on domains | calling Ash actions directly from LiveViews |
| Error matching | verify error types in tests | exact error message string matching |
