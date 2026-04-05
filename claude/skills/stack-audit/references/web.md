# Web preference checklist

## Structural

| Check                 | Preference                                      | Anti-pattern                         |
| --------------------- | ----------------------------------------------- | ------------------------------------ |
| Package manager       | pnpm                                            | npm, yarn                            |
| Type checker          | tsgo (`@typescript/native-preview`)             | JS-based `tsc`                       |
| Linter                | oxlint                                          | ESLint                               |
| Formatter             | oxfmt                                           | Prettier                             |
| App bundler           | Vite                                            | webpack, parcel, rollup (standalone) |
| App bundler (Phoenix) | esbuild (Phoenix default)                       | webpack in Phoenix projects          |
| Library bundler       | tsdown                                          | tsup, rollup, unbuild                |
| E2E testing           | Playwright                                      | Cypress, Selenium                    |

### Structural notes

- **tsgo**: Astro and Svelte tooling don't support tsgo yet, so `tsc` via
  `astro check` / `svelte-check` is acceptable in those projects. Don't flag
  this as a divergence when the project uses Astro or Svelte.
- **oxlint jsPlugins**: oxlint can consume ESLint plugin packages via its
  `jsPlugins` config field. ESLint plugin packages in `devDependencies` are
  expected when listed in `jsPlugins` --- don't flag them as vestigial.

## Soft

| Check                   | Preference                                               | Anti-pattern                                               |
| ----------------------- | -------------------------------------------------------- | ---------------------------------------------------------- |
| HTML                    | semantic elements (`nav`, `article`, `section`, `aside`) | div soup                                                   |
| Accessibility           | ARIA roles, labels, keyboard navigation, WCAG            | missing alt text, non-interactive divs with click handlers |
| CSS framework           | Tailwind (when present)                                  | Bootstrap, Material UI CSS                                 |
| CSS layout              | CSS Grid (primary) and Flexbox (1D)                      | float-based layouts, excessive breakpoint classes          |
| Responsive design       | fluid layouts, container queries                         | breakpoint-only responsive design                          |
| CSS theming             | custom properties                                        | inline styles, hardcoded values                            |
| CSS linter              | Stylelint                                                | no CSS linting                                             |
| CSS animations          | CSS transitions/animations over JS                       | jQuery animations, JS where CSS suffices                   |
| JS animations           | Motion library (when CSS can't handle it)                | GSAP, anime.js, jQuery                                     |
| JavaScript              | ES6+ (arrow functions, destructuring, async/await)       | `var`, callbacks, CommonJS `require`                       |
| Module system           | ES modules (`import`/`export`, `type="module"`)          | CommonJS, global scripts                                   |
| Code style              | functional (map, filter, reduce, pure functions)         | imperative loops, mutation-heavy                           |
| Ecosystem hygiene       | `@e18e/eslint-plugin` (works with oxlint and ESLint)     | no dependency weight linting                               |
| Unit testing            | Vitest                                                   | Jest                                                       |
| Progressive enhancement | works without JS where possible                          | JS-required for basic content                              |

## Recommended lint/format baselines

### oxlint

Use category-based configuration rather than listing rules individually.
Flag configs with large explicit rule lists (50+ rules) as a divergence ---
most of those rules are already covered by the built-in categories.

Recommended baseline:

- **Categories**: `correctness` (error) + `suspicious` (error) + `perf` (error)
- **Plugins**: `typescript`, `import`, `unicorn` (all built-in)
- **jsPlugins**: `@e18e/eslint-plugin`, `eslint-plugin-no-only-tests`,
  `eslint-plugin-perfectionist`, `eslint-plugin-unused-imports`
- **Cherry-pick rules** on top of categories as needed (e.g. `eqeqeq`,
  `no-warning-comments`, plugin-specific rules)
- **Don't enable**: `style`, `pedantic`, or `restriction` categories wholesale
  --- cherry-pick individual rules from these if wanted

The config file should be concise (under ~40 lines). If it's significantly
longer, the project is probably listing rules that the categories already cover.

### oxfmt

Should have an `.oxfmtrc.json` with at minimum:

```json
{
  "$schema": "./node_modules/oxfmt/configuration_schema.json"
}
```

Running with no config file works but emits a warning on every invocation.

### Stylelint

`stylelint-config-standard` is the right baseline. Minimal customisation is
expected --- typically just disabling `no-descending-specificity` (too many
false positives with modern CSS patterns).
