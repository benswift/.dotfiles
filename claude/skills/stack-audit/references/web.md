# Web preference checklist

## Structural

| Check | Preference | Anti-pattern |
|---|---|---|
| Package manager | pnpm | npm, yarn |
| Type checker | tsgo (`@typescript/native-preview`) | JS-based `tsc` |
| Linter | oxlint with `@nkzw/oxlint-config` | ESLint |
| Formatter | oxfmt (Prettier still fine for non-JS/TS files) | Prettier for JS/TS formatting |
| App bundler | Vite | webpack, parcel, rollup (standalone) |
| App bundler (Phoenix) | esbuild (Phoenix default) | webpack in Phoenix projects |
| Library bundler | tsdown | tsup, rollup, unbuild |
| E2E testing | Playwright | Cypress, Selenium |

## Soft

| Check | Preference | Anti-pattern |
|---|---|---|
| HTML | semantic elements (`nav`, `article`, `section`, `aside`) | div soup |
| Accessibility | ARIA roles, labels, keyboard navigation, WCAG | missing alt text, non-interactive divs with click handlers |
| CSS framework | Tailwind (when present) | Bootstrap, Material UI CSS |
| CSS layout | CSS Grid (primary) and Flexbox (1D) | float-based layouts, excessive breakpoint classes |
| Responsive design | fluid layouts, container queries | breakpoint-only responsive design |
| CSS theming | custom properties | inline styles, hardcoded values |
| CSS animations | CSS transitions/animations over JS | jQuery animations, JS where CSS suffices |
| JS animations | Motion library (when CSS can't handle it) | GSAP, anime.js, jQuery |
| JavaScript | ES6+ (arrow functions, destructuring, async/await) | `var`, callbacks, CommonJS `require` |
| Module system | ES modules (`import`/`export`, `type="module"`) | CommonJS, global scripts |
| Code style | functional (map, filter, reduce, pure functions) | imperative loops, mutation-heavy |
| Unit testing | Vitest | Jest |
| Progressive enhancement | works without JS where possible | JS-required for basic content |
