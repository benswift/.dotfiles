# Frontend preference checklist

## Structural

| Check | Preference | Anti-pattern |
|---|---|---|
| Bundler | Vite (standalone projects) | webpack, parcel, rollup (standalone) |
| Bundler (Phoenix) | esbuild (expected default) | webpack in Phoenix projects |
| Linter | oxlint | ESLint |
| Formatter | oxfmt | Prettier (for JS/TS; Prettier is still fine for markdown/CSS) |

## Soft

| Check | Preference | Anti-pattern |
|---|---|---|
| HTML | semantic elements (nav, article, section, aside) | div soup |
| Accessibility | ARIA roles, labels, keyboard navigation, WCAG | missing alt text, non-interactive divs with click handlers |
| CSS framework | Tailwind (when present) | Bootstrap, Material UI CSS |
| Layout | CSS Grid (primary) and Flexbox (1D) | float-based layouts, excessive breakpoint classes |
| Responsive design | fluid layouts, container queries | breakpoint-only responsive design |
| CSS theming | custom properties | inline styles, hardcoded values |
| CSS animations | CSS transitions/animations over JS | jQuery animations, JS where CSS suffices |
| JS animations | Motion library (when CSS can't handle it) | GSAP, anime.js, jQuery |
| JavaScript | ES6+ (arrow functions, destructuring, async/await) | var, callbacks, CommonJS require |
| Module system | ES modules (`import`/`export`, `type="module"`) | CommonJS, global scripts |
| Code style | functional (map, filter, reduce, pure functions) | imperative loops, mutation-heavy |
| Oxc toolchain | oxlint for linting, oxfmt for formatting, oxc transformer/resolver/minifier where applicable | separate ESLint + Prettier + Babel + Terser toolchain |
| E2E testing | Playwright | Cypress, Selenium |
| Unit testing | Vitest | Jest |
| Progressive enhancement | works without JS where possible | JS-required for basic content |
