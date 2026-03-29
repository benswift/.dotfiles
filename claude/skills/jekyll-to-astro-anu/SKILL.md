---
name: jekyll-to-astro-anu
description:
  Ports Jekyll sites to Astro 6 using the astro-theme-anu theme package.
  Handles content collection migration, frontmatter transformation, kramdown
  syntax conversion, and image path updates. Use when porting, migrating, or
  converting a Jekyll site to Astro, or when working with astro-theme-anu
  consumers.
---

You are an expert at migrating Jekyll sites to Astro 6, specifically using the
`astro-theme-anu` theme package. You understand both Jekyll's conventions
(collections, Liquid templates, kramdown, YAML frontmatter) and Astro's content
collections, file-based routing, and component model.

## Theme reference

The `astro-theme-anu` package lives at `~/projects/astro-theme-anu/` and
provides:

### Layouts

- **BaseLayout.astro** --- `<html>`/`<head>`/`<body>`, nav, footer, dark/light
  theme toggle. Props: `title`, `description`, `name`, `links`, `wide`.

### Components

- **Nav.astro** --- sticky nav with site name, links, active detection, theme
  toggle
- **Footer.astro** --- copyright + acknowledgement of country
- **ThemeToggle.svelte** --- dark/light toggle with localStorage persistence
- **Card.astro** --- title, optional `href`, optional `image`, slot for body
- **CardGrid.astro** --- responsive grid, `columns` prop (2 or 3)
- **Callout.astro** --- `type` prop: `info` (default), `warning`, `error`

### Design tokens

CSS custom properties with `--at-` prefix (semantic) and `--anu-` prefix
(brand). Light mode default, dark mode via `[data-theme="dark"]`. Key tokens:
`--at-bg`, `--at-text`, `--at-accent` (gold), `--at-heading`,
`--at-content-width` (48rem).

### Installation

```js
// astro.config.mjs
import anuTheme from "astro-theme-anu";
export default defineConfig({
  integrations: [svelte(), anuTheme()],
  fonts: [
    { name: "Public Sans", cssVariable: "--font-public-sans", provider: fontProviders.google() },
    { name: "Roboto Mono", cssVariable: "--font-roboto-mono", provider: fontProviders.google(), weights: ["400", "700"] },
  ],
});
```

## Migration workflow

### 1. Scaffold the Astro project

Create a new Astro 6 project with:
- `astro-theme-anu` as a dependency (npm, or `file:` path for local dev)
- `@astrojs/svelte` for interactive islands
- `@astrojs/sitemap` for SEO
- `lightningcss` as CSS transformer
- Standard tooling: oxlint, oxfmt, Prettier (astro + svelte plugins), stylelint

### 2. Map Jekyll collections to Astro content collections

Jekyll collections (`_collectionname/`) become Astro content collections in
`src/content/collectionname/`. Key differences:

| Jekyll | Astro 6 |
|---|---|
| `_config.yml` collections | `src/content.config.ts` with `defineCollection` + `glob()` loader |
| `_posts/YYYY-MM-DD-slug.md` | `src/content/news/slug.md` with `date` in frontmatter |
| Collection defaults in `_config.yml` | Zod schema defaults in `content.config.ts` |
| `layout: detail` in frontmatter | Layout chosen in the page template, not frontmatter |

### 3. Write content collection schemas

Use Astro 6's `glob()` loader pattern:

```ts
import { defineCollection, z } from "astro:content";
import { glob } from "astro/loaders";

const myCollection = defineCollection({
  loader: glob({ pattern: "**/*.md", base: "src/content/myCollection" }),
  schema: z.object({
    title: z.string(),
    // use .nullish() for fields that may be empty in YAML (null)
    tagline: z.string().nullish(),
    image: z.string().nullish(),
    hidden: z.coerce.boolean().default(false),
  }).passthrough(),  // .passthrough() tolerates unexpected Jekyll frontmatter keys
});
```

**Critical:** Jekyll YAML often has empty values (e.g. `email:` with no value)
which parse as `null`. Use `.nullish()` instead of `.optional()` for string
fields, and `.coerce.boolean()` for boolean fields that may be strings.

### 4. Transform content files

Write a Python migration script for each collection. The script should:

```python
# Key transformations:
content = content.replace("/assets/images/", "/images/")
content = content.replace("assets/images/", "/images/")

# Remove Jekyll-specific frontmatter keys
for key in ['layout', 'show_related', 'show_toc', 'show_pagination',
            'show_collection', 'permalink', 'author']:
    content = re.sub(rf'^{key}:.*\n', '', content, flags=re.MULTILINE)

# Rename snake_case to camelCase
content = content.replace("display_order:", "displayOrder:")
content = content.replace("image_alt:", "imageAlt:")

# Strip kramdown attribute syntax
content = re.sub(r'\{:[^}]*\}', '', content)

# Convert Jekyll YouTube includes to HTML
content = re.sub(
    r'\{%\s*include\s+youtube\.html\s+id="([^"]+)"\s*%\}',
    r'<div class="video-embed"><iframe src="https://www.youtube-nocookie.com/embed/\1" ...></iframe></div>',
    content
)

# Remove remaining Jekyll includes
content = re.sub(r'\{%\s*include\s+[^%]*%\}', '', content)
```

### 5. Handle date-prefixed posts

Jekyll posts use `YYYY-MM-DD-slug.md` filenames. Extract the date:

```python
match = re.match(r'(\d{4}-\d{2}-\d{2})-(.+)\.md', basename)
date_str, slug = match.group(1), match.group(2)
# Add date to frontmatter, save as slug.md
```

### 6. Build pages

Standard pattern for collection index + detail pages:

```astro
---
// index page
import { getCollection } from "astro:content";
const items = await getCollection("myCollection");
const visible = items.filter(i => !i.data.hidden).sort(...);
---
<ContentLayout title="Section">
  <CardGrid columns={3}>
    {visible.map(item => (
      <Card title={item.data.title} href={`/section/${item.id}/`} image={item.data.image}>
        {item.data.tagline && <p>{item.data.tagline}</p>}
      </Card>
    ))}
  </CardGrid>
</ContentLayout>
```

```astro
---
// [slug].astro detail page
import { getCollection, render } from "astro:content";
export async function getStaticPaths() {
  const items = await getCollection("myCollection");
  return items.map(item => ({ params: { slug: item.id }, props: { item } }));
}
const { item } = Astro.props;
const { Content } = await render(item);
---
<ContentLayout title={item.data.title} tagline={item.data.tagline} image={item.data.image}>
  <Content />
</ContentLayout>
```

### 7. Pagination (for large collections like news)

```astro
---
// [...page].astro
export async function getStaticPaths({ paginate }) {
  const posts = await getCollection("news");
  const sorted = posts.sort((a, b) => b.data.date.getTime() - a.data.date.getTime());
  return paginate(sorted, { pageSize: 12 });
}
const { page } = Astro.props;
---
```

### 8. Copy images selectively

Copy from Jekyll `assets/images/` to Astro `public/images/`. Don't use
`src/assets/` for bulk migration --- `public/` is simpler and avoids build-time
processing for hundreds of images.

## Common pitfalls

- **Empty YAML values**: `email:` (no value) parses as `null`, not `""`. Use
  `.nullish()` in Zod schemas.
- **Question marks in filenames**: Jekyll allows `?` in post slugs; filesystem
  and glob loaders don't. Sanitise during migration.
- **Duplicate frontmatter keys**: Some Jekyll files have duplicate keys (e.g.
  `published:` twice). YAML parsers reject these --- deduplicate in migration.
- **Kramdown class syntax**: `{:.lead}`, `{:.info-box}` etc. are not valid
  markdown in Astro. Strip them or convert to HTML.
- **Jekyll includes**: `{% include ... %}` must be replaced. YouTube embeds
  become raw `<iframe>` HTML; collection listings become Astro page logic.
- **Internal links**: Jekyll uses `{% link _collection/file.md %}`. Convert to
  absolute paths like `/collection/slug/`.
- **`type: "content"` vs `glob()` loader**: Astro 6 uses `glob()` loaders, not
  the older `type: "content"` syntax.

## Site-specific components

Keep site-specific components in `src/components/`, not in the theme. Common
ones needed for a migration:

- **ContentLayout.astro** --- wraps BaseLayout with banner image + title +
  tagline pattern
- **Pagination.astro** --- prev/next nav for paginated collections
- **TagFilter.svelte** --- client-side filtering island for tagged collections

## IA simplification checklist

When porting, look for opportunities to simplify:

- Merge small collections (< 5 items) into larger ones or static pages
- Drop collections that are just stubs linking to external sites (e.g. jobs)
- Flatten nested subdirectories into a single collection with a `kind` field
- Merge historical event collections into news with a tag
- Convert tiny collections (< 5 items) into YAML data files instead
