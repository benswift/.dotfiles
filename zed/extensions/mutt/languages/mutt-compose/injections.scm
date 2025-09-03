; Inject markdown (GFM) grammar into the body
((body) @injection.content
  (#set! injection.language "markdown"))