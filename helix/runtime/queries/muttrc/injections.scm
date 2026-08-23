((regex) @injection.content
  (#set! injection.language "regex"))

((shell) @injection.content
  (#set! injection.language "bash"))

(set_directive
  (command)
  (option) @_option
  content: (string) @injection.content
  (#set! injection.language "expando")
  (#match? @_option "_format$"))

(set_directive
  (command)
  (option) @_option
  content: (string) @injection.content
  (#set! injection.language "bash")
  (#match? @_option "_command"))

(set_directive
  (command)
  (option) @_option
  content: (string) @_content @injection.content
  (#set! injection.language "bash")
  (#eq? @_option "signature")
  (#match? @_content "\\|$"))

(comment
  (body) @injection.content
  (#set! injection.language "comment"))
