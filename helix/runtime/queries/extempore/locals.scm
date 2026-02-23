; locals.scm --- Extempore scope and variable tracking

; Scope-creating forms
(list
  .
  (symbol) @_keyword
  (#match? @_keyword "^(lambda|let|let\\*|letrec|letz|do|dolet|dotimes|doloop|memzone)$")) @local.scope

; bind-func creates a scope
(list
  .
  (symbol) @_keyword
  (#match? @_keyword "^(bind-func|definec)$")) @local.scope

; define creates a definition
(list
  .
  (symbol) @_keyword
  .
  (symbol) @local.definition
  (#match? @_keyword "^(define|bind-val)$"))

; bind-func name (symbol or typed_identifier)
(list
  .
  (symbol) @_keyword
  .
  (symbol) @local.definition
  (#match? @_keyword "^(bind-func|definec|bind-macro|define-macro)$"))

(list
  .
  (symbol) @_keyword
  .
  (typed_identifier) @local.definition
  (#match? @_keyword "^(bind-func|definec)$"))

; bind-data name (generic_identifier)
(list
  .
  (symbol) @_keyword
  .
  (generic_identifier) @local.definition
  (#match? @_keyword "^bind-data$"))

; Symbol references
(symbol) @local.reference
