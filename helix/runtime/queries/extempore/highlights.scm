; highlights.scm --- Extempore syntax highlighting
;
; Pattern ordering: later patterns take priority over earlier ones.
; The catch-all @function comes first so specific patterns override it.

; Default: first element in a list is a function call (lowest priority)
(list
  .
  (symbol) @function)

; xtlang type expressions
(xtlang_type) @type
(typed_identifier (type_annotation) @type)
(generic_identifier) @type

; Literals
(number) @constant.numeric
(string) @string
(escape_sequence) @string.escape
(boolean) @constant.builtin
(character) @constant.builtin

; Comments
(comment) @comment
(block_comment) @comment

; Punctuation
["(" ")"] @punctuation.bracket
["#("] @punctuation.bracket

; Quote forms
(quote "'") @punctuation.special
(quasiquote "`") @punctuation.special
(unquote ",") @punctuation.special
(unquote_splicing ",@") @punctuation.special

; Bare type names (xtlang primitives)
((symbol) @type.builtin
 (#match? @type.builtin "^(i1|i8|i16|i32|i64|f|f32|f64|d|float|double|void)$"))

; Operators
(list
  .
  (symbol) @operator
  (#match? @operator "^(\\+|-|\\*|/|=|<|>|<=|>=|!=|%|<<|>>|bitwise-and|bitwise-or|bitwise-xor|bitwise-not)$"))

; Special xtlang operators
(list
  .
  (symbol) @keyword.operator
  (#match? @keyword.operator "^(\\$|\\$\\$)$"))

; Builtins: Scheme
(list
  .
  (symbol) @function.builtin
  (#match? @function.builtin "^(car|cdr|cons|list|append|reverse|length|map|for-each|filter|fold|foldl|foldr|apply|eval|null\\?|pair\\?|number\\?|integer\\?|real\\?|string\\?|symbol\\?|procedure\\?|boolean\\?|char\\?|vector\\?|port\\?|eq\\?|eqv\\?|equal\\?|not|zero\\?|positive\\?|negative\\?|even\\?|odd\\?|abs|min|max|modulo|remainder|quotient|gcd|lcm|floor|ceiling|truncate|round|exact->inexact|inexact->exact|number->string|string->number|string->symbol|symbol->string|string->list|list->string|vector->list|list->vector|char->integer|integer->char|string-ref|string-set!|string-length|string-append|string-copy|substring|string-upcase|string-downcase|make-string|make-vector|vector-ref|vector-set!|vector-length|vector-fill!|assoc|assq|assv|member|memq|memv|sort|display|write|newline|println|print|printf|printout|read|error|format|open-input-file|open-output-file|close-input-port|close-output-port|read-char|write-char|peek-char|char-ready\\?|load|exit|quit|caar|cadr|cdar|cddr|caaar|caadr|cadar|caddr|cdaar|cdadr|cddar|cdddr)$"))

; Builtins: xtlang memory and data
(list
  .
  (symbol) @function.builtin
  (#match? @function.builtin "^(alloc|salloc|halloc|zalloc|stack-alloc|heap-alloc|zone-alloc|free|tref|tset!|tfill!|aref|aset!|aref-ptr|afill!|pref|pset!|pref-ptr|pfill!|vref|vset!|vfill!|make-array|array-ref|array-set!|null\\?|type-of|size-of|void|now|random|imp_rand)$"))

; Keywords: xtlang control flow and memory
(list
  .
  (symbol) @keyword
  (#match? @keyword "^(dotimes|doloop|dolet|memzone|callback|cast|convert|bitcast|call-as-xtlang|sync|spawn|async|catch|call/cc|call-with-current-continuation)$"))

; Keywords: Scheme special forms
(list
  .
  (symbol) @keyword
  (#match? @keyword "^(define|define-macro|macro|lambda|let|let\\*|letrec|letz|if|cond|case|begin|beginz|do|when|unless|set!|and|or|else|delay|receive|quasiquote|unquote|unquote-splicing)$"))

; Keywords: xtlang forms
(list
  .
  (symbol) @keyword
  (#match? @keyword "^(bind-func|bind-type|bind-data|bind-val|bind-lib|bind-dylib|bind-alias|bind-poly|bind-macro|bind-instrument|bind-sampler|definec|definec:dsp)$"))

; Scheme definition forms: (define name value)
(list
  .
  (symbol) @keyword
  .
  (symbol) @variable
  (#match? @keyword "^define$"))

; Scheme definition forms: (define (name args...) body)
(list
  .
  (symbol) @keyword
  .
  (list
    .
    (symbol) @function)
  (#match? @keyword "^define$"))

(list
  .
  (symbol) @keyword
  .
  (list
    .
    (symbol) @function)
  (#match? @keyword "^define-macro$"))

; xtlang binding forms: highlight the keyword and the name
(list
  .
  (symbol) @keyword
  .
  (symbol) @function
  (#match? @keyword "^bind-func$"))

(list
  .
  (symbol) @keyword
  .
  (typed_identifier
    (symbol) @function)
  (#match? @keyword "^bind-func$"))

(list
  .
  (symbol) @keyword
  .
  (symbol) @type
  (#match? @keyword "^(bind-type|bind-data|bind-alias)$"))

(list
  .
  (symbol) @keyword
  .
  (generic_identifier) @type
  (#match? @keyword "^bind-data$"))

(list
  .
  (symbol) @keyword
  .
  (symbol) @variable
  (#match? @keyword "^bind-val$"))

(list
  .
  (symbol) @keyword
  .
  (symbol) @function
  (#match? @keyword "^(bind-lib|bind-poly|bind-macro)$"))
