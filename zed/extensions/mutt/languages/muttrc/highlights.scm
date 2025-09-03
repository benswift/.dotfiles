; Comments
(comment) @comment

; String literals
(string) @string
(shell) @string.special

; Commands (the actual command word like "set", "bind", etc.)
(command) @keyword

; Options (variable names)
(option) @variable

; Keys
(key) @constant

; Objects
(object) @type

; Colors
(color) @constant.builtin

; Functions
(function) @function

; Operators and punctuation
"=" @operator