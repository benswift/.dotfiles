; Comments
(comment) @comment

; Keywords
[
  "set"
  "unset"
  "reset"
  "toggle"
  "source"
  "macro"
  "bind"
  "unbind"
  "exec"
  "push"
  "color"
  "uncolor"
  "mono"
  "unmono"
  "alias"
  "unalias"
  "mailboxes"
  "unmailboxes"
  "subscribe"
  "unsubscribe"
  "lists"
  "unlists"
  "hdr_order"
  "unhdr_order"
  "auto_view"
  "unauto_view"
  "alternative_order"
  "unalternative_order"
  "ignore"
  "unignore"
  "score"
  "unscore"
  "folder-hook"
  "send-hook"
  "send2-hook"
  "reply-hook"
  "account-hook"
  "pgp-hook"
  "crypt-hook"
  "message-hook"
  "save-hook"
  "fcc-hook"
  "fcc-save-hook"
  "charset-hook"
  "iconv-hook"
  "mbox-hook"
  "tag-transforms"
  "tag-formats"
] @keyword

; String literals
(string) @string
(quoted_string) @string
(backtick_expansion) @string.special

; Variables
(variable) @variable
(my_var) @variable
(shell_command) @function.call

; Numbers
(number) @number

; Boolean values
[
  "yes"
  "no"
  "ask-yes"
  "ask-no"
] @constant.builtin

; Operators
[
  "="
  "+="
  "-="
  "?="
] @operator

; Special characters
[
  "+"
  "-"
  "*"
  "/"
  "%"
  "!"
  "~"
  "^"
] @punctuation.special

; Delimiters
[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

[
  ","
  ";"
] @punctuation.delimiter