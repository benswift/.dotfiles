; Commands (keywords)
(command) @keyword

; Comments
(comment) @comment

; Strings
(string) @string
(shell) @string.special

; Regular expressions
(regex) @string.regexp

; Numbers
(int) @number

; Configuration options
(option) @variable.parameter

; Quadoptions (yes/no/ask-yes/ask-no)
(quadoption) @constant.builtin

; Colors
(color) @constant
(foreground) @constant
(background) @constant

; Attributes (bold, underline, etc.)
(attribute) @attribute

; Color objects
(object) @type
(composeobject) @type

; Keybinding maps
(map) @type.enum

; Functions
(function) @function

; Environment variables
(env_var) @variable.builtin

; Keys and key names
(key_name) @string.special.symbol
(escape) @string.escape

; Hook types
(hook_type) @type

; MIME types
(mime_type) @type
(sub_mime_type) @type

; Paths and files
(path) @string.special.path
(directory) @string.special.path
(mailbox) @string.special.path

; Command line options (-group, -rx, etc.)
(command_line_option) @attribute

; Addresses
(address) @string.special

; Descriptions
(description) @string

; Group names
(group_name) @variable

; Symbols (for ifdef/ifndef)
(symbol) @variable

; Operators
["=" "+=" "-="] @operator
["+" "-"] @operator
["?" "&" "!" "*"] @punctuation.special

; Punctuation
["<" ">"] @punctuation.bracket
["'" "\"" "`"] @punctuation.delimiter
[","] @punctuation.delimiter
