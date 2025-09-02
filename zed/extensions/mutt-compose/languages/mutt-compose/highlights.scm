; Email headers
(header_name) @keyword
(header_value) @string

; Specific important headers
((header_name) @keyword.directive
  (#match? @keyword.directive "^(From|To|Cc|Bcc|Subject)$"))

; Message ID and references
((header_name) @type
  (#match? @type "^(Message-ID|In-Reply-To|References)$"))

; Date header
((header_name) @attribute
  (#eq? @attribute "Date"))

; Body content
(body_line) @text
(regular_line) @text

; Markdown elements in body
(markdown_heading) @markup.heading
(markdown_list) @markup.list
(markdown_emphasis) @markup.italic
(markdown_link) @markup.link

; Code blocks
(markdown_code_block) @markup.raw
(language_name) @type
(code_line) @markup.raw.inline

; Quoted email sections
(quote_header) @comment
(quoted_line) @comment

; Quoted line markers
((quoted_line) @punctuation.special
  (#match? @punctuation.special "^>"))

; Separators
":" @punctuation.delimiter