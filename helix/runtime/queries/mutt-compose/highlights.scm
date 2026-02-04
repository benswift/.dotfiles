; Headers - highlight key and value separately using field names
(header
  key: (header_key) @type
  ":" @punctuation.delimiter
  value: (header_value) @string)

; Body - styled as text, with markdown injection handling formatting
(body) @text
