module.exports = grammar({
  name: 'mutt_compose',

  rules: {
    source_file: $ => seq(
      $.headers,
      optional($.body),
      optional($.quoted_section)
    ),

    headers: $ => repeat1($.header),

    header: $ => seq(
      field('name', $.header_name),
      ':',
      optional(field('value', $.header_value)),
      '\n'
    ),

    header_name: $ => choice(
      'From',
      'To',
      'Cc',
      'Bcc',
      'Subject',
      'Message-ID',
      'Reply-To',
      'In-Reply-To',
      'Date',
      'References',
      'X-Mailer',
      /[A-Z][A-Za-z-]*/
    ),

    header_value: $ => /[^\n]+/,

    body: $ => seq(
      '\n',
      repeat($.body_line)
    ),

    body_line: $ => seq(
      choice(
        $.regular_line,
        $.markdown_heading,
        $.markdown_list,
        $.markdown_code_block,
        $.markdown_link,
        $.markdown_emphasis
      ),
      '\n'
    ),

    regular_line: $ => /[^\n>].*/,

    markdown_heading: $ => /#{1,6}\s+.*/,

    markdown_list: $ => /[\*\-\+]\s+.*/,

    markdown_code_block: $ => seq(
      '```',
      optional($.language_name),
      '\n',
      repeat($.code_line),
      '```'
    ),

    language_name: $ => /[a-z]+/,

    code_line: $ => /[^`\n][^\n]*/,

    markdown_link: $ => /\[[^\]]+\]\([^\)]+\)/,

    markdown_emphasis: $ => choice(
      /\*[^*]+\*/,     // *italic*
      /\*\*[^*]+\*\*/,  // **bold**
      /_[^_]+_/,        // _italic_
      /__[^_]+__/       // __bold__
    ),

    quoted_section: $ => seq(
      $.quote_header,
      repeat1($.quoted_line)
    ),

    quote_header: $ => /On .+ wrote:\n/,

    quoted_line: $ => seq(
      repeat1('>'),
      optional(/[^\n]*/),
      '\n'
    )
  }
});