const PREC = {
  block: 1,
};

module.exports = grammar({
  name: 'bosatsu',

  externals: $ => [
    $._newline,
    $._indent,
    $._dedent,
  ],

  extras: $ => [
    /[ \t\r\f]+/,
    $.comment,
  ],

  supertypes: $ => [
    $.statement,
    $.atom,
  ],

  conflicts: $ => [
    [$.header_item, $.line_item],
  ],

  rules: {
    source_file: $ => repeat(choice(
      $.package_declaration,
      $.statement,
      $._newline,
      $._indent,
      $._dedent
    )),

    package_declaration: $ => seq(
      'package',
      field('name', $.package_name),
      $._newline
    ),

    package_name: $ => seq(
      $.package_segment,
      repeat(seq('/', $.package_segment))
    ),

    package_segment: _ => /[A-Za-z_][A-Za-z0-9_]*/,

    statement: $ => $.simple_statement,

    block_statement: $ => prec.right(PREC.block, seq(
      field('header', $.header_line),
      ':',
      $._newline,
      $._indent,
      repeat1(choice($.statement, $._newline)),
      $._dedent
    )),

    header_line: $ => repeat1($.header_item),

    header_item: $ => choice(
      $.atom,
      $.identifier,
      $.type_identifier,
      $.operator,
      $.keyword,
      $.unknown
    ),

    simple_statement: $ => seq(
      $.line_items,
      $._newline
    ),

    line_items: $ => repeat1($.line_item),

    line_item: $ => choice(
      $.atom,
      $.identifier,
      $.type_identifier,
      $.operator,
      $.keyword,
      $.punctuation,
      $.unknown
    ),

    atom: $ => choice(
      $.list,
      $.tuple,
      $.record,
      $.string,
      $.character,
      $.number,
      $.backticked_identifier
    ),

    tuple: $ => seq(
      '(',
      repeat(choice($.line_item, $._newline)),
      ')'
    ),

    list: $ => seq(
      '[',
      repeat(choice($.line_item, $._newline)),
      ']'
    ),

    record: $ => seq(
      '{',
      repeat(choice($.line_item, $._newline)),
      '}'
    ),

    keyword: _ => choice(
      'def',
      'enum',
      'struct',
      'external',
      'if',
      'elif',
      'else',
      'match',
      'case',
      'recur',
      'loop',
      'from',
      'import',
      'export',
      'operator'
    ),

    identifier: _ => /[a-z_][A-Za-z0-9_]*/,

    type_identifier: _ => /[A-Z][A-Za-z0-9_]*/,

    backticked_identifier: _ => /`[^`\n]+`/,

    number: _ => /-?(0[xX][0-9A-Fa-f_]+|0[oO][0-7_]+|0[bB][0-1_]+|[0-9][0-9_]*(\.[0-9_]+)?)/,

    string: _ => token(choice(
      /"([^"\\\n$]|\\.|\\\$\{[^}]*\})*"/,
      /'([^'\\\n]|\\.)*'/
    )),

    character: _ => token(seq('.', choice(
      seq("'", repeat(choice(/[^'\\\n]+/, /\\./)), "'"),
      seq('"', repeat(choice(/[^"\\\n]+/, /\\./)), '"')
    ))),

    operator: _ => /[!$%&*+\-./<=>?@\\^|~]+/,

    unknown: _ => /[^\s:\[\]\(\)\{\}]+/,

    punctuation: _ => choice(
      ':',
      ',',
      '.',
      ';',
      '='
    ),

    comment: _ => token(seq('#', /[^\n]*/)),
  },
});
