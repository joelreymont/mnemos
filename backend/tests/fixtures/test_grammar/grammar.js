module.exports = grammar({
  name: 'test_json',

  rules: {
    document: $ => optional($._value),

    _value: $ => choice(
      $.object,
      $.array,
      $.number,
      $.string,
      $.true,
      $.false,
      $.null
    ),

    object: $ => seq(
      '{',
      optional(seq(
        $.pair,
        repeat(seq(',', $.pair))
      )),
      '}'
    ),

    pair: $ => seq(
      field('key', $.string),
      ':',
      field('value', $._value)
    ),

    array: $ => seq(
      '[',
      optional(seq(
        $._value,
        repeat(seq(',', $._value))
      )),
      ']'
    ),

    string: $ => seq(
      '"',
      repeat(choice(
        /[^"\\]/,
        seq('\\', /./),
      )),
      '"'
    ),

    number: $ => /[0-9]+(\.[0-9]+)?/,

    true: $ => 'true',
    false: $ => 'false',
    null: $ => 'null',
  }
});
