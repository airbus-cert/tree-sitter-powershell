const PREC = {
  KEYWORD: 1,
  UNARY: 2,
  CAST : 3,
  ELEMENT_ACCESS : 4,
  EMPTY : 5
}

module.exports = grammar({
  name: 'powershell',

  externals: $ => [
    $._statement_terminator
  ],

  extras: $ => [
    $.comment,
    /\s/,
    /`\n/,
    /`\r\n/,
    /[\uFEFF\u2060\u200B\u00A0]/
  ],

  conflicts: $ => [
    [$._literal, $.member_name],
    [$.class_property_definition, $.attribute],
    [$.class_method_definition, $.attribute],
    [$.expandable_string_literal],
    [$.path_command_name, $._value],
    [$._expression, $.array_literal_expression]
  ],

  rules: {
    program: $ => seq(
      optional($.param_block),
      $.statement_list
    ),

    // Comments
    comment: $ => token(
      choice(
        /#[^\r\n]*/,
        seq(
          "<#",
          repeat(
            choice(
              /[^#`]+/,
              /#+[^>#]/,
              /`.{1}|`\r?\n/
            )
          ),
          /#+>/
        )
      )
    ),

    // Literal
    _literal: $ => choice(
      $.integer_literal,
      $.string_literal,
      $.real_literal
    ),

    // Integer Literals
    integer_literal: $ => choice(
      $.decimal_integer_literal,
      $.hexadecimal_integer_literal
    ),

    decimal_integer_literal: _ => token(seq(
      /[0-9]+/, optional(choice("l", "d")), optional(choice("kb", "mb", "gb", "tb", "pb"))
    )),

    hexadecimal_integer_literal: _ => token(seq(
      "0x", /[0-9a-fA-F]+/, optional("l"), optional(choice("kb", "mb", "gb", "tb", "pb"))
    )),

    // Real Literals
    real_literal: _ => token(choice(
      seq(/[0-9]+\.[0-9]+/, optional(token(seq("e", optional(choice("+", "-")), /[0-9]+/))), optional(choice("kb", "mb", "gb", "tb", "pb"))),
      seq(/\.[0-9]+/, optional(token(seq("e", optional(choice("+", "-")), /[0-9]+/))), optional(choice("kb", "mb", "gb", "tb", "pb"))),
      seq(/[0-9]+/, token(seq("e", optional(choice("+", "-")), /[0-9]+/)), optional(choice("kb", "mb", "gb", "tb", "pb")))
    )),

    // String literal
    string_literal: $ => choice(
      $.expandable_string_literal,
      $.verbatim_string_characters,
      $.expandable_here_string_literal,
      $.verbatim_here_string_characters
    ),

    expandable_string_literal: $ => seq(
      /\"(\s*\#*)*/,  // this is a trick to avoid tree-sitter allowing comment between tokens, as string should be tokenize but powershell allow subexpression inside it...
      repeat(
        choice(
          token.immediate(/[^\$\"`]+/),
          $.variable,
          $.sub_expression,
          token.immediate(/\$(`.{1}|`\r?\n|[\s\\])/),
          token.immediate(/`.{1}|`\r?\n/),
          token.immediate("\"\""),
          token.immediate("$"),
        )
      ),
      repeat(token.immediate("$")),
      token.immediate(/(\s*\#*)*\"/)
    ),

    expandable_here_string_literal: $ => seq(
      /@\" *\r?\n/,
      repeat(
        choice(
          token.immediate(/[^\$\r\n`]+/),
          $.variable,
          $.sub_expression,
          token.immediate(/(\r?\n)+[^\"\r\n]/),
          token.immediate(/(\r?\n)+\"[^@]/),
          token.immediate("$"),
          token.immediate(/`.{1}|`\r?\n/)
        )
      ),
      token.immediate(/(\r?\n)+\"@/)
    ),

    verbatim_string_characters: $ => token(seq(
      "'",
      repeat(
        choice(
          /[^']+/,
          "''"
        )
      ),
      "'"
    )),

    verbatim_here_string_characters: $ => token(
      seq(
        /@\'\s*\r?\n/,
        repeat(
          choice(
            /[^\r\n]/,
            /(\r?\n)+[^\'\r\n]/,
            /\r?\n\'[^@]/,
          )
        ),
        /(\r?\n)+\'@/
      )
    ),

    // Simple names
    simple_name: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,

    // Type names
    type_identifier: $ => /[a-zA-Z0-9_]+/,

    type_name: $ => choice(
      $.type_identifier,
      seq($.type_name, ".", $.type_identifier )
    ),

    array_type_name: $ => seq($.type_name, "["),
    generic_type_name: $ => seq($.type_name, "["),

    // Operators and punctuators
    assignement_operator: _ => choice(
      "=", "!=", "+=", "*=", "/=", "%="
    ),

    file_redirection_operator: _ => choice(
      ">",  ">>",  "2>",  "2>>",  "3>",  "3>>",  "4>",  "4>>",
      "5>",  "5>>",  "6>",  "6>>",  "*>",  "*>>",  "<"
    ),

    merging_redirection_operator: _ => choice(
      "*>&1",  "2>&1",  "3>&1",  "4>&1",  "5>&1",  "6>&1",
      "*>&2",  "1>&2",  "3>&2",  "4>&2",  "5>&2",  "6>&2"
    ),

    comparison_operator: _ => choice(
      reservedWord("-as"),reservedWord("-ccontains"),reservedWord("-ceq"),
      reservedWord("-cge"),reservedWord("-cgt"),reservedWord("-cle"),
      reservedWord("-clike"),reservedWord("-clt"),reservedWord("-cmatch"),
      reservedWord("-cne"),reservedWord("-cnotcontains"),reservedWord("-cnotlike"),
      reservedWord("-cnotmatch"),reservedWord("-contains"),reservedWord("-creplace"),
      reservedWord("-csplit"),reservedWord("-eq") ,reservedWord("-ge"),
      reservedWord("-gt"), reservedWord("-icontains"),reservedWord("-ieq"),
      reservedWord("-ige"),reservedWord("-igt"), reservedWord("-ile"),
      reservedWord("-ilike"),reservedWord("-ilt"),reservedWord("-imatch"),
      reservedWord("-in"),reservedWord("-ine"),reservedWord("-inotcontains"),
      reservedWord("-inotlike"),reservedWord("-inotmatch"),reservedWord("-ireplace"),
      reservedWord("-is"),reservedWord("-isnot"),reservedWord("-isplit"),
      reservedWord("-join"),reservedWord("-le"),reservedWord("-like"),
      reservedWord("-lt"),reservedWord("-match"),reservedWord("-ne"),
      reservedWord("-notcontains"),reservedWord("-notin"),reservedWord("-notlike"),
      reservedWord("-notmatch"),reservedWord("-replace"),reservedWord("-shl"),
      reservedWord("-shr"),reservedWord("-split")
    ),

    format_operator: _ => reservedWord("-f"),

    // Variables
    variable: $ => choice(
      '$$',
      '$^',
      '$?',
      '$_',
      token(seq('$', optional(seq(choice(reservedWord("global:"), reservedWord("local:"), reservedWord("private:"), reservedWord("script:"), reservedWord("using:"), reservedWord("workflow:"), /[a-zA-Z0-9_]+/), ":")), /[a-zA-Z0-9_]+|\?/)),
      token(seq('@', optional(seq(choice(reservedWord("global:"), reservedWord("local:"), reservedWord("private:"), reservedWord("script:"), reservedWord("using:"), reservedWord("workflow:"), /[a-zA-Z0-9_]+/), ":")), /[a-zA-Z0-9_]+|\?/)),
      $.braced_variable
    ),

    braced_variable: _ => /\$\{[^}]+\}/,

    // Commands
    generic_token: _ => token(
      /[^\(\)\$\"\'\-\{\}@\|\[`\s][^\s\(\)\}\|;,]*/,
    ),

    _command_token: _ => token(/[^\(\)\{\}\s;]+/),

    // Parameters
    command_parameter: _ => token(
      choice(
        /-+[a-zA-Z_?\-`]+/,
        "--"
      )
    ),

    _verbatim_command_argument_chars: _ => repeat1(
      choice(
        /"[^"]*"/,
        /&[^&]*/,
        /[^\|\r\n]+/
      )
    ),

    // Grammar

    // Statements
    script_block: $ => choice(
      field("script_block_body", $.script_block_body),
      seq(seq($.param_block, $._statement_terminator, repeat(";")), field("script_block_body", optional($.script_block_body)))
    ),

    param_block: $ => seq(
      optional($.attribute_list), reservedWord("param"), "(", optional($.parameter_list), ")"
    ),

    parameter_list: $ => seq(
      $.script_parameter,
      repeat(seq(",", $.script_parameter))
    ),

    script_parameter: $ => seq(
      optional($.attribute_list), $.variable, optional($.script_parameter_default)
    ),

    script_parameter_default: $ => seq(
      "=", $._expression
    ),

    script_block_body: $ => choice(
      field("named_block_list", $.named_block_list),
      field("statement_list", $.statement_list)
    ),

    named_block_list: $ => repeat1(
      $.named_block
    ),

    named_block: $ => seq(
      $.block_name, $.statement_block
    ),

    block_name: _ => choice(
      reservedWord("dynamicparam"),
      reservedWord("begin"),
      reservedWord("process"),
      reservedWord("end")
    ),

    statement_block: $ => seq(
      "{", field("statement_list", optional($.statement_list)), "}"
    ),

    statement_list: $ => repeat1($._statement),

    _statement: $ => prec.right(choice(
      $.if_statement,
      seq(optional($.label), $._labeled_statement),
      $.function_statement,
      $.class_statement,
      $.enum_statement,
      seq($.flow_control_statement, $._statement_terminator),
      $.trap_statement,
      $.try_statement,
      $.data_statement,
      $.inlinescript_statement,
      $.parallel_statement,
      $.sequence_statement,
      seq($.pipeline, $._statement_terminator),
      $.empty_statement
    )),

    empty_statement: _ => prec(PREC.EMPTY, ";"),

    if_statement: $ => prec.left(seq(
      reservedWord("if"), "(", field("condition", $.pipeline), ")", $.statement_block, field("elseif_clauses", optional($.elseif_clauses)), field("else_clause", optional($.else_clause))
    )),

    elseif_clauses: $ => prec.left(repeat1($.elseif_clause)),

    elseif_clause: $ => seq(
      reservedWord("elseif"), "(", field("condition", $.pipeline), ")", $.statement_block
    ),

    else_clause: $ => seq(reservedWord("else"), $.statement_block),

    _labeled_statement: $ => choice(
      $.switch_statement,
      $.foreach_statement,
      $.for_statement,
      $.while_statement,
      $.do_statement
    ),

    switch_statement: $ => seq(
      reservedWord("switch"), optional($.switch_parameters), $.switch_condition, $.switch_body
    ),

    switch_parameters: $ => repeat1($.switch_parameter),

    switch_parameter: _ => choice(
      reservedWord("-regex"),
      reservedWord("-wildcard"),
      reservedWord("-exact"),
      reservedWord("-casesensitive"),
      reservedWord("-parallel")
    ),

    switch_condition: $ => choice(
      seq("(", $.pipeline, ")"),
      seq(reservedWord("-file"), $.switch_filename)
    ),

    switch_filename: $ => choice(
      $._command_token,
      $._primary_expression
    ),

    switch_body: $ => seq("{", optional($.switch_clauses), "}"),

    switch_clauses: $ => repeat1($.switch_clause),

    switch_clause: $ => seq($.switch_clause_condition, $.statement_block, $._statement_terminator, repeat(";")),

    switch_clause_condition: $ => choice(
      $._command_token,
      $._primary_expression
    ),

    foreach_statement: $ => seq(
      reservedWord("foreach"), optional($.foreach_parameter), "(", $.variable, reservedWord("in"), $.pipeline, ")", $.statement_block
    ),

    foreach_parameter: _ => reservedWord("-parallel"),

    for_statement: $ => seq(
      reservedWord("for"), "(",
        optional(
          seq(optional(seq(field("for_initializer", $.for_initializer), $._statement_terminator)),
            optional(
              seq(choice(";", token.immediate(/\r?\n/)), optional(seq(field("for_condition", $.for_condition), $._statement_terminator)),
                optional(
                  seq(choice(";", token.immediate(/\r?\n/)), optional(seq(field("for_iterator", $.for_iterator), $._statement_terminator)))
                )
              )
            )
          ),
        ),
        ")", $.statement_block
    ),

    for_initializer: $ => $.pipeline,

    for_condition: $ => $.pipeline,

    for_iterator: $ => $.pipeline,

    while_statement: $ => seq(
      reservedWord("while"), "(", field("condition", $.while_condition), ")", $.statement_block
    ),

    while_condition: $ => $.pipeline,

    do_statement: $ => seq(
      reservedWord("do"), $.statement_block, choice(reservedWord("while"), reservedWord("until")), "(", field("condition", $.while_condition), ")"
    ),

    function_statement: $ => seq(
      choice(
        reservedWord("function"),
        reservedWord("filter"),
        reservedWord("workflow")
      ),
      $.function_name,
      optional($.function_parameter_declaration),
      "{", optional($.script_block), "}"
    ),

    function_name: $ => $._command_token,

    function_parameter_declaration: $ => seq(
      "(", optional($.parameter_list), ")"
    ),

    flow_control_statement: $ => choice(
      seq(reservedWord("break"), optional($.label_expression)),
      seq(reservedWord("continue"), optional($.label_expression)),
      seq(reservedWord("throw"), optional($.pipeline)),
      seq(reservedWord("return"), optional($.pipeline)),
      seq(reservedWord("exit"), optional($.pipeline))
    ),

    label: _ => token(seq(":", /[a-zA-Z_][a-zA-Z0-9_]*/)),

    label_expression: $ => choice(
      $.label,
      $._unary_expression
    ),

    trap_statement: $ => seq(
      reservedWord("trap"), optional($.type_literal), $.statement_block
    ),

    try_statement: $ => seq(
      reservedWord("try"),
      $.statement_block,
      choice(
        seq($.catch_clauses, optional($.finally_clause)),
        optional($.finally_clause)
      )
    ),

    catch_clauses: $ => repeat1($.catch_clause),

    catch_clause: $ => seq(
      reservedWord("catch"), optional($.catch_type_list), $.statement_block
    ),

    catch_type_list: $ => seq(
      $.type_literal,
      repeat(
        seq(",", $.type_literal)
      )
    ),

    finally_clause: $ => seq(
      reservedWord("finally"), $.statement_block
    ),

    data_statement: $ => seq(
      reservedWord("data"), optional($.data_name), optional($.data_commands_allowed), $.statement_block
    ),

    data_name: $ => $.simple_name,

    data_commands_allowed: $ => seq(
      reservedWord("-supportedcommand"), $.data_commands_list
    ),

    data_commands_list: $ => seq(
      $.data_command,
      repeat(seq(",", $.data_command))
    ),

    data_command: $ => $.command_name_expr,

    inlinescript_statement: $ => seq(
      reservedWord("inlinescript"), $.statement_block
    ),

    parallel_statement: $ => seq(
      reservedWord("parallel"), $.statement_block
    ),

    sequence_statement: $ => seq(
      reservedWord("sequence"), $.statement_block
    ),

    pipeline: $ => choice(
      $.assignment_expression,
      seq($._expression, optional($.redirections), optional($._pipeline_tail)),
      seq($.command, optional($.verbatim_command_argument), optional($._pipeline_tail))
    ),

    // Distinct a normal expression to a left assignement expession
    left_assignment_expression: $ => $._expression,

    assignment_expression: $ => seq(
      $.left_assignment_expression, $.assignement_operator, field("value", $._statement)
    ),

    _pipeline_tail: $ => repeat1(
      seq('|', $.command)
    ),

    command: $ => choice(
      seq(field("command_name", $.command_name), field("command_elements", optional($.command_elements))),
      $.foreach_command,
      seq($.command_invokation_operator, /*optional($.command_module),*/ field("command_name", $.command_name_expr), field("command_elements", optional($.command_elements)))
    ),

    command_invokation_operator: _ => choice(
      ".",
      "&"
    ),

    // This rule is ignored as it does not appear as a rule
    //command_module: $ => $.primary_expression,

    _expandable_string_literal_immediate: $ => seq(
      repeat(
        choice(
          /[^\$"`]+/,
          $.variable,
          /\$`(.{1}|`\r?\n)/,
          /`.{1}|`\r?\n/,
          "\"\"",
          $.sub_expression
        )
      ),
      repeat("$"),
      "\""
    ),

    command_name: $ => prec.right(seq(
      /[^\{\}\(\);,\|\&`"'\s\r\n\[\]\+\-\*\/\$@<\!%]+/,
      repeat(
        choice(
          token.immediate(/[^\{\}\(\);,\|\&"'\s\r\n]+/),
          seq(token.immediate("\""), $._expandable_string_literal_immediate),
          token.immediate("\"\""),
          token.immediate("''")
        )
      )
    )),

    path_command_name_token: $ => /[0-9a-zA-Z_?\-\.\\]+/,

    // Use to parse command path
    path_command_name: $ => prec.right(
      repeat1(
        choice(
          $.path_command_name_token,
          $.variable
        )
      )
    ),

    command_name_expr: $ => choice(
      $.command_name,
      $.path_command_name,
      $._primary_expression
    ),

    command_elements: $ => prec.right(repeat1($._command_element)),

    _command_element: $ => choice(
      $.command_parameter,
      seq($._command_argument, optional($.argument_list)),
      $.redirection,
      $.stop_parsing
    ),

    // Stop parsing is a token that end the parsing of command line
    stop_parsing: _ => /--%[^\r\n]*/,

    // Generic token is hard to manage
    // So a definition is that a generic token must have to begin by one or more space char
    command_argument_sep: _ => prec.right(choice(repeat1(" "), ":")),

    // Adapt the grammar to have same behavior
    _command_argument: $ => prec.right(choice(
      seq($.command_argument_sep, optional($.generic_token)),
      seq($.command_argument_sep, $.array_literal_expression)
    )),

    foreach_command: $ => seq(choice("%", reservedWord("foreach-object")), field("command_elements", repeat1($.script_block_expression))),

    verbatim_command_argument: $ => seq(
      "--%", $._verbatim_command_argument_chars
    ),

    redirections: $ => repeat1($.redirection),

    redirection: $ => choice(
      $.merging_redirection_operator,
      seq($.file_redirection_operator, $.redirected_file_name)
    ),

    redirected_file_name: $ => choice(
      $._command_argument,
      $._primary_expression
    ),

    // Class
    class_attribute : _ => choice(reservedWord("hidden"), reservedWord("static")),

    class_property_definition: $ => seq(
      optional($.attribute),
      repeat($.class_attribute),
      optional($.type_literal),
      $.variable,
      optional(
        seq(
          "=",
          $._expression
        )
      )
    ),

    class_method_parameter: $ => seq(
      optional($.type_literal),
      $.variable
    ),

    class_method_parameter_list: $ => seq(
      $.class_method_parameter,
      repeat(seq(",", $.class_method_parameter))
    ),

    class_method_definition: $ => seq(
      optional($.attribute),
      repeat($.class_attribute),
      optional($.type_literal),
      $.simple_name,
      "(", optional($.class_method_parameter_list), ")",
      "{", optional($.script_block), "}"
    ),

    class_statement: $ => seq(
      reservedWord("class"), $.simple_name, optional(seq(":", $.simple_name, repeat(seq(",", $.simple_name)))),
      "{",
      repeat(
        choice(
          seq($.class_property_definition, $._statement_terminator, repeat(";")),
          $.class_method_definition
        )
      ),
      "}"
    ),

    // Enums
    enum_statement: $ => seq(
      reservedWord("enum"), $.simple_name, "{",
      repeat(
        seq($.enum_member, $._statement_terminator, repeat(";"))
      ),
      "}"
    ),

    enum_member: $ => seq(
      $.simple_name,
      optional(seq("=", $.integer_literal))
    ),

    // Expressions
    _expression: $ => choice(
      $.logical_expression,
      $.bitwise_expression,
      $.comparison_expression,
      $.additive_expression,
      $.multiplicative_expression,
      $.format_expression,
      $.range_expression,
      $.array_literal_expression,
      $._unary_expression,
    ),

    logical_expression: $ => prec.left(
      seq(
        $._expression,
        choice(reservedWord("-and"), reservedWord("-or"), reservedWord("-xor")),
        $._expression,
      )
    ),

    bitwise_expression: $ => prec.left(
      seq(
        $._expression,
        choice(reservedWord("-band"), reservedWord("-bor"), reservedWord("-bxor")),
        $._expression,
      )
    ),

    comparison_expression: $ => prec.left(
      seq(
        $._expression,
        $.comparison_operator,
        $._expression,
      )
    ),

    additive_expression: $ => prec.left(
      seq(
        $._expression,
        choice("+", "-"),
        $._expression,
      )
    ),

    multiplicative_expression: $ => prec.left(
      seq(
        $._expression,
        choice("/", "\\", "%", "*"),
        $._expression,
      )
    ),

    format_expression: $ => prec.left(
      seq(
        $._expression,
        $.format_operator,
        $._expression,
      )
    ),

    range_expression: $ => prec.left(
      seq(
        $._expression,
        "..",
        $._expression,
      )
    ),

    array_literal_expression: $ => prec.left(seq(
      $._unary_expression,
      repeat(
        seq(
          ",",
          $._unary_expression
        )
      )
    )),

    _unary_expression: $ => prec.right(choice(
      $.unary_expression,
      $._primary_expression
    )),

    unary_expression: $ => prec.right(choice(
      $._expression_with_unary_operator
    )),

    _expression_with_unary_operator: $ => choice(
      seq(",", $._unary_expression),
      seq(reservedWord("-not"), $._unary_expression),
      seq("!", $._unary_expression),
      seq(reservedWord("-bnot"), $._unary_expression),
      seq("+", $._unary_expression),
      seq("-", $._unary_expression),
      $.pre_increment_expression,
      $.pre_decrement_expression,
      $.cast_expression,
      seq(reservedWord("-split"), $._unary_expression),
      seq(reservedWord("-join"), $._unary_expression)
    ),

    pre_increment_expression: $ => seq("++", $._unary_expression),
    pre_decrement_expression: $ => seq("--", $._unary_expression),

    cast_expression: $ => prec(PREC.CAST, seq($.type_literal, $._unary_expression)),

    attributed_variable: $ => seq($.type_literal, $.variable),

    _primary_expression: $ => choice(
      $._value,
      $.member_access,
      $.element_access,
      $.invokation_expression,
      $.post_increment_expression,
      $.post_decrement_expression
    ),

    _value: $ => choice(
      $.parenthesized_expression,
      $.sub_expression,
      $.array_expression,
      $.script_block_expression,
      $.hash_literal_expression,
      $._literal,
      $.type_literal,
      $.variable
    ),

    parenthesized_expression: $ => seq("(", $.pipeline, ")"),

    sub_expression: $ => seq("$(", field("statements", optional($.statement_list)), ")"),

    array_expression: $ => seq("@(", field("statements", optional($.statement_list)), ")"),

    script_block_expression: $ => seq("{", optional($.param_block), $.script_block, "}"),

    hash_literal_expression: $ => seq("@{", optional($.hash_literal_body), "}"),

    hash_literal_body: $ => repeat1($.hash_entry),

    hash_entry: $ => seq(
      $.key_expression,
      "=",
      $._statement, $._statement_terminator, repeat(";")
    ),

    key_expression: $ => choice(
      $.simple_name,
      $._unary_expression
    ),

    post_increment_expression: $ => prec(PREC.UNARY, seq($._primary_expression, "++")),

    post_decrement_expression: $ => prec(PREC.UNARY, seq($._primary_expression, "--")),

    member_access: $ => prec.left(choice(
      seq($._primary_expression, token.immediate("."), $.member_name),
      seq($._primary_expression, "::", $.member_name),
    )),

    member_name: $ => choice(
      $.simple_name,
      $.string_literal,
      $._expression_with_unary_operator,
      $._value
    ),

    element_access: $ => prec(PREC.ELEMENT_ACCESS, seq($._primary_expression, "[", $._expression, "]")),

    invokation_expression: $ => choice(
      seq($._primary_expression, token.immediate("."), $.member_name, $.argument_list),
      seq($._primary_expression, "::", $.member_name, $.argument_list),
      $.invokation_foreach_expression
    ),

    // adding this rule to handle .foreach synthax
    invokation_foreach_expression: $ => seq($._primary_expression, token.immediate(reservedWord(".foreach")), $.script_block_expression),

    argument_list: $ => seq("(", field("argument_expression_list", optional($.argument_expression_list)), ")"),

    argument_expression_list: $ => prec.left(seq(
      $.argument_expression,
      repeat(
        seq(",", $.argument_expression)
      )
    )),

    argument_expression: $ => $.logical_argument_expression,

    logical_argument_expression: $ => prec.left(choice(
      $.bitwise_argument_expression,
      seq (
        $.logical_argument_expression,
        choice(reservedWord("-and"), reservedWord("-or"), reservedWord("-xor")), $.bitwise_argument_expression
      )
    )),

    bitwise_argument_expression: $ => prec.left(choice(
      $.comparison_argument_expression,
      seq (
        $.bitwise_argument_expression,
        choice(reservedWord("-and"), reservedWord("-or"), reservedWord("-xor")), $.comparison_argument_expression
      )
    )),

    comparison_argument_expression: $ => prec.left(choice(
      $.additive_argument_expression,
      seq (
        $.comparison_argument_expression,
        $.comparison_operator, $.additive_argument_expression
      )
    )),

    additive_argument_expression: $ => prec.left(choice(
      $.multiplicative_argument_expression,
      seq (
        $.additive_argument_expression,
        choice("+", "-"), $.multiplicative_argument_expression
      )
    )),

    multiplicative_argument_expression: $ => prec.left(choice(
      $.format_argument_expression,
      seq (
        $.multiplicative_argument_expression,
        choice("/", "\\", "%", "*"), $.format_argument_expression
      )
    )),

    format_argument_expression: $ => prec.left(choice(
      $.range_argument_expression,
      seq (
        $.format_argument_expression,
        $.format_operator, $.range_argument_expression
      )
    )),

    range_argument_expression: $ => prec.left(choice(
      $._unary_expression,
      seq (
        $.range_argument_expression,
        "..", $._unary_expression
      )
    )),

    type_literal: $ => seq("[", $.type_spec, "]"),

    type_spec: $ => choice(
      seq($.array_type_name, optional($.dimension), "]"),
      seq($.generic_type_name, $.generic_type_arguments, "]"),
      $.type_name
    ),

    dimension: _ => repeat1(","),

    generic_type_arguments: $ => seq(
      $.type_spec,
      repeat(seq(",", $.type_spec))
    ),

    // Attributes
    attribute_list: $ => repeat1($.attribute),

    attribute: $ => choice(
      seq("[", $.attribute_name, "(", optional($.attribute_arguments), ")", "]"),
      $.type_literal
    ),

    attribute_name: $ => $.type_spec,

    attribute_arguments: $ => seq(
      $.attribute_argument,
      repeat(seq(",", $.attribute_argument))
    ),

    attribute_argument: $ => choice(
      $._expression,
      seq($.simple_name, optional(seq("=", $._expression)))
    )
  },
});

// inspired from https://github.com/tree-sitter/tree-sitter/issues/261
function reservedWord(word) {
  //return word // when debuging
  return alias(reserved(caseInsensitive(word)), word)
}

function reserved(regex) {
  return token(prec(PREC.KEYWORD, new RegExp(regex)))
}

function caseInsensitive(word) {
  return word.split('')
      .map(letter => `[${letter}${letter.toUpperCase()}]`)
      .join('')
}
