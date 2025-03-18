const PREC = {
  KEYWORD: 1,
  UNARY: 2,
  CAST: 3,
  ELEMENT_ACCESS: 4,
  EMPTY: 5
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
    /[\uFEFF\u2060\u200B\u00A0]/
  ],

  conflicts: $ => [
    [$._literal, $.member_name],
    [$.class_property_definition, $.attribute],
    [$.class_method_definition, $.attribute],
    [$.expandable_string_literal],
    [$.path_command_name, $._value]
  ],

  rules: {

    program: $ => seq(
      optional($.param_block),
      $.statement_list
    ),

    // Comments
    comment: $ => token(
      choice(
        /#[^\n]*/,
        seq(
          "<#",
          repeat(
            choice(
              /[^#`]+/,
              /#+[^>#]/,
              /`.{1}|`\n/
            )
          ),
          /#+>/
        )
      )
    ),

    // Literals
    _literal: $ => choice(
      $.integer_literal,
      $.string_literal,
      $.real_literal
    ),

    integer_literal: $ => choice(
      $.decimal_integer_literal,
      $.hexadecimal_integer_literal
    ),

    decimal_integer_literal: $ => token(seq(
      /[0-9]+/, optional(choice("l", "d")), optional(choice("kb", "mb", "gb", "tb", "pb"))
    )),

    hexadecimal_integer_literal: $ => token(seq(
      "0x", /[0-9a-fA-F]+/, optional("l"), optional(choice("kb", "mb", "gb", "tb", "pb"))
    )),

    real_literal: $ => token(choice(
      seq(/[0-9]+\.[0-9]+/, optional(token(seq("e", optional(choice("+", "-")), /[0-9]+/))), optional(choice("kb", "mb", "gb", "tb", "pb"))),
      seq(/\.[0-9]+/, optional(token(seq("e", optional(choice("+", "-")), /[0-9]+/))), optional(choice("kb", "mb", "gb", "tb", "pb"))),
      seq(/[0-9]+/, token(seq("e", optional(choice("+", "-")), /[0-9]+/)), optional(choice("kb", "mb", "gb", "tb", "pb")))
    )),

    // String literals
    string_literal: $ => choice(
      $.expandable_string_literal,
      $.verbatim_string_characters,
      $.expandable_here_string_literal,
      $.verbatim_here_string_characters
    ),

    expandable_string_literal: $ => seq(
      /\"(\s*\#*)*/,
      repeat(
        choice(
          token.immediate(/[^\$\"`]+/),
          $.variable,
          $.sub_expression,
          token.immediate(/\$(`.{1}|`\n|[\s\\])/),
          token.immediate(/`.{1}|`\n/),
          token.immediate("\"\""),
          token.immediate("$")
        )
      ),
      repeat(token.immediate("$")),
      token.immediate(/(\s*\#*)*\"/)
    ),

    expandable_here_string_literal: $ => seq(
      /@\" *\n/,
      repeat(
        choice(
          token.immediate(/[^\$\n`]+/),
          $.variable,
          $.sub_expression,
          token.immediate(/\n+[^\"\n]/),
          token.immediate(/\n+\"[^@]/),
          token.immediate("$"),
          token.immediate(/`.{1}|`\n/)
        )
      ),
      token.immediate(/\n+\"@/)
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
        /@\'\s*\n/,
        repeat(
          choice(
            /[^\n]/,
            /\n+[^\'\n]/,
            /\n\'[^@]/,
          )
        ),
        /\n+\'@/
      )
    ),

    // Simple names and type names
    simple_name: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,
    type_identifier: $ => /[a-zA-Z0-9_]+/,
    type_name: $ => choice(
      $.type_identifier,
      seq($.type_name, ".", $.type_identifier)
    ),
    array_type_name: $ => seq($.type_name, "["),
    generic_type_name: $ => seq($.type_name, "["),

    // Operators
    assignement_operator: $ => choice(
      "=", "!=", "+=", "*=", "/=", "%="
    ),
    file_redirection_operator: $ => choice(
      ">",  ">>",  "2>",  "2>>",  "3>",  "3>>",  "4>",  "4>>",
      "5>",  "5>>",  "6>",  "6>>",  "*>",  "*>>",  "<"
    ),
    merging_redirection_operator: $ => choice(
      "*>&1",  "2>&1",  "3>&1",  "4>&1",  "5>&1",  "6>&1",
      "*>&2",  "1>&2",  "3>&2",  "4>&2",  "5>&2",  "6>&2"
    ),
    comparison_operator: $ => choice(
      reservedWord("-as"), reservedWord("-ccontains"), reservedWord("-ceq"),
      reservedWord("-cge"), reservedWord("-cgt"), reservedWord("-cle"),
      reservedWord("-clike"), reservedWord("-clt"), reservedWord("-cmatch"),
      reservedWord("-cne"), reservedWord("-cnotcontains"), reservedWord("-cnotlike"),
      reservedWord("-cnotmatch"), reservedWord("-contains"), reservedWord("-creplace"),
      reservedWord("-csplit"), reservedWord("-eq"), reservedWord("-ge"),
      reservedWord("-gt"), reservedWord("-icontains"), reservedWord("-ieq"),
      reservedWord("-ige"), reservedWord("-igt"), reservedWord("-ile"),
      reservedWord("-ilike"), reservedWord("-ilt"), reservedWord("-imatch"),
      reservedWord("-in"), reservedWord("-ine"), reservedWord("-inotcontains"),
      reservedWord("-inotlike"), reservedWord("-inotmatch"), reservedWord("-ireplace"),
      reservedWord("-is"), reservedWord("-isnot"), reservedWord("-isplit"),
      reservedWord("-join"), reservedWord("-le"), reservedWord("-like"),
      reservedWord("-lt"), reservedWord("-match"), reservedWord("-ne"),
      reservedWord("-notcontains"), reservedWord("-notin"), reservedWord("-notlike"),
      reservedWord("-notmatch"), reservedWord("-replace"), reservedWord("-shl"),
      reservedWord("-shr"), reservedWord("-split")
    ),
    format_operator: $ => reservedWord("-f"),

    // Variables
    variable: $ => choice(
      '$$',
      '$^',
      '$?',
      '$_',
      token(seq('$', optional(seq(choice(
        reservedWord("global:"), reservedWord("local:"), reservedWord("private:"),
        reservedWord("script:"), reservedWord("using:"), reservedWord("workflow:"), /[a-zA-Z0-9_]+/
      ), ":")), /[a-zA-Z0-9_]+|\?/)),
      token(seq('@', optional(seq(choice(
        reservedWord("global:"), reservedWord("local:"), reservedWord("private:"),
        reservedWord("script:"), reservedWord("using:"), reservedWord("workflow:"), /[a-zA-Z0-9_]+/
      ), ":")), /[a-zA-Z0-9_]+|\?/)),
      $.braced_variable
    ),
    braced_variable: $ => /\$\{[^}]+\}/,

    // Commands
    generic_token: $ => token(
      /[^\(\)\$\"\'\-\{\}@\|\[`\s][^\s\(\)\}\|;,]*/
    ),
    _command_token: $ => token(/[^\(\)\{\}\s;]+/),

    // Parameters
    command_parameter: $ => token(
      choice(
        /-+[a-zA-Z_?\-`]+/,
        "--"
      )
    ),
    _verbatim_command_argument_chars: $ => repeat1(
      choice(
        /"[^"]*"/,
        /&[^&]*/,
        /[^\|\n]+/
      )
    ),

    // Grammar – Statements
    script_block: $ => choice(
      field("script_block_body", $.script_block_body),
      seq(seq($.param_block, $._statement_terminator, repeat(";")),
          field("script_block_body", optional($.script_block_body)))
    ),

    param_block: $ => seq(
      field("attributes", optional($.attribute_list)),
      field("keyword", reservedWord("param")),
      "(",
      field("parameters", optional($.parameter_list)),
      ")"
    ),

    parameter_list: $ => seq(
      $.script_parameter,
      repeat(seq(",", $.script_parameter))
    ),

    script_parameter: $ => seq(
      field("attributes", optional($.attribute_list)),
      field("variable", $.variable),
      field("default", optional($.script_parameter_default))
    ),

    script_parameter_default: $ => seq(
      "=",
      field("default_value", $._expression)
    ),

    script_block_body: $ => choice(
      field("named_block_list", $.named_block_list),
      field("statement_list", $.statement_list)
    ),

    named_block_list: $ => repeat1($.named_block),

    named_block: $ => seq(
      field("block_name", $.block_name),
      field("body", $.statement_block)
    ),

    block_name: $ => field("name", choice(
      reservedWord("dynamicparam"),
      reservedWord("begin"),
      reservedWord("process"),
      reservedWord("end")
    )),

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

    empty_statement: $ => prec(PREC.EMPTY, ";"),

    if_statement: $ => prec.left(seq(
      field("keyword", reservedWord("if")),
      "(",
      field("condition", $.pipeline),
      ")",
      field("body", $.statement_block),
      field("elseif_clauses", optional($.elseif_clauses)),
      field("else_clause", optional($.else_clause))
    )),

    elseif_clauses: $ => prec.left(repeat1($.elseif_clause)),

    elseif_clause: $ => seq(
      field("keyword", reservedWord("elseif")),
      "(",
      field("condition", $.pipeline),
      ")",
      field("body", $.statement_block)
    ),

    else_clause: $ => seq(
      field("keyword", reservedWord("else")),
      field("body", $.statement_block)
    ),

    _labeled_statement: $ => choice(
      $.switch_statement,
      $.foreach_statement,
      $.for_statement,
      $.while_statement,
      $.do_statement
    ),

    switch_statement: $ => seq(
      field("keyword", reservedWord("switch")),
      field("parameters", optional($.switch_parameters)),
      field("condition", $.switch_condition),
      field("body", $.switch_body)
    ),

    switch_parameters: $ => repeat1($.switch_parameter),

    switch_parameter: $ => choice(
      reservedWord("-regex"),
      reservedWord("-wildcard"),
      reservedWord("-exact"),
      reservedWord("-casesensitive"),
      reservedWord("-parallel")
    ),

    switch_condition: $ => choice(
      seq("(", field("condition", $.pipeline), ")"),
      seq(field("keyword", reservedWord("-file")), field("filename", $.switch_filename))
    ),

    switch_filename: $ => choice(
      $._command_token,
      $._primary_expression
    ),

    switch_body: $ => seq("{", optional($.switch_clauses), "}"),

    switch_clauses: $ => repeat1($.switch_clause),

    switch_clause: $ => seq(
      field("condition", $.switch_clause_condition),
      field("body", $.statement_block),
      $._statement_terminator,
      repeat(";")
    ),

    switch_clause_condition: $ => choice(
      $._command_token,
      $._primary_expression
    ),

    foreach_statement: $ => seq(
      field("keyword", reservedWord("foreach")),
      field("parameter", optional($.foreach_parameter)),
      "(",
      field("variable", $.variable),
      field("in_keyword", reservedWord("in")),
      field("collection", $.pipeline),
      ")",
      field("body", $.statement_block)
    ),

    foreach_parameter: $ => choice(
      reservedWord("-parallel")
    ),

    for_statement: $ => seq(
      field("keyword", reservedWord("for")),
      "(",
        optional(
          seq(
            optional(seq(field("for_initializer", $.for_initializer), $._statement_terminator)),
            optional(
              seq(
                choice(";", "\n"),
                optional(seq(field("for_condition", $.for_condition), $._statement_terminator)),
                optional(
                  seq(
                    choice(";", "\n"),
                    optional(seq(field("for_iterator", $.for_iterator), $._statement_terminator))
                  )
                )
              )
            )
          )
        ),
      ")",
      field("body", $.statement_block)
    ),

    for_initializer: $ => $.pipeline,
    for_condition: $ => $.pipeline,
    for_iterator: $ => $.pipeline,

    while_statement: $ => seq(
      field("keyword", reservedWord("while")),
      "(",
      field("condition", $.while_condition),
      ")",
      field("body", $.statement_block)
    ),

    while_condition: $=> $.pipeline,

    do_statement: $ => seq(
      field("keyword", reservedWord("do")),
      field("body", $.statement_block),
      field("loop_keyword", choice(reservedWord("while"), reservedWord("until"))),
      "(",
      field("condition", $.while_condition),
      ")"
    ),

    function_statement: $ => seq(
      field("keyword", choice(
        reservedWord("function"),
        reservedWord("filter"),
        reservedWord("workflow")
      )),
      field("function_name", $.function_name),
      field("parameters", optional($.function_parameter_declaration)),
      "{",
      field("body", optional($.script_block)),
      "}"
    ),

    function_name: $ => $._command_token,

    function_parameter_declaration: $ => seq(
      "(",
      field("parameters", optional($.parameter_list)),
      ")"
    ),

    flow_control_statement: $ => choice(
      seq(field("keyword", reservedWord("break")), field("label", optional($.label_expression))),
      seq(field("keyword", reservedWord("continue")), field("label", optional($.label_expression))),
      seq(field("keyword", reservedWord("throw")), field("expression", optional($.pipeline))),
      seq(field("keyword", reservedWord("return")), field("expression", optional($.pipeline))),
      seq(field("keyword", reservedWord("exit")), field("expression", optional($.pipeline)))
    ),

    label: $ => token(seq(":", /[a-zA-Z_][a-zA-Z0-9_]*/)),
    label_expression: $ => choice(
      $.label,
      $.unary_expression
    ),

    trap_statement: $ => seq(
      field("keyword", reservedWord("trap")),
      field("type", optional($.type_literal)),
      field("body", $.statement_block)
    ),

    try_statement: $ => seq(
      field("keyword", reservedWord("try")),
      field("body", $.statement_block),
      field("handler", choice(
        seq(field("catches", $.catch_clauses), field("finally", optional($.finally_clause))),
        field("finally", optional($.finally_clause))
      ))
    ),

    catch_clauses: $ => repeat1($.catch_clause),

    catch_clause: $ => seq(
      field("keyword", reservedWord("catch")),
      field("types", optional($.catch_type_list)),
      field("body", $.statement_block)
    ),

    catch_type_list: $ => seq(
      $.type_literal,
      repeat(seq(",", $.type_literal))
    ),

    finally_clause: $ => seq(
      field("keyword", reservedWord("finally")),
      field("body", $.statement_block)
    ),

    data_statement: $ => seq(
      field("keyword", reservedWord("data")),
      field("data_name", $.data_name),
      field("commands_allowed", optional($.data_commands_allowed)),
      field("body", $.statement_block)
    ),
    data_name: $ => $.simple_name,

    data_commands_allowed: $ => seq(
      reservedWord("-supportedcommand"),
      field("commands", $.data_commands_list)
    ),
    data_commands_list: $ => seq(
      $.data_command,
      repeat(seq(",", $.data_command))
    ),
    data_command: $ => $.command_name_expr,

    inlinescript_statement: $ => seq(
      field("keyword", reservedWord("inlinescript")),
      field("body", $.statement_block)
    ),

    parallel_statement: $ => seq(
      field("keyword", reservedWord("parallel")),
      field("body", $.statement_block)
    ),

    sequence_statement: $ => seq(
      field("keyword", reservedWord("sequence")),
      field("body", $.statement_block)
    ),

    pipeline: $ => choice(
      $.assignment_expression,
      seq(
        field("expression", $._expression),
        field("redirections", optional($.redirections)),
        field("tail", optional($._pipeline_tail))
      ),
      seq(
        field("command", $.command),
        field("argument", optional($.verbatim_command_argument)),
        field("tail", optional($._pipeline_tail))
      )
    ),

    left_assignment_expression: $ => $._expression,

    assignment_expression: $ => seq(
      field("left", $.left_assignment_expression),
      field("operator", $.assignement_operator),
      field("right", $._statement)
    ),

    _pipeline_tail: $ => repeat1(
      seq('|', $.command)
    ),

    command: $ => choice(
      seq(
        field("command_name", $.command_name),
        field("command_elements", optional($.command_elements))
      ),
      $.foreach_command,
      seq(
        field("invocation_operator", $.command_invokation_operator),
        field("command_name", $.command_name_expr),
        field("command_elements", optional($.command_elements))
      )
    ),

    command_invokation_operator: $ => choice(
      ".",
      "&"
    ),

    _expandable_string_literal_immediate: $ => seq(
      repeat(
        choice(
          /[^\$"`]+/,
          $.variable,
          /\$`(.{1}|`\n)/,
          /`.{1}|`\n/,
          "\"\"",
          $.sub_expression
        )
      ),
      repeat("$"),
      "\""
    ),

    command_name: $ => prec.right(seq(
      /[^\{\}\(\);,\|\&`"'\s\n\[\]\+\-\*\/\$@<\!%]+/,
      repeat(
        choice(
          token.immediate(/[^\{\}\(\);,\|\&"'\s\n]+/),
          seq(token.immediate("\""), $._expandable_string_literal_immediate),
          token.immediate("\"\""),
          token.immediate("''")
        )
      )
    )),

    path_command_name_token: $ => /[0-9a-zA-Z_?\-\.\\]+/,
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
      seq(
        field("argument", $._command_argument),
        field("arguments", optional($.argument_list))
      ),
      $.redirection,
      $.stop_parsing
    ),

    stop_parsing: $ => /--%[^\n]*/,
    command_argument_sep: $ => prec.right(choice(repeat1(" "), ":")),
    _command_argument: $ => prec.right(choice(
      seq($.command_argument_sep, optional($.generic_token)),
      seq($.command_argument_sep, $.array_literal_expression)
    )),

    foreach_command: $ => seq(
      field("keyword", choice("%", reservedWord("foreach-object"))),
      field("command_elements", repeat1($.script_block_expression))
    ),

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

    // Class-related rules
    class_attribute: $ => choice(reservedWord("hidden"), reservedWord("static")),
    class_property_definition: $ => seq(
      optional($.attribute),
      repeat($.class_attribute),
      optional($.type_literal),
      field("property_name", $.variable),
      optional(seq("=", field("value", $._expression)))
    ),
    class_method_parameter: $ => seq(
      field("type", optional($.type_literal)),
      field("variable", $.variable)
    ),
    class_method_parameter_list: $ => seq(
      $.class_method_parameter,
      repeat(seq(",", $.class_method_parameter))
    ),
    class_method_definition: $ => seq(
      optional($.attribute),
      repeat($.class_attribute),
      optional($.type_literal),
      field("method_name", $.simple_name),
      "(",
      field("parameters", optional($.class_method_parameter_list)),
      ")",
      "{",
      field("body", optional($.script_block)),
      "}"
    ),
    class_statement: $ => seq(
      field("keyword", reservedWord("class")),
      field("class_name", $.simple_name),
      optional(seq(":", field("base_classes", seq($.simple_name, repeat(seq(",", $.simple_name)))))),
      "{",
      repeat(
        choice(
          seq(field("property", $.class_property_definition), $._statement_terminator, repeat(";")),
          $.class_method_definition
        )
      ),
      "}"
    ),

    // Enum-related rules
    enum_statement: $ => seq(
      field("keyword", reservedWord("enum")),
      field("enum_name", $.simple_name),
      "{",
      repeat(seq(field("member", $.enum_member), $._statement_terminator, repeat(";"))),
      "}"
    ),
    enum_member: $ => seq(
      field("member_name", $.simple_name),
      optional(seq("=", field("value", $.integer_literal)))
    ),

    // Expressions
    _expression: $ => $.logical_expression,
    logical_expression: $ => prec.left(choice(
      $.bitwise_expression,
      seq(
        $.logical_expression,
        choice(reservedWord("-and"), reservedWord("-or"), reservedWord("-xor")),
        $.bitwise_expression
      )
    )),
    bitwise_expression: $ => prec.left(choice(
      $.comparison_expression,
      seq(
        $.bitwise_expression,
        choice(reservedWord("-band"), reservedWord("-bor"), reservedWord("-bxor")),
        $.comparison_expression
      )
    )),
    comparison_expression: $ => prec.left(choice(
      $.additive_expression,
      seq(
        $.comparison_expression,
        $.comparison_operator,
        $.additive_expression
      )
    )),
    additive_expression: $ => prec.left(choice(
      $.multiplicative_expression,
      seq(
        $.additive_expression,
        choice("+", "-"),
        $.multiplicative_expression
      )
    )),
    multiplicative_expression: $ => prec.left(choice(
      $.format_expression,
      seq(
        $.multiplicative_expression,
        choice("/", "\\", "%", "*"),
        $.format_expression
      )
    )),
    format_expression: $ => prec.left(choice(
      $.range_expression,
      seq(
        $.format_expression,
        $.format_operator,
        $.range_expression
      )
    )),
    range_expression: $ => prec.left(choice(
      $.array_literal_expression,
      seq(
        $.range_expression,
        "..",
        $.array_literal_expression
      )
    )),
    array_literal_expression: $ => prec.left(seq(
      $.unary_expression,
      repeat(seq(",", $.unary_expression))
    )),
    unary_expression: $ => prec.right(choice(
      $._primary_expression,
      $.expression_with_unary_operator
    )),
    expression_with_unary_operator: $ => choice(
      seq(",", $.unary_expression),
      seq(reservedWord("-not"), $.unary_expression),
      seq("!", $.unary_expression),
      seq(reservedWord("-bnot"), $.unary_expression),
      seq("+", $.unary_expression),
      seq("-", $.unary_expression),
      $.pre_increment_expression,
      $.pre_decrement_expression,
      $.cast_expression,
      seq(reservedWord("-split"), $.unary_expression),
      seq(reservedWord("-join"), $.unary_expression)
    ),
    pre_increment_expression: $ => seq("++", $.unary_expression),
    pre_decrement_expression: $ => seq("--", $.unary_expression),
    cast_expression: $ => prec(PREC.CAST, seq($.type_literal, $.unary_expression)),
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
    parenthesized_expression: $ => seq("(", field("expression", $.pipeline), ")"),
    sub_expression: $ => seq("$(", field("statements", optional($.statement_list)), ")"),
    array_expression: $ => seq("@(", field("statements", optional($.statement_list)), ")"),
    script_block_expression: $ => seq("{", optional($.param_block), $.script_block, "}"),
    hash_literal_expression: $ => seq("@{", field("entries", optional($.hash_literal_body)), "}"),
    hash_literal_body: $ => repeat1($.hash_entry),
    hash_entry: $ => seq(
      field("key", $.key_expression),
      "=",
      field("value", $._statement),
      $._statement_terminator,
      repeat(";")
    ),
    key_expression: $ => choice(
      $.simple_name,
      $.unary_expression
    ),
    post_increment_expression: $ => prec(PREC.UNARY, seq($._primary_expression, "++")),
    post_decrement_expression: $ => prec(PREC.UNARY, seq($._primary_expression, "--")),
    member_access: $ => prec.left(choice(
      seq(field("object", $._primary_expression), token.immediate("."), field("member", $.member_name)),
      seq(field("object", $._primary_expression), "::", field("member", $.member_name))
    )),
    member_name: $ => choice(
      $.simple_name,
      $.string_literal,
      $.expression_with_unary_operator,
      $._value
    ),
    element_access: $ => prec(PREC.ELEMENT_ACCESS, seq(
      field("object", $._primary_expression),
      "[",
      field("index", $._expression),
      "]"
    )),
    invokation_expression: $ => choice(
      seq(
        field("object", $._primary_expression),
        token.immediate("."),
        field("member", $.member_name),
        field("arguments", $.argument_list)
      ),
      seq(
        field("object", $._primary_expression),
        "::",
        field("member", $.member_name),
        field("arguments", $.argument_list)
      ),
      $.invokation_foreach_expression
    ),
    invokation_foreach_expression: $ => seq(
      field("object", $._primary_expression),
      token.immediate(reservedWord(".foreach")),
      field("block", $.script_block_expression)
    ),
    argument_list: $ => seq("(", field("argument_expression_list", optional($.argument_expression_list)), ")"),
    argument_expression_list: $ => prec.left(seq(
      $.argument_expression,
      repeat(seq(",", $.argument_expression))
    )),
    argument_expression: $ => $.logical_argument_expression,
    logical_argument_expression: $ => prec.left(choice(
      $.bitwise_argument_expression,
      seq(
        $.logical_argument_expression,
        choice(reservedWord("-and"), reservedWord("-or"), reservedWord("-xor")),
        $.bitwise_argument_expression
      )
    )),
    bitwise_argument_expression: $ => prec.left(choice(
      $.comparison_argument_expression,
      seq(
        $.bitwise_argument_expression,
        choice(reservedWord("-and"), reservedWord("-or"), reservedWord("-xor")),
        $.comparison_argument_expression
      )
    )),
    comparison_argument_expression: $ => prec.left(choice(
      $.additive_argument_expression,
      seq(
        $.comparison_argument_expression,
        $.comparison_operator,
        $.additive_argument_expression
      )
    )),
    additive_argument_expression: $ => prec.left(choice(
      $.multiplicative_argument_expression,
      seq(
        $.additive_argument_expression,
        choice("+", "-"),
        $.multiplicative_argument_expression
      )
    )),
    multiplicative_argument_expression: $ => prec.left(choice(
      $.format_argument_expression,
      seq(
        $.multiplicative_argument_expression,
        choice("/", "\\", "%", "*"),
        $.format_argument_expression
      )
    )),
    format_argument_expression: $ => prec.left(choice(
      $.range_argument_expression,
      seq(
        $.format_argument_expression,
        $.format_operator,
        $.range_argument_expression
      )
    )),
    range_argument_expression: $ => prec.left(choice(
      $.unary_expression,
      seq(
        $.range_argument_expression,
        "..",
        $.unary_expression
      )
    )),
    type_literal: $ => seq("[", field("type", $.type_spec), "]"),
    type_spec: $ => choice(
      seq($.array_type_name, optional($.dimension), "]"),
      seq($.generic_type_name, $.generic_type_arguments, "]"),
      $.type_name
    ),
    dimension: $ => repeat1(","),
    generic_type_arguments: $ => seq(
      $.type_spec,
      repeat(seq(",", $.type_spec))
    ),

    // Attributes
    attribute_list: $ => repeat1($.attribute),
    attribute: $ => choice(
      seq("[", field("name", $.attribute_name), "(", field("arguments", optional($.attribute_arguments)), ")", "]"),
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

// Inspired by https://github.com/tree-sitter/tree-sitter/issues/261
function reservedWord(word) {
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
