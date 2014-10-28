{ grammar, rules } = require("tree-sitter-compiler")
{ choice, err, repeat, seq, sym, keyword, token, optional, prec } = rules

commaSep1 = (rule) ->
  seq(rule, repeat(seq(",", rule)))

commaSep = (rule) ->
  optional(commaSep1(rule))

terminator = ->
  choice(";", sym("_line_break"))

module.exports = grammar
  name: 'go',

  ubiquitous: -> [
    @_comment,
    @_line_break,
    /[ \t\r]/
  ]

  rules:
    program: -> @source_file

    #########################################################################
    # Common

    _identifier: -> /[a-zA-Z_$][a-zA-Z\d_$]*/

    _comment: -> token(choice(
      seq("//", /.*/),
      seq("/*", repeat(choice(/[^\*]/, /\*[^/]/)), "*/")))

    _line_break: -> "\n"

    # these are misnomers as JS regexps don't support \p{} for unicode.
    # they're mainly here for completion.
    _unicode_char: -> /[^\n]/
    _unicode_letter: -> /[a-zA-Z]/
    _unicode_digit: -> /[0-9]/

    _binary_op: -> choice("||", "&&", @_rel_op, @_add_op, @_mul_op)
    _rel_op: -> choice("==", "!=", "<", "<=", ">", ">=")
    _add_op: -> choice("+", "-", "|", "^")
    _mul_op: -> choice("*", "/", "%", "<<", ">>", "&", "&^")
    _unary_op: -> choice("+", "-", "!", "^", "*", "&", "<-")

    _assign_op: -> seq(choice(@_add_op, @_mul_op), "=")

    _int_lit: -> choice(@_decimal_lit, @_octal_lit, @_hex_lit)
    _decimal_lit: -> /[1-9][0-9]*/
    _octal_lit: -> /0[0-7]*/
    _hex_lit: -> /0[xX][0-9A-Fa-f]+/

    _float_lit: -> choice(
      seq(@_decimals, ".", optional(@_decimals), optional(@_exponent)),
      seq(@_decimals, @_exponent),
      seq(".", @_decimals, optional(@_exponent)))
    _decimals: -> /[0-9]+/
    _exponent: -> /[eE][\+\-][0-9]+/

    _imaginary_lit: -> seq(choice(@_decimals, @_float_lit), "i")

    _rune_lit: -> seq("'", choice(@_unicode_value, @_byte_value), "'")
    _unicode_value: -> choice(@_unicode_char, @_little_u_value, @_big_u_value, @_escaped_char)
    _byte_value: -> choice(@_octal_byte_value, @_hex_byte_value)
    _octal_byte_value: -> /\\[0-7]{3}/
    _hex_byte_value: -> /\\x[0-9A-Fa-f]{2}/
    _little_u_value: -> /\\u[0-9A-Fa-f]{4}/
    _big_u_value: -> /\\U[0-9A-Fa-f]{8}/
    _escaped_char: -> /\\[abfnrtv\\'"]/

    _string_lit: -> choice(@_raw_string_lit, @_interpreted_string_lit)
    _raw_string_lit: -> seq('`', repeat(choice(/[^\\`]/, /\\./)), '`')
    _interpreted_string_lit: -> seq('"', repeat(choice(/[^\\"\n]/, /\\./)), '"')

    identifier_list: -> commaSep1(@_identifier)
    qualified_ident: -> seq(@package_name, ".", @_identifier)
    package_name: -> @_identifier


    #########################################################################
    # Package

    source_file: -> seq(
      @package_clause,
      terminator(),
      repeat(seq(@import_decl, terminator())),
      repeat(seq(@top_level_decl, terminator())))

    package_clause: -> seq(keyword("package"), @package_name)
    package_name: -> @_identifier

    import_decl: -> seq(keyword("import"), choice(@import_spec, seq("(", repeat(seq(@import_spec, terminator())), ")")))
    import_spec: -> seq(optional(choice(".", @package_name)), @import_path)
    import_path: -> @_string_lit


    #########################################################################
    # Expressions

    expression_list: -> commaSep1(@expression)

    expression: -> choice(@unary_expr, seq(@expression, @_binary_op, @unary_expr)) # TODO operator precedence
    unary_expr: -> choice(@primary_expr, seq(@_unary_op, @unary_expr))
    primary_expr: -> choice(
      @operand,
      @conversion,
      @builtin_call,
      seq(@primary_expr, @selector),
      seq(@primary_expr, @index),
      seq(@primary_expr, @slice),
      seq(@primary_expr, @type_assertion),
      seq(@primary_expr, @call))

    selector: -> seq(".", @_identifier)
    index: -> seq("[", @expression, "]")
    slice: -> seq(
      "[",
      choice(
        seq(optional(@expression), ":", optional(@expression))
        seq(optional(@expression), ":", @expression, ":", @expression)),
      "]")
    type_assertion: -> seq(".", "(", @type, ")")
    call: -> seq("(", optional(seq(@argument_list, optional(","))), ")")
    argument_list: -> seq(@expression_list, optional("..."))

    operand: -> choice(@literal, @operand_name, @method_expr, seq("(", @expression, ")"))
    literal: -> choice(@basic_lit, @composite_lit, @function_lit)
    basic_lit: -> choice(@_int_lit, @_float_lit, @_imaginary_lit, @_rune_lit, @_string_lit)
    operand_name: -> choice(@_identifier, @qualified_ident)

    composite_lit: -> seq(@literal_type, @literal_value)
    literal_type: -> choice(
      @struct_type,
      @array_type,
      seq("[", "...", "]", @element_type),
      @slice_type,
      @map_type,
      @type_name)
    literal_value: -> seq("{", optional(seq(@element_list, ",")), "}")
    element_list: -> commaSep1(@element)
    element: -> optional(seq(@key, ":"), @value)
    key: -> choice(@field_name, @element_index)
    field_name: -> @_identifier
    element_index: -> @expression
    value: -> choice(@expression, @literal_value)

    function_lit: -> seq(keyword("func"), @function)
    function: -> seq(@signature, @function_body)
    function_body: -> @block

    method_expr: -> seq(@receiver_type, ".", @method_name)
    receiver_type: -> choice(
      @type_name,
      seq("(", "*", @type_name, ")"),
      seq("(", @receiver_type, ")"))

    conversion: -> seq(@type, "(", @expression, optional(","), ")")

    builtin_call: -> seq(@_identifier, "(", optional(seq(@builtin_args, optional(","))), ")")
    builtin_args: -> choice(seq(@type, optional(seq(",", @argument_list))), @argument_list)


    #########################################################################
    # Types

    type_list: -> commaSep1(@type)

    type: -> choice(@type_name, @type_literal, seq("(", @type, ")"))
    type_name: -> choice(@_identifier, @qualified_ident)
    type_literal: -> choice(
      @array_type,
      @struct_type,
      @pointer_type,
      @function_type,
      @interface_type,
      @slice_type,
      @map_type,
      @channel_type)

    element_type: -> @type

    array_type: -> seq("[", @array_length, "]", @element_type)
    array_length: -> @expression

    struct_type: -> seq(keyword("struct"), "{", repeat(seq(@field_decl, terminator())), "}")
    field_decl: -> seq(
      choice(seq(@identifier_list, @type), @anonymous_field),
      optional(@tag))
    anonymous_field: -> seq(optional("*"), @type_name)
    tag: -> @_string_lit

    pointer_type: -> seq("*", @base_type)
    base_type: -> @type

    function_type: -> seq(keyword("func"), @signature)
    signature: -> seq(@parameters, optional(@result))
    result: -> choice(@parameters, @type)
    parameters: -> seq("(", optional(seq(@parameter_list, optional(","))), ")")
    parameter_list: -> seq(@parameter_decl, repeat(seq(",", @parameter_decl)))
    parameter_decl: -> seq(optional(@binding_or_type), optional("..."), @type)
    binding_or_type: -> choice(@_identifier, @type)

    interface_type: -> seq(keyword("interface"), "{", repeat(seq(@method_spec, terminator())), "}")
    method_spec: -> choice(seq(@method_name, @signature), @interface_type_name)
    method_name: -> @_identifier
    interface_type_name: -> @type_name

    slice_type: -> seq("[", "]", @element_type)

    map_type: -> seq(keyword("map"), "[", @key_type, "]", @element_type)
    key_type: -> @type

    channel_type: -> seq(choice(keyword("chan"), seq(keyword("chan"), "<-"), seq("<-", keyword("chan"))), @element_type)

    #########################################################################
    # Statements

    block: -> seq("{", @statement_list, "}")
    statement_list: -> repeat(seq(@statement, terminator()))

    statement: -> choice(
      @declaration,
      @labeled_stmt,
      @simple_stmt,
      @go_stmt,
      @return_stmt,
      @break_stmt,
      @continue_stmt,
      @goto_stmt,
      @fallthrough_stmt,
      @block,
      @if_stmt,
      @switch_stmt,
      @select_stmt,
      @for_stmt,
      @defer_stmt)

    simple_stmt: -> choice(
      @empty_stmt,
      @expression_stmt,
      @send_stmt,
      @inc_dec_stmt,
      @assignment,
      @short_var_decl)

    declaration: -> choice(@const_decl, @type_decl, @var_decl)
    top_level_decl: -> choice(@declaration, @function_decl, @method_decl)

    const_decl: -> seq(
      keyword("const"),
      choice(@const_spec, seq("(", repeat(@const_spec, terminator()), ")")))
    const_spec: -> seq(
      @identifier_list,
      optional(seq(optional(@type), "=", @expression_list)))

    type_decl: -> seq(
      keyword("type"),
      choice(@type_spec, seq("(", repeat(@type_spec, terminator()), ")")))
    type_spec: -> seq(@_identifier, @type)

    var_decl: -> seq(
      keyword("var"),
      choice(@var_spec, seq("(", repeat(@var_spec, terminator()), ")")))
    var_spec: -> seq(
      @identifier_list,
      choice(
        seq(@type, optional(seq("=", @expression_list))),
        seq("=", @expression_list)))

    function_decl: -> seq(
      keyword("func"),
      @function_name, choice(@function, @signature))
    function_name: -> @_identifier

    method_decl: -> seq(
      keyword("func"),
      @receiver, @method_name, choice(@function, @signature))
    receiver: -> seq("(", optional(@_identifier), optional("*", @base_type_name), ")")
    base_type_name: -> @_identifier

    labeled_stmt: -> seq(@label, ":", @statement)
    label: -> @_identifier

    empty_stmt: -> seq() # yup

    expression_stmt: -> @expression

    send_stmt: -> seq(@channel, "<-", @expression)
    channel: -> @expression

    inc_dec_stmt: -> seq(@expression, choice("++", "--"))

    assignment: -> seq(@expression_list, @_assign_op, @expression_list)

    short_var_decl: -> seq(@identifier_list, ":=", @expression_list)

    go_stmt: -> seq(keyword("go"), @expression)

    return_stmt: -> seq(keyword("return"), optional(@expression_list))

    break_stmt: -> seq(keyword("break"), optional(@label))
    continue_stmt: -> seq(keyword("continue"), optional(@label))
    goto_stmt: -> seq(keyword("goto"), optional(@label))
    fallthrough_stmt: -> keyword("fallthrough")

    if_stmt: -> seq(
      keyword("if"),
      optional(seq(@simple_stmt, ";")),
      @expression,
      @block,
      optional(seq(
        keyword("else"),
        choice(@if_stmt, @block))))

    switch_stmt: -> choice(@expr_switch_stmt, @type_switch_stmt)
    expr_switch_stmt: -> seq(
      keyword("switch"),
      optional(seq(@simple_stmt, ";")),
      optional(@expression),
      "{",
      repeat(@expr_case_clause),
      "}")
    expr_case_clause: -> seq(@expr_switch_case, ":", @statement_list)
    expr_switch_case: -> choice(seq(keyword("case"), @expression_list), keyword("default"))
    type_switch_stmt: -> seq(
      keyword("switch"),
      optional(seq(@simple_stmt, ";")),
      @type_switch_guard,
      "{",
      repeat(@type_case_clause),
      "}")
    type_switch_guard: -> seq(
      optional(seq(@_identifier, ":=")),
      @primary_expr,
      ".",
      "(",
      keyword("type"),
      ")")
    type_case_clause: -> seq(@type_switch_case, ":", @statement_list)
    type_switch_case: -> choice(seq(keyword("case"), @type_list), keyword("default"))

    select_stmt: -> seq(
      keyword("select"),
      "{",
      @comm_clause,
      "}")
    comm_clause: -> seq(@comm_case, ":", @statement_list)
    comm_case: -> choice(seq(keyword("case"), choice(@send_stmt, @recv_stmt)), keyword("default"))
    recv_stmt: -> seq(
      optional(choice(
        seq(@expression_list, "="),
        seq(@identifier_list, ":="))),
      @recv_expr)
    recv_expr: -> @expression

    for_stmt: -> seq(
      keyword("for"),
      optional(choice(@condition, @for_clause, @range_clause)),
      @block)
    condition: -> @expression
    for_clause: -> seq(optional(@init_stmt), ";", optional(@condition), ";", optional(@post_stmt))
    init_stmt: -> @simple_stmt
    post_stmt: -> @simple_stmt
    range_clause: -> seq(
      choice(
        seq(@expression_list, "="),
        seq(@identifier_list, ":=")),
      keyword("range"),
      @expression)

    defer_stmt: -> seq(keyword("defer"), @expression)
