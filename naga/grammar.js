/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "naga",

  rules: {
      module: $ => repeat($._defn),

      _defn: $ => choice(
          $.func_defn
      ),

      func_defn: $ => seq(
          'fun',
          $.ident,
          '()',
          $.block
      ),

      block: $ => seq(
          ':',
          repeat(choice($._expr, $._stmt)),
          ';'
      ),

      _expr: $ => choice(
          $.ident,
          $.numeral,
          $.arith_expr,
      ),

      _stmt: $ => choice(
          $.print_stmt,
          $.assign_stmt,
      ),

      arith_expr: $ => prec.left(seq(
          $._expr,
          $.binop,
          $._expr
      )),

      print_stmt: $ => seq(
          "print",
          $._expr
      ),

      assign_stmt: $ => seq(
          $.ident,
          "=",
          $._expr
      ),

      ident: $ => /[a-z]+/,
      numeral: $ => /\d+/,
      binop: $ => /[\+\-]{1}/
  }

});
