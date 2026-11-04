#define ATS_PACKNAME "crimsopie.parser"
staload "src/lexer/token.sats"
staload "src/lexer/lexer.sats"
staload "src/parser/ast.sats"

fun parse_program (l: lexer_t, filename: string): Option(program_t)
fun parse_expr (l: lexer_t): Option(expr_t)
fun parse_stmt (l: lexer_t): Option(stmt_t)
