#define ATS_PACKNAME "crimsopie.lexer"
staload "src/lexer/token.sats"

abstype lexer_t = ptr

fun lexer_create  (src: string, filename: string): lexer_t
fun lexer_destroy (l: lexer_t): void
fun lexer_next    (l: lexer_t): token_t
fun lexer_peek    (l: lexer_t): token_t
fun lexer_peek2   (l: lexer_t): token_t
