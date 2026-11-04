#define ATS_PACKNAME "crimsopie.lexer.token"

abstype token_t = ptr
fun token_create (kind: int, line: int, col: int, text: string): token_t
fun token_kind   (t: token_t): int
fun token_line   (t: token_t): int
fun token_col    (t: token_t): int
fun token_text   (t: token_t): string
fun token_is_eof (t: token_t): bool
