#define ATS_DYNLOADFLAG 0
#include "share/atspre_staload.hats"
staload "src/lexer/token.sats"

datatype token = Token of (int, int, int, string)
assume token_t = token

implement token_create (k, l, c, s) = Token (k, l, c, s)
implement token_kind (Token (k, _, _, _)) = k
implement token_line (Token (_, l, _, _)) = l
implement token_col  (Token (_, _, c, _)) = c
implement token_text (Token (_, _, _, s)) = s
implement token_is_eof (t) = token_kind(t) = 58
