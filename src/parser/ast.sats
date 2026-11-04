#define ATS_PACKNAME "crimsopie.parser.ast"

abstype loc_t = ptr
fun loc_file (l: loc_t): string
fun loc_line (l: loc_t): int
fun loc_col  (l: loc_t): int
fun loc_to_string (l: loc_t): string
fun loc_make (file: string, line: int, col: int): loc_t

datatype expr_t =
  | EIdent     of (loc_t, string)
  | ESubscript of (loc_t, string, string)
  | ENat       of (loc_t, int)
  | EReal      of (loc_t, string)
  | EOpRef     of (loc_t, string, string)
  | EActRef    of (loc_t, string)
  | EPipe      of (loc_t, expr_t, expr_t)
  | ENorm      of (loc_t, expr_t)
  | EDiv       of (loc_t, expr_t, expr_t)
  | EMul       of (loc_t, expr_t, expr_t)
  | EAdd       of (loc_t, expr_t, expr_t)
  | ESub       of (loc_t, expr_t, expr_t)
  | EPow       of (loc_t, expr_t, expr_t)
  | ELt        of (loc_t, expr_t, expr_t)
  | EGt        of (loc_t, expr_t, expr_t)
  | EEq        of (loc_t, expr_t, expr_t)
  | EConj      of (loc_t, expr_t, expr_t)

datatype cond_t =
  | CExpr of (loc_t, expr_t)
  | CAnd  of (loc_t, cond_t, cond_t)

datatype inv_cond_t = InvCond of (loc_t, List0(cond_t))

datatype param_t =
  | PLinearOperator of (loc_t, string)
  | PSharedOperator of (loc_t, string)
  | PVector   of (loc_t, string)
  | PSize     of (loc_t, string)
  | PReal     of (loc_t, string)
  | PDim      of (loc_t, string)

datatype stmt_t =
  | SApply       of (loc_t, expr_t, expr_t, Option(cond_t))
  | SElim        of (loc_t, expr_t, expr_t, Option(cond_t))
  | SCanon       of (loc_t, expr_t, expr_t)
  | SCanon2      of (loc_t, expr_t)
  | SDirectSum   of (loc_t, expr_t, expr_t)
  | SMarker      of (loc_t, expr_t, string)
  | SGhostOp     of (loc_t, expr_t)
  | SRef         of (loc_t, expr_t)
  | SFrame       of (loc_t, string)
  | SMat         of (loc_t, string, List0(double))
  | SLock        of (loc_t, string, string)
  | SUnlock      of (loc_t, string, string)
  | SCommute     of (loc_t, string, string)
  | SPole        of (loc_t, string, expr_t)
  | SInv         of (loc_t, List0(inv_cond_t))
  | SExit        of (loc_t, List0(inv_cond_t))
  | SDisjoint    of (loc_t, List0(string))
  | SDimDecl     of (loc_t, string, bool, int)
  | SBranch      of (loc_t, cond_t, List0(stmt_t))
  | SDecompose   of (loc_t, int, int, int, string, List0(stmt_t))
  | SMerge       of (loc_t, string, List0(string))
  | SPivot       of (loc_t, string, List0(string))
  | SLive        of (loc_t, List0(string))
  | SDeferred    of (loc_t, string, List0(string))
  | SOnpie       of (loc_t, List0(string))
  | SGath        of (loc_t, List0(string))
  | SLoop        of (loc_t, string, string, int, List0(inv_cond_t), List0(stmt_t))
  | SJoin        of (loc_t, List0(string))
  | SGFun        of (loc_t, string, List0(param_t), List0(stmt_t))
  | SCompressBlk of (loc_t, string, List0(stmt_t))

datatype func_t = Func of (loc_t, string, List0(param_t), List0(stmt_t))
datatype program_t = Program of List0(func_t)

fun expr_loc (e: expr_t): loc_t
fun stmt_loc (s: stmt_t): loc_t
