#define ATS_PACKNAME "crimsopie.checker.verify"
staload "src/parser/ast.sats"
staload "src/checker/env.sats"
staload "src/errors/error.sats"

dataviewtype check_res =
  | CKok of (env_vt)
  | CKerr of (env_vt)

fun check_program   (p: program_t): bool
fun check_stmt      (s: stmt_t, e: env_vt): check_res
fun check_stmts     (ss: List0(stmt_t), e: env_vt): check_res
fun check_func      (f: func_t, e: env_vt): check_res
fun validate_stmt   (s: stmt_t, e: env_vt): check_res
fun apply_stmt      (s: stmt_t, e: env_vt): env_vt
fun validate_program (p: program_t): bool
fun apply_program    (p: program_t, e: env_vt): env_vt
