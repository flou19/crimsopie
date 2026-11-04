#define ATS_PACKNAME "crimsopie.errors"

datatype error_kind =
  | EK_LinearityViolation
  | EK_NoncommutativeViolation
  | EK_UnboundOperator
  | EK_UnappliedOperator
  | EK_FrameMismatch
  | EK_LockNotReleased
  | EK_CycleInGraph
  | EK_DimensionUnderflow
  | EK_PoleUnhandled
  | EK_CommutativityFailed
  | EK_NumericalCommutativityCheck
  | EK_ParseError
  | EK_TypeError
  | EK_InternalError

datatype error_t = Error of (error_kind, int, int, string, string, string)

fun error_kind_str (k: error_kind): string
fun error_format (e: error_t): string
fun error_emit (e: error_t): void
fun error_fatal (line: int, col: int, file: string, msg: string): void

fun mk_linearity_violation      (line: int, col: int, file: string, opname: string, detail: string): error_t
fun mk_unbound_operator         (line: int, col: int, file: string, opname: string): error_t
fun mk_unapplied_operator       (line: int, col: int, file: string, opname: string): error_t
fun mk_lock_not_released        (line: int, col: int, file: string, mtx: string, opname: string): error_t
fun mk_dimension_underflow      (line: int, col: int, file: string, dname: string): error_t
fun mk_numerical_commutativity_check (line: int, col: int, file: string, h1: string, h2: string): error_t
fun mk_commutativity_failed     (line: int, col: int, file: string, h1: string, h2: string): error_t
fun mk_frame_mismatch           (line: int, col: int, file: string, detail: string): error_t
fun mk_cycle_in_graph           (line: int, col: int, file: string, detail: string): error_t
fun mk_pole_unhandled           (line: int, col: int, file: string, dname: string, lamstr: string): error_t
fun mk_noncommutative_violation (line: int, col: int, file: string, h1: string, h2: string): error_t
fun mk_parse_error              (line: int, col: int, file: string, msg: string): error_t
