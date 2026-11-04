#define ATS_DYNLOADFLAG 0
#include "share/atspre_staload.hats"
staload UN = "prelude/SATS/unsafe.sats"
staload "src/errors/error.sats"

fun sapp (a: string, b: string): string =
  $UN.castvwtp0{string}(string_append(a, b))

implement error_kind_str (k) =
  case+ k of
  | EK_LinearityViolation () => "linearity.violation"
  | EK_NoncommutativeViolation () => "noncommutative.violation"
  | EK_UnboundOperator () => "unbound operator"
  | EK_UnappliedOperator () => "unapplied operator"
  | EK_FrameMismatch () => "frame mismatch"
  | EK_LockNotReleased () => "lock not released"
  | EK_CycleInGraph () => "cycle in operator graph"
  | EK_DimensionUnderflow () => "dimension.underflow"
  | EK_PoleUnhandled () => "pole.unhandled"
  | EK_CommutativityFailed () => "commutativity.failed"
  | EK_NumericalCommutativityCheck () => "numerical_commutativity_check"
  | EK_ParseError () => "parse error"
  | EK_TypeError () => "type error"
  | EK_InternalError () => "internal error"

implement error_format (Error (k, line, col, file, detail, hint)) =
  let
    val rule = error_kind_str (k)
    val loc = sapp (sapp (sapp (file, ":"), tostring_int (line)), sapp (":", tostring_int (col)))
  in
    sapp (sapp (sapp (sapp ("crimsopie: build aborted\n  rule:   ", rule), sapp ("\n  at:     ", loc)), sapp ("\n  detail: ", detail)), sapp ("\n  hint:   ", hint))
  end

implement error_emit (e) =
  let val s = error_format (e) in print_string (s); print_newline () end

implement error_fatal (line, col, file, msg) =
  error_emit (Error (EK_InternalError, line, col, file, msg, ""))

implement mk_linearity_violation (line, col, file, opname, detail) =
  Error (EK_LinearityViolation, line, col, file, sapp ("operator ", sapp (opname, sapp (": ", detail))), "ensure linearity")

implement mk_unbound_operator (line, col, file, opname) =
  Error (EK_UnboundOperator, line, col, file, sapp ("operator ", sapp (opname, " is not bound")), "introduce operator first")

implement mk_unapplied_operator (line, col, file, opname) =
  Error (EK_UnappliedOperator, line, col, file, sapp ("operator ", sapp (opname, " remains unapplied")), "apply before join")

implement mk_lock_not_released (line, col, file, mtx, opname) =
  Error (EK_LockNotReleased, line, col, file, sapp ("lock on ", sapp (mtx, sapp (" for ", opname))), "pair lock with unlock")

implement mk_dimension_underflow (line, col, file, dname) =
  Error (EK_DimensionUnderflow, line, col, file, sapp ("spectral parameter ", sapp (dname, " underflows")), "check bounds")

implement mk_commutativity_failed (line, col, file, h1, h2) =
  Error (EK_CommutativityFailed, line, col, file, sapp ("numerical commutativity check failed for ", sapp (h1, sapp (" and ", h2))), "use lock or provide diagonal/identity matrices for symbolic check")

implement mk_frame_mismatch (line, col, file, detail) =
  Error (EK_FrameMismatch, line, col, file, detail, "check frame")

implement mk_cycle_in_graph (line, col, file, detail) =
  Error (EK_CycleInGraph, line, col, file, detail, "must be DAG")

implement mk_pole_unhandled (line, col, file, dname, lamstr) =
  Error (EK_PoleUnhandled, line, col, file, sapp ("pole unhandled for ", lamstr), sapp ("add handler for ", dname))

implement mk_noncommutative_violation (line, col, file, h1, h2) =
  Error (EK_NoncommutativeViolation, line, col, file, sapp ("noncommutative ", sapp (h1, sapp (" and ", h2))), "use lock")

implement mk_parse_error (line, col, file, msg) =
  Error (EK_ParseError, line, col, file, msg, "check syntax")


implement mk_numerical_commutativity_check (line, col, file, h1, h2) =
  Error (EK_NumericalCommutativityCheck, line, col, file, sapp ("numerical commutativity check failed for ", sapp (h1, sapp (" and ", h2))), "use lock or provide diagonal/identity matrices for symbolic check")
