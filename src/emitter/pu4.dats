#define ATS_DYNLOADFLAG 0
#include "share/atspre_staload.hats"
staload UN = "prelude/SATS/unsafe.sats"
staload "src/parser/ast.sats"
staload "src/emitter/pu4.sats"
staload "src/utils/io.sats"

fun sapp (a: string, b: string): string =
  $UN.castvwtp0{string}(string_append(a, b))

fun expr_to_str (e: expr_t): string =
  case+ e of
  | EIdent (_, n) => n
  | ENat (_, i) => tostring_int (i)
  | EReal (_, s) => s
  | EOpRef (_, h, t) => sapp (h, sapp ("|", t))
  | EActRef (_, n) => sapp (n, "|act")
  | EPipe (_, a, b) => sapp (expr_to_str (a), sapp (" | ", expr_to_str (b)))
  | ENorm (_, e0) => sapp ("norm(", sapp (expr_to_str (e0), ")"))
  | EDiv (_, a, b) => sapp (expr_to_str (a), sapp (" / ", expr_to_str (b)))
  | EAdd (_, a, b) => sapp (expr_to_str (a), sapp (" + ", expr_to_str (b)))
  | EMul (_, a, b) => sapp (expr_to_str (a), sapp (" * ", expr_to_str (b)))
  | ESub (_, a, b) => sapp (expr_to_str (a), sapp (" - ", expr_to_str (b)))
  | EPow (_, a, b) => sapp (expr_to_str (a), sapp ("^", expr_to_str (b)))
  | ELt (_, a, b) => sapp (expr_to_str (a), sapp (" < ", expr_to_str (b)))
  | EGt (_, a, b) => sapp (expr_to_str (a), sapp (" > ", expr_to_str (b)))
  | EEq (_, a, b) => sapp (expr_to_str (a), sapp (" = ", expr_to_str (b)))
  | EConj (_, a, b) => sapp (expr_to_str (a), sapp (" and ", expr_to_str (b)))
  | ESubscript (_, a, b) => sapp (a, sapp ("[", sapp (b, "]")))

fun emit_stmt (s: stmt_t, ind: int, buf: ref(string)): void =
  let
    fun w (line: string): void =
      !buf := sapp (!buf, sapp (line, "\n"))
  in
    case+ s of
    | SApply (loc, lhs, rhs, _) =>
      w (sapp ("L", sapp (tostring_int (loc_line (loc)), sapp ("  apply:   ", sapp (expr_to_str (lhs), sapp (" >> ", expr_to_str (rhs)))))))
    | SElim (loc, lhs, rhs, _) =>
      w (sapp ("L", sapp (tostring_int (loc_line (loc)), sapp ("  elim:    ", sapp (expr_to_str (lhs), sapp (" << ", expr_to_str (rhs)))))))
    | SCanon (loc, lhs, rhs) =>
      w (sapp ("L", sapp (tostring_int (loc_line (loc)), sapp ("  canon:   ", sapp (expr_to_str (lhs), sapp (" := ", expr_to_str (rhs)))))))
    | SCanon2 (loc, e0) =>
      w (sapp ("L", sapp (tostring_int (loc_line (loc)), sapp ("  canon2:  ", expr_to_str (e0)))))
    | SDirectSum (loc, a, b) =>
      w (sapp ("L", sapp (tostring_int (loc_line (loc)), sapp ("  dirsum:  ", sapp (expr_to_str (a), sapp (" * ", expr_to_str (b)))))))
    | SMarker (loc, a, nm) =>
      w (sapp ("L", sapp (tostring_int (loc_line (loc)), sapp ("  marker:  ", sapp (expr_to_str (a), sapp (" % ", nm))))))
    | SFrame (loc, n) =>
      w (sapp ("L", sapp (tostring_int (loc_line (loc)), sapp ("  frame:   ", n))))
    | SLock (loc, mtx, opname) =>
      w (sapp ("L", sapp (tostring_int (loc_line (loc)), sapp ("  lock:    ", sapp (mtx, sapp (", ", opname))))))
    | SUnlock (loc, mtx, opname) =>
      w (sapp ("L", sapp (tostring_int (loc_line (loc)), sapp ("  unlock:  ", sapp (mtx, sapp (", ", opname))))))
    | SCommute (loc, h1, h2) =>
      w (sapp ("L", sapp (tostring_int (loc_line (loc)), sapp ("  commute: ", sapp (h1, sapp (", ", h2))))))
    | SJoin (loc, _) =>
      w (sapp ("L", sapp (tostring_int (loc_line (loc)), "  join")))
    | SLoop (loc, h, x, k, _, body) =>
      let
        val () = w (sapp ("L", sapp (tostring_int (loc_line (loc)), sapp ("  loop:    ", sapp (h, sapp (", ", x))))))
        fun go (ss: List0(stmt_t)): void =
          case+ ss of
          | list_nil () => ()
          | list_cons (s0, rest) => begin emit_stmt (s0, ind + 1, buf); go (rest) end
      in go (body) end
    | SDecompose (loc, _, _, _, _, body) =>
      let
        val () = w (sapp ("L", sapp (tostring_int (loc_line (loc)), "  decompose")))
        fun go (ss: List0(stmt_t)): void =
          case+ ss of
          | list_nil () => ()
          | list_cons (s0, rest) => begin emit_stmt (s0, ind + 1, buf); go (rest) end
      in go (body) end
    | SInv (loc, _) => w (sapp ("L", sapp (tostring_int (loc_line (loc)), "  inv")))
    | SExit (loc, _) => w (sapp ("L", sapp (tostring_int (loc_line (loc)), "  exit")))
    | SMerge (loc, n, _) => w (sapp ("L", sapp (tostring_int (loc_line (loc)), sapp ("  merge ", n))))
    | SPivot (loc, n, _) => w (sapp ("L", sapp (tostring_int (loc_line (loc)), sapp ("  pivot ", n))))
    | SLive (loc, _) => w (sapp ("L", sapp (tostring_int (loc_line (loc)), "  live")))
    | SDeferred (loc, n, _) => w (sapp ("L", sapp (tostring_int (loc_line (loc)), sapp ("  defer ", n))))
    | SOnpie (loc, _) => w (sapp ("L", sapp (tostring_int (loc_line (loc)), "  onpie")))
    | SGath (loc, _) => w (sapp ("L", sapp (tostring_int (loc_line (loc)), "  gath")))
    | SBranch (loc, _, body) =>
      let
        val () = w (sapp ("L", sapp (tostring_int (loc_line (loc)), "  branch")))
        fun go (ss: List0(stmt_t)): void =
          case+ ss of
          | list_nil () => ()
          | list_cons (s0, rest) => begin emit_stmt (s0, ind + 1, buf); go (rest) end
      in go (body) end
    | SGhostOp (loc, e0) =>
      w (sapp ("L", sapp (tostring_int (loc_line (loc)), sapp ("  ghost:   ", expr_to_str (e0)))))
    | SGFun (loc, n, _, body) =>
      let
        val () = w (sapp ("L", sapp (tostring_int (loc_line (loc)), sapp ("  gfun:    ", n))))
        fun go (ss: List0(stmt_t)): void =
          case+ ss of
          | list_nil () => ()
          | list_cons (s0, rest) => begin emit_stmt (s0, ind + 1, buf); go (rest) end
      in go (body) end
    | SCompressBlk (loc, _, body) =>
      let
        val () = w (sapp ("L", sapp (tostring_int (loc_line (loc)), "  compress")))
        fun go (ss: List0(stmt_t)): void =
          case+ ss of
          | list_nil () => ()
          | list_cons (s0, rest) => begin emit_stmt (s0, ind + 1, buf); go (rest) end
      in go (body) end
    | SDimDecl (loc, _, _, _) => w (sapp ("L", sapp (tostring_int (loc_line (loc)), "  dim")))
    | SPole (loc, _, _) => w (sapp ("L", sapp (tostring_int (loc_line (loc)), "  pole")))
    | _ => w (sapp ("L", sapp (tostring_int (loc_line (stmt_loc (s))), "  unhandled")))
  end

implement emit_program (Program (funcs), src_file, out_file) =
  let
    val buf = ref<string>(sapp (".pu4\n  source:  ", sapp (src_file, "\n")))
    fun go_body (ss: List0(stmt_t)): void =
      case+ ss of
      | list_nil () => ()
      | list_cons (s, ss2) => begin emit_stmt (s, 1, buf); go_body (ss2) end
    fun go_funcs (fs: List0(func_t)): void =
      case+ fs of
      | list_nil () => ()
      | list_cons (Func (_, name, _, body), rest) =>
        let
          val () = !buf := sapp (!buf, sapp ("\noperator: ", sapp (name, "\n")))
          val () = go_body (body)
        in go_funcs (rest) end
    val () = go_funcs (funcs)
  in
    write_file (out_file, !buf)
  end
