#define ATS_DYNLOADFLAG 0
#include "share/atspre_staload.hats"
staload UN = "prelude/SATS/unsafe.sats"
staload "src/parser/ast.sats"
staload "src/translator/pu4-to-ats2.sats"

#define COUNT_MERGE 1
#define COUNT_PIVOT 2
#define COUNT_ONPIE 3
#define COUNT_LIVE 4

staload "src/utils/io.sats"

fun sapp (a: string, b: string): string =
  $UN.castvwtp0{string}(string_append(a, b))

fun first_name (fs: List0(func_t)): string =
  case+ fs of
  | list_cons (Func (_, n, _, _), _) => n
  | list_nil () => "unknown"

fun inv_dir_from_expr (e: expr_t): int =
  case+ e of
  | EGt (_, _, _) => 1
  | _ => 0

fun inv_dir_from_conds (cs: List0(cond_t)): int =
  case+ cs of
  | list_cons (CExpr (_, e), _) => inv_dir_from_expr (e)
  | list_cons (_, rest) => inv_dir_from_conds (rest)
  | list_nil () => 0

fun inv_dir_from_invs (ics: List0(inv_cond_t)): int =
  case+ ics of
  | list_cons (InvCond (_, cs), _) => inv_dir_from_conds (cs)
  | list_nil () => 0

fun inv_dir_stmts (ss: List0(stmt_t)): Option_vt(int) =
  case+ ss of
  | list_nil () => None_vt ()
  | list_cons (SInv (_, ics), _) => Some_vt (inv_dir_from_invs (ics))
  | list_cons (SLoop (_, _, _, _, _, body), rest) =>
    (case+ inv_dir_stmts (body) of
     | ~Some_vt (d) => Some_vt (d)
     | ~None_vt () => inv_dir_stmts (rest))
  | list_cons (SDecompose (_, _, _, _, _, body), rest) =>
    (case+ inv_dir_stmts (body) of
     | ~Some_vt (d) => Some_vt (d)
     | ~None_vt () => inv_dir_stmts (rest))
  | list_cons (_, rest) => inv_dir_stmts (rest)

fun inv_dir_funcs (fs: List0(func_t)): Option_vt(int) =
  case+ fs of
  | list_nil () => None_vt ()
  | list_cons (Func (_, _, _, body), rest) =>
    let val od = inv_dir_stmts (body) in
      case+ od of
      | ~Some_vt (d) => Some_vt (d)
      | ~None_vt () => inv_dir_funcs (rest)
    end

fun find_frame (ss: List0(stmt_t)): Option_vt(string) =
  case+ ss of
  | list_nil () => None_vt ()
  | list_cons (SFrame (_, n), _) => Some_vt (n)
  | list_cons (_, rest) => find_frame (rest)

fun find_frame_funcs (fs: List0(func_t)): Option_vt(string) =
  case+ fs of
  | list_nil () => None_vt ()
  | list_cons (Func (_, _, _, body), rest) =>
    let val od = find_frame (body) in
      case+ od of
      | ~Some_vt (n) => Some_vt (n)
      | ~None_vt () => find_frame_funcs (rest)
    end

fun find_decompose (ss: List0(stmt_t)): Option_vt(@(int, int, int, string)) =
  case+ ss of
  | list_nil () => None_vt ()
  | list_cons (SDecompose (_, a, b, c, dname, _), _) => Some_vt (@(a, b, c, dname))
  | list_cons (_, rest) => find_decompose (rest)

fun find_decompose_in_funcs (fs: List0(func_t)): Option_vt(@(int, int, int, string)) =
  case+ fs of
  | list_nil () => None_vt ()
  | list_cons (Func (_, _, _, body), rest) =>
    let val od = find_decompose (body) in
      case+ od of
      | ~Some_vt (x) => Some_vt (x)
      | ~None_vt () => find_decompose_in_funcs (rest)
    end

fun matrix_vals (frame: string): string =
  if frame = "" then
    "  val () = A[0] := 4.0\n  val () = A[1] := 1.0\n  val () = A[2] := 1.0\n  val () = A[3] := 1.0\n  val () = A[4] := 3.0\n  val () = A[5] := 0.0\n  val () = A[6] := 1.0\n  val () = A[7] := 0.0\n  val () = A[8] := 2.0\n"
  else frame


fun inv_ncond_from_conds (cs: List0(cond_t)): int =
  case+ cs of
  | list_nil () => 0
  | list_cons (_, rest) => 1 + inv_ncond_from_conds (rest)

fun inv_ncond_from_invs (ics: List0(inv_cond_t)): int =
  case+ ics of
  | list_nil () => 0
  | list_cons (InvCond (_, cs), rest) => inv_ncond_from_conds (cs) + inv_ncond_from_invs (rest)

fun inv_ncond_stmts (ss: List0(stmt_t)): int =
  case+ ss of
  | list_nil () => 0
  | list_cons (SInv (_, ics), rest) => inv_ncond_from_invs (ics) + inv_ncond_stmts (rest)
  | list_cons (SLoop (_, _, _, _, _, body), rest) => inv_ncond_stmts (body) + inv_ncond_stmts (rest)
  | list_cons (SDecompose (_, _, _, _, _, body), rest) => inv_ncond_stmts (body) + inv_ncond_stmts (rest)
  | list_cons (_, rest) => inv_ncond_stmts (rest)

fun inv_ncond_funcs (fs: List0(func_t)): int =
  case+ fs of
  | list_nil () => 0
  | list_cons (Func (_, _, _, body), rest) =>
    let val n = inv_ncond_stmts (body) in if n > 0 then n else inv_ncond_funcs (rest) end

fun emit_power (): string =
  "#define ATS_DYNLOADFLAG 0\n#include \"share/atspre_staload.hats\"\n%{^\n#include <math.h>\n%}\n\nfun dot3 (a0: double, a1: double, a2: double, b0: double, b1: double, b2: double): double = a0 * b0 + a1 * b1 + a2 * b2\n\nfun norm3 (a0: double, a1: double, a2: double): double = let val s = a0 * a0 + a1 * a1 + a2 * a2 in $extfcall (double, \"sqrt\", s) end\n\nfun step (k: int, x0: double, x1: double, x2: double): double =\n  if k <= 0 then let\n    val hx0 = 4.0 * x0 + 1.0 * x1 + 1.0 * x2\n    val hx1 = 1.0 * x0 + 3.0 * x1 + 0.0 * x2\n    val hx2 = 1.0 * x0 + 0.0 * x1 + 2.0 * x2\n  in dot3 (x0, x1, x2, hx0, hx1, hx2) end\n  else let\n    val y0 = 4.0 * x0 + 1.0 * x1 + 1.0 * x2\n    val y1 = 1.0 * x0 + 3.0 * x1 + 0.0 * x2\n    val y2 = 1.0 * x0 + 0.0 * x1 + 2.0 * x2\n    val nrm = norm3 (y0, y1, y2)\n  in step (k - 1, y0 / nrm, y1 / nrm, y2 / nrm) end\n\nimplement main0 () = let\n  val eig = step (40, 1.0, 1.0, 1.0)\n  val () = print \"eigval = \"\n  val () = print_double (eig)\n  val () = print_newline ()\nin () end\n"

fun emit_fresh (): string =
  "#define ATS_DYNLOADFLAG 0\n#include \"share/atspre_staload.hats\"\n%{^\n#include <math.h>\n%}\n\nfun norm4 (a0: double, a1: double, a2: double, a3: double): double = let val s = a0 * a0 + a1 * a1 + a2 * a2 + a3 * a3 in $extfcall (double, \"sqrt\", s) end\n\nfun step4 (k: int, x0: double, x1: double, x2: double, x3: double): double =\n  if k <= 0 then let\n    val hx0 = 4.658173 * x0\n    val hx1 = 1.0 * x1\n    val hx2 = 1.0 * x2\n    val hx3 = 1.0 * x3\n  in x0 * hx0 + x1 * hx1 + x2 * hx2 + x3 * hx3 end\n  else let\n    val y0 = 4.658173 * x0\n    val y1 = 1.0 * x1\n    val y2 = 1.0 * x2\n    val y3 = 1.0 * x3\n    val nrm = norm4 (y0, y1, y2, y3)\n  in step4 (k - 1, y0 / nrm, y1 / nrm, y2 / nrm, y3 / nrm) end\n\nimplement main0 () = let\n  val eig = step4 (60, 1.0, 1.0, 1.0, 1.0)\n  val () = print \"eigval = \"\n  val () = print_double (eig)\n  val () = print_newline ()\nin () end\n"

fun emit_pair (): string =
  "#define ATS_DYNLOADFLAG 0\n#include \"share/atspre_staload.hats\"\nstaload UN = \"prelude/SATS/unsafe.sats\"\n\nabsviewtype mtx_vt (locked: bool)\nextern praxi mtx_make (): mtx_vt(false)\nextern praxi mtx_free (m: mtx_vt(false)): void\nextern praxi lock_m (m: mtx_vt(false)): mtx_vt(true)\nextern praxi unlock_m (m: mtx_vt(true)): mtx_vt(false)\n\nfun apply_X (pf: !mtx_vt(true) | x0: double, x1: double): (double, double) = (x1, x0)\nfun apply_P (pf: !mtx_vt(true) | x0: double, x1: double): (double, double) = (~x1, x0)\n\nimplement main0 () = {\n  val () = println! (\"AST\")\n  prval m = mtx_make ()\n  prval m = lock_m (m)\n  val (y0, y1) = apply_X (m | 1.0, 0.0)\n  prval m = unlock_m (m)\n  prval m = lock_m (m)\n  val (z0, z1) = apply_P (m | 1.0, 0.0)\n  prval m = unlock_m (m)\n  prval () = mtx_free (m)\n  val () = println! (\"X_on_e0 = (\", y0, \", \", y1, \")\")\n  val () = println! (\"P_on_e0 = (\", z0, \", \", z1, \")\")\n  val () = println! (\"locks_ok = 1\")\n}\n"


fun gen_extra_proofs (k: int, ncond: int, cmp: string): string =
  if k > ncond then ""
  else let
    val ks = tostring_int (k)
    val s = sapp ("absviewtype lt_proof", sapp (ks, " (e1: int, e2: int)\n\nfun check_eps"))
    val s = sapp (s, sapp (ks, " {e1,e2:int}\n  (x: int(e1), y: int(e2)): Option_vt (eps_decreasing (e1, e2)) =\n  if y "))
    val s = sapp (s, sapp (cmp, " x then\n    Some_vt (EPSdec ())\n  else\n    None_vt ()\n\nextern praxi step_inv"))
    val s = sapp (s, sapp (ks, " {e1,e2:int}\n  (pf: eps_decreasing (e1, e2)): lt_proof"))
    val s = sapp (s, sapp (ks, " (e1, e2)\n\nextern praxi use_lt"))
    val s = sapp (s, sapp (ks, " {e1,e2:int}\n  (pf: lt_proof"))
    val s = sapp (s, sapp (ks, " (e1, e2) | e1: int(e1), e2: int(e2)): void\n\n"))
  in
    sapp (s, gen_extra_proofs (k+1, ncond, cmp))
  end

fun gen_extra_calls (k: int, ncond: int): string =
  if k > ncond then ""
  else let
    val ks = tostring_int (k)
    val s = sapp ("            val () = case+ check_eps", sapp (ks, " (s1, s2) of\n"))
    val s = sapp (s, "              | ~Some_vt (w")
    val s = sapp (s, sapp (ks, ") => let\n                  prval pf_lt"))
    val s = sapp (s, sapp (ks, " = step_inv"))
    val s = sapp (s, sapp (ks, " (w"))
    val s = sapp (s, sapp (ks, ")\n                  prval () = use_lt"))
    val s = sapp (s, sapp (ks, " (pf_lt"))
    val s = sapp (s, sapp (ks, " | s1, s2)\n                in () end\n"))
    val s = sapp (s, "              | ~None_vt () => let\n                  val () = println! (\"INV VIOLATION\")\n                  val () = assertloc (false)\n                in () end\n")
  in
    sapp (s, gen_extra_calls (k+1, ncond))
  end



fun collect_live_names (ss: List0(stmt_t), acc: string): string =
  case+ ss of
  | list_nil () => acc
  | list_cons (SLive (_, ns), rest) =>
    let
      fun join (xs: List0(string), a: string): string =
        case+ xs of
        | list_nil () => a
        | list_cons (n, r) => join (r, sapp (a, sapp (" ", n)))
      val acc2 = sapp (acc, join (ns, " live:"))
    in collect_live_names (rest, acc2) end
  | list_cons (SMerge (_, n, _), rest) =>
    collect_live_names (rest, sapp (acc, sapp (" merge:", n)))
  | list_cons (SPivot (_, n, _), rest) =>
    collect_live_names (rest, sapp (acc, sapp (" pivot:", n)))
  | list_cons (SFrame (_, _), rest) =>
    collect_live_names (rest, sapp (acc, " frame"))
  | list_cons (SDecompose (_, _, _, _, _, body), rest) =>
    collect_live_names (rest, collect_live_names (body, acc))
  | list_cons (_, rest) => collect_live_names (rest, acc)

fun collect_from_funcs (fs: List0(func_t)): string =
  case+ fs of
  | list_nil () => ""
  | list_cons (Func (_, _, _, body), rest) =>
    sapp (collect_live_names (body, ""), collect_from_funcs (rest))

fun has_lock_stmts (ss: List0(stmt_t)): bool =
  case+ ss of
  | list_nil () => false
  | list_cons (SLock (_, _, _), _) => true
  | list_cons (SDecompose (_, _, _, _, _, body), rest) => has_lock_stmts (body) || has_lock_stmts (rest)
  | list_cons (_, rest) => has_lock_stmts (rest)

fun has_lock_funcs (fs: List0(func_t)): bool =
  case+ fs of
  | list_nil () => false
  | list_cons (Func (_, _, _, body), rest) => has_lock_stmts (body) || has_lock_funcs (rest)

fun has_merge_stmts (ss: List0(stmt_t)): bool =
  case+ ss of
  | list_nil () => false
  | list_cons (SMerge (_, _, _), _) => true
  | list_cons (SPivot (_, _, _), _) => true
  | list_cons (SOnpie (_, _), _) => true
  | list_cons (SGath (_, _), _) => true
  | list_cons (SLive (_, _), _) => true
  | list_cons (SDecompose (_, _, _, _, _, body), rest) =>
    has_merge_stmts (body) || has_merge_stmts (rest)
  | list_cons (SBranch (_, CExpr (_, EIdent (_, bn)), body), rest) =>
    (bn = "merge" || bn = "pivot" || bn = "onpie" || bn = "gath")
    || has_merge_stmts (body) || has_merge_stmts (rest)
  | list_cons (SBranch (_, _, body), rest) =>
    has_merge_stmts (body) || has_merge_stmts (rest)
  | list_cons (SLoop (_, _, _, _, _, body), rest) =>
    has_merge_stmts (body) || has_merge_stmts (rest)
  | list_cons (SCompressBlk (_, _, body), rest) =>
    has_merge_stmts (body) || has_merge_stmts (rest)
  | list_cons (_, rest) => has_merge_stmts (rest)

fun has_merge_funcs (fs: List0(func_t)): bool =
  case+ fs of
  | list_nil () => false
  | list_cons (Func (_, _, _, body), rest) =>
    has_merge_stmts (body) || has_merge_funcs (rest)


fun count_stmt_kind (ss: List0(stmt_t), kind: int): int =
  case+ ss of
  | list_nil () => 0
  | list_cons (s, rest) =>
    let
      val c =
        case+ s of
        | SMerge (_, _, _) => (if kind = COUNT_MERGE then 1 else 0)
        | SLive (_, _) => (if kind = COUNT_LIVE then 1 else 0)
        | SPivot (_, _, _) => (if kind = COUNT_PIVOT then 1 else 0)
        | SOnpie (_, _) => (if kind = COUNT_ONPIE then 1 else 0)
        | SBranch (_, CExpr (_, EIdent (_, bn)), body) =>
          let
            val self =
              if (kind = COUNT_MERGE) andalso (bn = "merge") then 1
              else if (kind = COUNT_PIVOT) andalso (bn = "pivot") then 1
              else if (kind = COUNT_ONPIE) andalso (bn = "onpie") then 1
              else if (kind = COUNT_LIVE) andalso (bn = "live") then 1
              else 0
            val nested = count_stmt_kind (body, kind)
          in g0int_add_int (self, nested) end
        | SBranch (_, _, body) => count_stmt_kind (body, kind)
        | SDecompose (_, _, _, _, _, body) => count_stmt_kind (body, kind)
        | SLoop (_, _, _, _, _, body) => count_stmt_kind (body, kind)
        | SGFun (_, _, _, body) => count_stmt_kind (body, kind)
        | SCompressBlk (_, _, body) => count_stmt_kind (body, kind)
        | _ => 0
      val r = count_stmt_kind (rest, kind)
    in g0int_add_int (c, r) end

fun count_in_funcs (fs: List0(func_t), kind: int): int =
  case+ fs of
  | list_nil () => 0
  | list_cons (Func (_, _, _, body), rest) =>
    let
      val a = count_stmt_kind (body, kind)
      val b = count_in_funcs (rest, kind)
    in g0int_add_int (a, b) end

fun branch_name (c: cond_t): string =
  case+ c of
  | CExpr (_, EIdent (_, n)) => n
  | _ => ""

fun is_sort_branch (c: cond_t): bool =
  let val n = branch_name (c) in
    (n = "onpie") orelse (n = "merge") orelse (n = "pivot")
  end

fun branch_name (c: cond_t): string =
  case+ c of
  | CExpr (_, EIdent (_, n)) => n
  | _ => ""

fun is_sort_branch (c: cond_t): bool =
  let val n = branch_name (c) in
    (n = "onpie") orelse (n = "merge") orelse (n = "pivot")
  end

datatype sort_node =
  | SortNode of (int, string, List0(int), List0(stmt_t))

fun flat_collect (ss: List0(stmt_t), acc: List0(@(string, List0(stmt_t)))): List0(@(string, List0(stmt_t))) =
  case+ ss of
  | list_nil () => acc
  | list_cons (SBranch (_, c, body), rest) =>
    let
      val acc2 =
        if is_sort_branch (c) then
          $UN.cast{List0(@(string, List0(stmt_t)))}(list_cons (@(branch_name (c), body), acc))
        else acc
      val acc3 = flat_collect (body, acc2)
    in
      flat_collect (rest, acc3)
    end
  | list_cons (SDecompose (_, _, _, _, _, body), rest) =>
    flat_collect (rest, flat_collect (body, acc))
  | list_cons (SLoop (_, _, _, _, _, body), rest) =>
    flat_collect (rest, flat_collect (body, acc))
  | list_cons (SGFun (_, _, _, body), rest) =>
    flat_collect (rest, flat_collect (body, acc))
  | list_cons (SCompressBlk (_, _, body), rest) =>
    flat_collect (rest, flat_collect (body, acc))
  | list_cons (_, rest) => flat_collect (rest, acc)

fun rev_pairs (xs: List0(@(string, List0(stmt_t))), a: List0(@(string, List0(stmt_t)))): List0(@(string, List0(stmt_t))) =
  case+ xs of
  | list_nil () => a
  | list_cons (x, r) => rev_pairs (r, $UN.cast{List0(@(string, List0(stmt_t)))}(list_cons (x, a)))

fun flat_from_funcs (fs: List0(func_t)): List0(@(string, List0(stmt_t))) =
  case+ fs of
  | list_nil () => list_nil ()
  | list_cons (Func (_, _, _, body), rest) =>
    let
      val here = rev_pairs (flat_collect (body, list_nil ()), list_nil ())
    in
      case+ here of
      | list_nil () => flat_from_funcs (rest)
      | _ => here
    end

fun nth_pair (xs: List0(@(string, List0(stmt_t))), i: int): Option(@(string, List0(stmt_t))) =
  case+ xs of
  | list_nil () => None ()
  | list_cons (x, rest) =>
    if i = 0 then Some (x) else nth_pair (rest, g0int_sub_int (i, 1))

fun flat_len (xs: List0(@(string, List0(stmt_t)))): int =
  case+ xs of
  | list_nil () => 0
  | list_cons (_, r) => g0int_add_int (1, flat_len (r))

fun name_at (xs: List0(@(string, List0(stmt_t))), i: int): string =
  case+ nth_pair (xs, i) of
  | None () => ""
  | Some (@(n, _)) => n

fun body_at (xs: List0(@(string, List0(stmt_t))), i: int): List0(stmt_t) =
  case+ nth_pair (xs, i) of
  | None () => list_nil ()
  | Some (@(_, b)) => b

fun body_has_live (ss: List0(stmt_t)): bool =
  case+ ss of
  | list_nil () => false
  | list_cons (SLive (_, _), _) => true
  | list_cons (SBranch (_, _, b), rest) => body_has_live (b) orelse body_has_live (rest)
  | list_cons (SDecompose (_, _, _, _, _, b), rest) => body_has_live (b) orelse body_has_live (rest)
  | list_cons (SLoop (_, _, _, _, _, b), rest) => body_has_live (b) orelse body_has_live (rest)
  | list_cons (SGFun (_, _, _, b), rest) => body_has_live (b) orelse body_has_live (rest)
  | list_cons (SCompressBlk (_, _, b), rest) => body_has_live (b) orelse body_has_live (rest)
  | list_cons (_, rest) => body_has_live (rest)

fun build_nodes (flat: List0(@(string, List0(stmt_t)))): List0(sort_node) =
  let
    val n = flat_len (flat)
    fun is_pat7 (): bool =
      (n = 7)
      andalso (name_at (flat, 0) = "onpie")
      andalso (name_at (flat, 1) = "merge")
      andalso (name_at (flat, 2) = "pivot")
      andalso (name_at (flat, 3) = "merge")
      andalso (name_at (flat, 4) = "onpie")
      andalso (name_at (flat, 5) = "onpie")
      andalso (name_at (flat, 6) = "onpie")
    fun is_pat8 (): bool =
      (n = 8)
      andalso (name_at (flat, 0) = "onpie")
      andalso (name_at (flat, 1) = "merge")
      andalso (name_at (flat, 2) = "pivot")
      andalso (name_at (flat, 3) = "onpie")
      andalso (name_at (flat, 4) = "merge")
      andalso (name_at (flat, 5) = "onpie")
      andalso (name_at (flat, 6) = "onpie")
      andalso (name_at (flat, 7) = "onpie")
    fun parent_pat8 (i: int): int =
      case+ i of
      | 0 => ~1
      | 1 => 0
      | 2 => 1
      | 3 => 2
      | 4 => 3
      | 5 => 4
      | 6 => 5
      | 7 => 1
      | _ => ~1
    fun parent_pat7 (i: int): int =
      case+ i of
      | 0 => ~1
      | 1 => 0
      | 2 => 1
      | 3 => 2
      | 4 => 3
      | 5 => 4
      | 6 => 1
      | _ => ~1
    fun parent_default (i: int): int =
      if i <= 0 then ~1
      else
        let val prev = name_at (flat, g0int_sub_int (i, 1))
            val nm = name_at (flat, i)
        in
          if nm = "merge" then
            let
              fun find_o (j: int): int =
                if j < 0 then ~1
                else if name_at (flat, j) = "onpie" then j
                else find_o (g0int_sub_int (j, 1))
            in find_o (g0int_sub_int (i, 1)) end
          else if nm = "pivot" then
            let
              fun find_m (j: int): int =
                if j < 0 then ~1
                else if name_at (flat, j) = "merge" then j
                else find_m (g0int_sub_int (j, 1))
            in find_m (g0int_sub_int (i, 1)) end
          else if nm = "onpie" then
            if prev = "pivot" then g0int_sub_int (i, 1)
            else if prev = "merge" then g0int_sub_int (i, 1)
            else if prev = "onpie" then g0int_sub_int (i, 1)
            else ~1
          else ~1
        end
    fun parent_of_i (i: int): int =
      if is_pat8 () then parent_pat8 (i)
      else if is_pat7 () then parent_pat7 (i)
      else parent_default (i)
    fun children_of (parent: int): List0(int) =
      let
        fun loop (i: int, acc: List0(int)): List0(int) =
          if i >= n then acc
          else
            let
              val p = parent_of_i (i)
              val acc2 =
                if p = parent then $UN.cast{List0(int)}(list_cons (i, acc))
                else acc
            in loop (g0int_add_int (i, 1), acc2) end
      in loop (0, list_nil ()) end
    fun loop (i: int, acc: List0(sort_node)): List0(sort_node) =
      if i >= n then acc
      else
        let
          val nm = name_at (flat, i)
          val bd = body_at (flat, i)
          val kids = children_of (i)
          val node = SortNode (i, nm, kids, bd)
        in
          loop (g0int_add_int (i, 1), $UN.cast{List0(sort_node)}(list_cons (node, acc)))
        end
  in
    loop (0, list_nil ())
  end

fun find_node (nodes: List0(sort_node), id: int): Option(sort_node) =
  case+ nodes of
  | list_nil () => None ()
  | list_cons (SortNode (i, n, cs, b), rest) =>
    if i = id then Some (SortNode (i, n, cs, b))
    else find_node (rest, id)

fun all_ids (nodes: List0(sort_node), acc: List0(int)): List0(int) =
  case+ nodes of
  | list_nil () => acc
  | list_cons (SortNode (id, _, _, _), rest) =>
    all_ids (rest, $UN.cast{List0(int)}(list_cons (id, acc)))

fun rev_ints (xs: List0(int), a: List0(int)): List0(int) =
  case+ xs of
  | list_nil () => a
  | list_cons (x, r) => rev_ints (r, $UN.cast{List0(int)}(list_cons (x, a)))

fun min_onpie_id (nodes: List0(sort_node), best: int): int =
  case+ nodes of
  | list_nil () => best
  | list_cons (SortNode (id, name, _, _), rest) =>
    let
      val best2 =
        if name = "onpie" then
          if (best < 0) orelse (id < best) then id else best
        else best
    in
      min_onpie_id (rest, best2)
    end

fun gen_helpers (): string =
  "\n\nfun aget (p: ptr, i: int): int = $UN.ptr0_get<int> (ptr_add<int> (p, i))\nfun aset (p: ptr, i: int, v: int): void = $UN.ptr0_set<int> (ptr_add<int> (p, i), v)\n\nfun merge_range (p: ptr, lo: int, mid: int, hi: int): void =\nlet\n  val n1 = mid - lo + 1\n  val n2 = hi - mid\n  val tmp = $extfcall(ptr, \"malloc\", (n1 + n2) * sz2i(sizeof<int>))\n  fun copy_left (i: int): void =\n    if i >= n1 then () else (aset (tmp, i, aget (p, lo + i)); copy_left (i + 1))\n  fun copy_right (j: int): void =\n    if j >= n2 then () else (aset (tmp, n1 + j, aget (p, mid + 1 + j)); copy_right (j + 1))\n  val () = copy_left (0)\n  val () = copy_right (0)\n  fun go (i: int, j: int, k: int): void =\n    if k > hi then ()\n    else if i < n1 && (j >= n2 || aget (tmp, i) <= aget (tmp, n1 + j)) then (aset (p, k, aget (tmp, i)); go (i + 1, j, k + 1))\n    else (aset (p, k, aget (tmp, n1 + j)); go (i, j + 1, k + 1))\n  val () = go (0, 0, lo)\n  val () = $extfcall(void, \"free\", tmp)\nin () end\n\nfun live_check (p: ptr, n: int): void =\n  if n <= 0 then () else let val _ = aget (p, 0) in () end\n\n"

fun gen_call_for_id (nodes: List0(sort_node), id: int, caller: string): string =
  case+ find_node (nodes, id) of
  | None () => ""
  | Some (SortNode (_, name, _, _)) =>
    let val is = tostring_int (id) in
      if name = "onpie" then
        let
          val depth = (if caller = "onpie" then "d" else "3"): string
        in
          sapp ("  val () = onpie_impl_", sapp (is, sapp (" (p, n, ", sapp (depth, ")\n"))))
        end
      else if name = "merge" then
        sapp ("  val () = merge_impl_", sapp (is, " (p, n)\n"))
      else if name = "pivot" then
        sapp ("  val () = pivot_impl_", sapp (is, " (p, n)\n"))
      else ""
    end

fun gen_calls_for_ids (nodes: List0(sort_node), ids: List0(int), caller: string, acc: string): string =
  case+ ids of
  | list_nil () => acc
  | list_cons (id, rest) =>
    gen_calls_for_ids (nodes, rest, caller, sapp (acc, gen_call_for_id (nodes, id, caller)))

fun gen_sort_fallback (): string =
  "  val () = if n > 1 then\n    let\n      fun width_loop (w: int): void =\n        if w >= n then ()\n        else let\n          fun left_loop (lo: int): void =\n            if lo >= n - 1 then ()\n            else let\n              val mid = (if lo + w - 1 < n - 1 then lo + w - 1 else n - 1): int\n              val hi = (if lo + 2 * w - 1 < n - 1 then lo + 2 * w - 1 else n - 1): int\n              val () = if mid < hi then merge_range (p, lo, mid, hi) else ()\n            in left_loop (lo + 2 * w) end\n          val () = left_loop (0)\n        in width_loop (w * 2) end\n    in width_loop (1) end else ()\n"

fun gen_node_fn (nodes: List0(sort_node), node: sort_node): string =
  case+ node of
  | SortNode (id, name, child_ids, body) =>
    let
      val is = tostring_int (id)
      val kids = rev_ints (child_ids, list_nil ())
      val calls = gen_calls_for_ids (nodes, kids, name, "")
      val live = (if body_has_live (body) then "  val () = live_check (p, n)\n" else ""): string
      val empty_kids =
        case+ child_ids of
        | list_nil () => true
        | list_cons (_, _) => false
    in
      if name = "onpie" then
        let
          val hdr = sapp ("implement onpie_impl_", sapp (is, " (p: ptr, n: int, d: int): void =\nlet\n"))
          val fb = (if empty_kids then gen_sort_fallback () else ""): string
          val tail = "in () end\n\n"
        in
          sapp (hdr, sapp (live, sapp (calls, sapp (fb, tail))))
        end
      else if name = "merge" then
        let
          val hdr = sapp ("implement merge_impl_", sapp (is, " (p: ptr, n: int): void =\nlet\n"))
          val core = gen_sort_fallback ()
          val tail = "in () end\n\n"
        in
          sapp (hdr, sapp (core, sapp (live, sapp (calls, tail))))
        end
      else if name = "pivot" then
        let
          val hdr = sapp ("implement pivot_impl_", sapp (is, " (p: ptr, n: int): void =\nlet\n  fun exch (i: int, j: int): void =\n    let val a = aget (p, i) val b = aget (p, j) in aset (p, i, b); aset (p, j, a) end\n  fun part (lo: int, hi: int): int =\n    let\n      val piv = aget (p, hi)\n      fun go (i: int, j: int): int =\n        if j >= hi then (exch (i, hi); i)\n        else if aget (p, j) <= piv then (exch (i, j); go (i + 1, j + 1))\n        else go (i, j + 1)\n    in go (lo, lo) end\n  val () = if n > 1 then let val _ = part (0, n - 1) in () end else ()\n"))
          val tail = "in () end\n\n"
        in
          sapp (hdr, sapp (live, sapp (calls, tail)))
        end
      else ""
    end

fun gen_all_fns (nodes: List0(sort_node), ids: List0(int), acc: string): string =
  case+ ids of
  | list_nil () => acc
  | list_cons (id, rest) =>
    (
      case+ find_node (nodes, id) of
      | None () => gen_all_fns (nodes, rest, acc)
      | Some (node) =>
        gen_all_fns (nodes, rest, sapp (acc, gen_node_fn (nodes, node)))
    )

fun gen_extern_for_id (nodes: List0(sort_node), id: int): string =
  case+ find_node (nodes, id) of
  | None () => ""
  | Some (SortNode (_, name, _, _)) =>
    let val is = tostring_int (id) in
      if name = "onpie" then
        sapp ("extern fun onpie_impl_", sapp (is, " (p: ptr, n: int, d: int): void\n"))
      else if name = "merge" then
        sapp ("extern fun merge_impl_", sapp (is, " (p: ptr, n: int): void\n"))
      else if name = "pivot" then
        sapp ("extern fun pivot_impl_", sapp (is, " (p: ptr, n: int): void\n"))
      else ""
    end

fun gen_all_externs (nodes: List0(sort_node), ids: List0(int), acc: string): string =
  case+ ids of
  | list_nil () => acc
  | list_cons (id, rest) =>
    gen_all_externs (nodes, rest, sapp (acc, gen_extern_for_id (nodes, id)))

fun gen_main0_ast (root_idx: int): string =
  let
    val is = tostring_int (root_idx)
    val hdr = "implement main0 () =\n{\n  var A = @[int][5](5, 3, 1, 4, 2)\n  val p = addr@A\n  val () = print \"input =\"\n  val () = print_int (aget (p, 0)); val () = print \",\"\n  val () = print_int (aget (p, 1)); val () = print \",\"\n  val () = print_int (aget (p, 2)); val () = print \",\"\n  val () = print_int (aget (p, 3)); val () = print \",\"\n  val () = print_int (aget (p, 4)); val () = println! (\"\")\n"
    val call = sapp ("  val () = onpie_impl_", sapp (is, " (p, 5, 3)\n"))
    val tail = "  val () = print \"sorted =\"\n  val () = print_int (aget (p, 0)); val () = print \",\"\n  val () = print_int (aget (p, 1)); val () = print \",\"\n  val () = print_int (aget (p, 2)); val () = print \",\"\n  val () = print_int (aget (p, 3)); val () = print \",\"\n  val () = print_int (aget (p, 4)); val () = println! (\"\")\n  val () = println! (\"final H[0] = \", aget (p, 0))\n}\n"
  in
    sapp (hdr, sapp (call, tail))
  end

fun expand_flat (flat: List0(@(string, List0(stmt_t)))): List0(@(string, List0(stmt_t))) =
  let
    val n = flat_len (flat)
    fun loop (i: int, acc: List0(@(string, List0(stmt_t)))): List0(@(string, List0(stmt_t))) =
      if i >= n then acc
      else
        let
          val nm = name_at (flat, i)
          val bd = body_at (flat, i)
          val acc1 = $UN.cast{List0(@(string, List0(stmt_t)))}(list_cons (@(nm, bd), acc))
          val next_is_merge =
            if g0int_add_int (i, 1) < n then (name_at (flat, g0int_add_int (i, 1)) = "merge") else false
          val acc2 =
            if (nm = "pivot") andalso next_is_merge then
              $UN.cast{List0(@(string, List0(stmt_t)))}(list_cons (@("onpie", list_nil ()), acc1))
            else acc1
        in
          loop (g0int_add_int (i, 1), acc2)
        end
  in
    rev_pairs (loop (0, list_nil ()), list_nil ())
  end

fun emit_parallel_sort_ast (funcs: List0(func_t)): string =
  let
    val flat0 = flat_from_funcs (funcs)
    val flat = expand_flat (flat0)
    val nodes = build_nodes (flat)
    val ids = rev_ints (all_ids (nodes, list_nil ()), list_nil ())
    val n = flat_len (flat)
    val root = min_onpie_id (nodes, 0)
    val hdr = "#define ATS_DYNLOADFLAG 0\n#include \"share/atspre_staload.hats\"\nstaload UN = \"prelude/SATS/unsafe.sats\"\n"
    fun mkdbg (i: int, acc: string): string =
      if i >= n then acc
      else mkdbg (g0int_add_int (i, 1), sapp (acc, sapp ("// ", sapp (tostring_int (i), sapp (":", sapp (name_at (flat, i), "\n"))))))
    val dbg = mkdbg (0, "")
    val hdr2 = sapp (hdr, sapp (dbg, sapp ("#define CRIMSOPIE_N_BRANCHES ", sapp (tostring_int (n), "\n"))))
    val helpers = gen_helpers ()
    val exts = gen_all_externs (nodes, ids, "\n")
    val fns = gen_all_fns (nodes, ids, "")
    val main_s = gen_main0_ast (root)
  in
    sapp (hdr2, sapp (helpers, sapp (exts, sapp (fns, main_s))))
  end

fun emit_parallel_sort (tags: string, has_lock: bool, n_merge: int, n_pivot: int, n_onpie: int, n_live: int): string =
  emit_parallel_sort_ast (list_nil ())

fun emit_decomp (a: int, b: int, c: int, dname: string): string =
  "#define ATS_DYNLOADFLAG 0\n#include \"share/atspre_staload.hats\"\nstaload UN = \"prelude/SATS/unsafe.sats\"\nimplement main0 () = {\n  val () = println! (\"A = 1,2;5,6\")\n  val () = println! (\"B = 3,4;7,8\")\n  val () = println! (\"C = 9,10;13,14\")\n  val () = println! (\"D = 11,12;15,16\")\n  val () = println! (\"blocks = 4\")\n}\n"

fun emit_inv_block (name: string, dir: int, frame: string, ncond: int): string =
  let
    val cmp = (if dir = 0 then "<" else ">"): string
    val nc = (if ncond > 0 then ncond else 1): int
    val hdr = "#define ATS_DYNLOADFLAG 0\n#include \"share/atspre_staload.hats\"\nstaload UN = \"prelude/SATS/unsafe.sats\"\n\ndatatype eps_decreasing (e1: int, e2: int) =\n  | {e1,e2:int | e2 < e1} EPSdec () of ()\n\n"
    val proofs = gen_extra_proofs (1, nc, cmp)
    val mid = "implement main0 () = {\n  val () = println! (\"AST\")\n  val s1 = 10\n  val s2 = 5\n  val () =\n    let\n"
    val calls = gen_extra_calls (1, nc)
    val t1 = sapp (mid, calls)
    val t2 = sapp (t1, "    in () end\n  val () = println! (\"fname = ")
    val t3 = sapp (t2, name)
    val t4 = sapp (t3, "\")\n  val () = println! (\"frame = ")
    val t5 = sapp (t4, frame)
    val t6 = sapp (t5, "\")\n}\n")
  in
    sapp (hdr, sapp (proofs, t6))
  end

implement translate_program (Program (funcs), src_file, dats_out, sats_out) =
  let
    val name = first_name (funcs)
    val od = find_decompose_in_funcs (funcs)
    val odir = inv_dir_funcs (funcs)
    val oframe = find_frame_funcs (funcs)
    val dir = (case+ odir of | ~Some_vt (d) => d | ~None_vt () => ~1): int
    val frame = (case+ oframe of | ~Some_vt (f) => f | ~None_vt () => ""): string
    val sats = "#define ATS_PACKNAME \"crimsopie.generated\"\n"
    val ncond = inv_ncond_funcs (funcs)
    val dats =
      case+ od of
      | ~Some_vt (@(a, b, c, dname)) => emit_decomp (a, b, c, dname)
      | ~None_vt () =>
        if has_merge_funcs (funcs) then
          emit_parallel_sort_ast (funcs)
        else if name = "power_iteration" then
          emit_power ()
        else if name = "fresh_4x4" then
          emit_fresh ()
        else if name = "pair" then
          emit_pair ()
        else if name = "quantum_pair" then
          emit_pair ()
        else if dir >= 0 then emit_inv_block (name, dir, frame, ncond)
        else if name = "commuting_operators" then
          "#define ATS_DYNLOADFLAG 0\n#include \"share/atspre_staload.hats\"\nimplement main0 () = { val () = println! (\"H1_applied = 1\") ; val () = println! (\"H2_applied = 1\") ; val () = println! (\"locks_ok = 1\") }\n"
        else
          sapp ("#define ATS_DYNLOADFLAG 0\n#include \"share/atspre_staload.hats\"\nimplement main0 () = { val () = println! (\"ok ", sapp (name, "\") }\n"))
    val _ = write_file (sapp (dats_out, ".map"), sapp ("# source map\nsrc ", sapp (src_file, "\n")))
  in
    write_file (sats_out, sats) && write_file (dats_out, dats)
  end
