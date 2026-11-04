#define ATS_DYNLOADFLAG 0
#include "share/atspre_staload.hats"
staload UN = "prelude/SATS/unsafe.sats"
staload "src/parser/ast.sats"
staload "src/checker/env.sats"
staload "src/errors/error.sats"
staload "src/checker/verify.sats"

fun op_name_of_expr (e: expr_t): string =
  case+ e of
  | EIdent (_, n) => n
  | ESubscript (_, n, _) => n
  | EOpRef (_, n, _) => n
  | EActRef (_, n) => n
  | EPipe (_, e0, _) => op_name_of_expr (e0)
  | ENorm (_, e0) => op_name_of_expr (e0)
  | EDiv (_, e0, _) => op_name_of_expr (e0)
  | EMul (_, e0, _) => op_name_of_expr (e0)
  | EAdd (_, e0, _) => op_name_of_expr (e0)
  | ESub (_, e0, _) => op_name_of_expr (e0)
  | EPow (_, e0, _) => op_name_of_expr (e0)
  | ELt (_, e0, _) => op_name_of_expr (e0)
  | EGt (_, e0, _) => op_name_of_expr (e0)
  | EEq (_, e0, _) => op_name_of_expr (e0)
  | EConj (_, e0, _) => op_name_of_expr (e0)
  | _ => ""

fun collect_all_plus (ss: List0(stmt_t), acc: List0(string)): List0(string) =
  case+ ss of
  | list_nil () => acc
  | list_cons (SApply (_, lhs, _, _), rest) =>
    collect_all_plus (rest, $UN.cast{List0(string)}(list_cons (op_name_of_expr (lhs), acc)))
  | list_cons (SGhostOp (_, e0), rest) =>
    collect_all_plus (rest, $UN.cast{List0(string)}(list_cons (op_name_of_expr (e0), acc)))
  | list_cons (SBranch (_, _, body), rest) =>
    collect_all_plus (rest, collect_all_plus (body, acc))
  | list_cons (SDecompose (_, _, _, _, _, body), rest) =>
    collect_all_plus (rest, collect_all_plus (body, acc))
  | list_cons (SLoop (_, _, _, _, _, body), rest) =>
    collect_all_plus (rest, collect_all_plus (body, acc))
  | list_cons (SGFun (_, _, _, body), rest) =>
    collect_all_plus (rest, collect_all_plus (body, acc))
  | list_cons (SCompressBlk (_, _, body), rest) =>
    collect_all_plus (rest, collect_all_plus (body, acc))
  | list_cons (_, rest) => collect_all_plus (rest, acc)

fun introduce_all (e: env_vt, names: List0(string), line: int, col: int): env_vt =
  case+ names of
  | list_nil () => e
  | list_cons (n, rest) =>
    let
      val __r = env_is_bound (e, n)
    val+ ~BE (bound, e) = __r
      val e =
        (if bound then e else env_introduce (e, n, line, col, false)) : env_vt
    in
      introduce_all (e, rest, line, col)
    end

fun onpie_letrec_intro (e: env_vt, body: List0(stmt_t), line: int, col: int): env_vt =
  introduce_all (e, collect_all_plus (body, list_nil ()), line, col)

fun validate_apply (e: env_vt, opname: string, line: int, col: int, file: string): bool_env =
  let
    val __r = env_is_bound (e, opname)
    val+ ~BE (bound, e) = __r
    val __r = env_get_status (e, opname)
    val+ ~IE (st, e) = __r
    val __r = env_is_locked (e, opname)
    val+ ~BE (locked, e) = __r
    val __r = env_is_shared (e, opname)
    val+ ~BE (shared, e) = __r
  in
    if ~bound then BE (true, e)
    else if st = 0 then BE (true, e)
    else if st = 4 then BE (true, e)
    else if st = 1 andalso ~locked andalso ~shared then
      let
        val () = error_emit (mk_linearity_violation (line, col, file, opname,
          "applied twice without ~ or @ lock"))
      in BE (false, e) end
    else if st = 2 andalso ~locked andalso ~shared then
      let
        val () = error_emit (mk_linearity_violation (line, col, file, opname,
          "re-applied after elimination without ~ or @ lock"))
      in BE (false, e) end
    else if st = 3 andalso ~locked andalso ~shared then
      let
        val () = error_emit (mk_linearity_violation (line, col, file, opname,
          "re-applied after canon without ~ or @ lock"))
      in BE (false, e) end
    else BE (true, e)
  end

fun apply_apply (e: env_vt, opname: string, line: int, col: int): env_vt =
  let
    val __r = env_is_bound (e, opname)
    val+ ~BE (bound, e) = __r
    val e =
      (if ~bound then env_introduce (e, opname, line, col, false) else e) : env_vt
    val __r = env_apply (e, opname, line, col)
    val+ ~BE (_, e) = __r
  in e end

implement check_stmt (s, e) =
  case+ s of
  | SApply (loc, lhs, _, _) =>
    let
      val line = loc_line (loc)
      val col = loc_col (loc)
      val file = loc_file (loc)
      val opname = op_name_of_expr (lhs)
      val __r = validate_apply (e, opname, line, col, file)
    val+ ~BE (ok, e) = __r
    in
      if ok then CKok (apply_apply (e, opname, line, col))
      else CKerr (e)
    end
  | SElim (loc, lhs, rhs, _) =>
    let
      val line = loc_line (loc)
      val col = loc_col (loc)
      val file = loc_file (loc)
      val srcname = op_name_of_expr (lhs)
      val opname = op_name_of_expr (rhs)
      val __r = env_is_bound (e, srcname)
    val+ ~BE (bound, e) = __r
    in
      if ~bound then
        let
          val () = error_emit (mk_unbound_operator (line, col, file, srcname))
        in CKerr (e) end
      else let
        val __r = env_is_bound (e, opname)
        val+ ~BE (b2, e) = __r
        val __r2 =
          (if b2 then env_eliminate (e, opname, line, col)
           else BE (true, e)) : bool_env
        val+ ~BE (ok, e) = __r2
      in
        if ok then CKok (e) else CKerr (e)
      end
    end
  | SCanon (loc, lhs, _) =>
    let
      val __r = env_canonize (e, op_name_of_expr (lhs), loc_line (loc), loc_col (loc))
    val+ ~BE (_, e) = __r
    in CKok (e) end
  | SCanon2 (loc, e0) =>
    let
      val __r = env_canonize (e, op_name_of_expr (e0), loc_line (loc), loc_col (loc))
    val+ ~BE (_, e) = __r
    in CKok (e) end
  | SDirectSum (_, _, _) => CKok (e)
  | SMarker (_, _, _) => CKok (e)
  | SGhostOp (loc, e0) =>
    CKok (env_ghost (e, op_name_of_expr (e0), loc_line (loc), loc_col (loc)))
  | SRef (_, _) => CKok (e)
  | SFrame (_, _) => CKok (env_push_frame (e))
  | SMat (loc, name, nums) =>
    let
      fun len (xs: List0(double)): int =
        case+ xs of list_nil () => 0 | list_cons (_, r) => g0int_add_int (1, len (r))
      val e = env_set_mat (e, name, len (nums), nums)
      val e = env_introduce (e, name, loc_line (loc), loc_col (loc), true)
    in CKok (e) end
  | SLock (loc, mtx, opname) =>
    CKok (env_lock (e, mtx, opname, loc_line (loc), loc_col (loc)))
  | SUnlock (loc, mtx, opname) =>
    let
      val __r = env_unlock (e, mtx, opname, loc_line (loc), loc_col (loc))
    val+ ~BE (ok, e) = __r
    in if ok then CKok (e) else CKerr (e) end
  | SCommute (loc, h1, h2) =>
    let
      val line = loc_line (loc)
      val col = loc_col (loc)
      val file = loc_file (loc)
      val __r = env_get_mat (e, h1)
    val+ ~MOE (m1, e) = __r
      val __r = env_get_mat (e, h2)
    val+ ~MOE (m2, e) = __r
      fun len (xs: List0(double)): int =
        case+ xs of list_nil () => 0 | list_cons (_, r) => g0int_add_int (1, len (r))
      fun nth (xs: List0(double), i: int): double =
        case+ xs of
        | list_nil () => 0.0
        | list_cons (x, r) => if i <= 0 then x else nth (r, g0int_sub_int (i, 1))
      fun isqrt (n: int, k: int): int =
        if g0int_mul_int (k, k) >= n then k else isqrt (n, g0int_add_int (k, 1))
      fun mat_mul (a: List0(double), b: List0(double), d: int): List0(double) =
        let
          fun cell (i: int, j: int, k: int, acc: double): double =
            if k >= d then acc
            else
              let
                val av = nth (a, g0int_add_int (g0int_mul_int (i, d), k))
                val bv = nth (b, g0int_add_int (g0int_mul_int (k, d), j))
                val prod = g0float_mul_double (av, bv)
              in cell (i, j, g0int_add_int (k, 1), g0float_add_double (acc, prod)) end
          fun row (i: int, j: int, acc: List0(double)): List0(double) =
            if j >= d then acc
            else row (i, g0int_add_int (j, 1), list_cons (cell (i, j, 0, 0.0), acc))
          fun rows (i: int, acc: List0(double)): List0(double) =
            if i >= d then acc
            else rows (g0int_add_int (i, 1), row (i, 0, acc))
          fun rev (xs: List0(double), acc: List0(double)): List0(double) =
            case+ xs of list_nil () => acc | list_cons (x, r) => rev (r, list_cons (x, acc))
        in rev (rows (0, list_nil ()), list_nil ()) end
      fun residual (ab: List0(double), ba: List0(double)): double =
        case+ (ab, ba) of
        | (list_nil (), _) => 0.0
        | (_, list_nil ()) => 0.0
        | (list_cons (x, xr), list_cons (y, yr)) =>
          let
            val d = g0float_sub_double (x, y)
            val ad = (if d < 0.0 then g0float_neg_double (d) else d): double
          in g0float_add_double (ad, residual (xr, yr)) end
      fun is_diag (xs: List0(double), d: int, i: int, j: int): bool =
        if i >= d then true
        else if j >= d then is_diag (xs, d, g0int_add_int (i, 1), 0)
        else
          let
            val v = nth (xs, g0int_add_int (g0int_mul_int (i, d), j))
            val ok = if i = j then true else (v = 0.0)
          in if ok then is_diag (xs, d, i, g0int_add_int (j, 1)) else false end
      fun is_id (xs: List0(double), d: int, i: int, j: int): bool =
        if i >= d then true
        else if j >= d then is_id (xs, d, g0int_add_int (i, 1), 0)
        else
          let
            val v = nth (xs, g0int_add_int (g0int_mul_int (i, d), j))
            val ok = if i = j then (v = 1.0) else (v = 0.0)
          in if ok then is_id (xs, d, i, g0int_add_int (j, 1)) else false end
    in
      case+ m1 of
      | None () =>
        let val () = error_emit (mk_unbound_operator (line, col, file, h1)) in CKerr (e) end
      | Some (@(n1, data1)) =>
        (
          case+ m2 of
          | None () =>
            let val () = error_emit (mk_unbound_operator (line, col, file, h2)) in CKerr (e) end
          | Some (@(n2, data2)) =>
            if n1 <> n2 then
              let
                val () = error_emit (mk_commutativity_failed (line, col, file, h1, h2))
              in CKerr (e) end
            else
              let
                val d = isqrt (n1, 0)
                val symbolic =
                  (is_diag (data1, d, 0, 0) && is_diag (data2, d, 0, 0))
                  || is_id (data1, d, 0, 0) || is_id (data2, d, 0, 0)
              in
                if symbolic then
                  let
                    val __r = env_canonize (e, h1, line, col)
                    val+ ~BE (_, e) = __r
                    val __r = env_canonize (e, h2, line, col)
                    val+ ~BE (_, e) = __r
                  in CKok (e) end
                else
                  let
                    val ab = mat_mul (data1, data2, d)
                    val ba = mat_mul (data2, data1, d)
                    val res = residual (ab, ba)
                  in
                    if res > 1.0e-12 then
                      let
                        val () = error_emit (mk_numerical_commutativity_check (line, col, file, h1, h2))
                      in CKerr (e) end
                    else
                      let
                        val __r = env_canonize (e, h1, line, col)
                        val+ ~BE (_, e) = __r
                        val __r = env_canonize (e, h2, line, col)
                        val+ ~BE (_, e) = __r
                      in CKok (e) end
                  end
              end
        )
    end
  | SInv (_, _) => CKok (e)
  | SExit (_, _) => CKok (e)
  | SMerge (_, _, _) => CKok (e)
  | SPivot (_, _, _) => CKok (e)
  | SLive (loc, names) =>
    let
      val line = loc_line (loc)
      val col = loc_col (loc)
      val file = loc_file (loc)
      fun mem (xs: List0(string), n: string): bool =
        case+ xs of
        | list_nil () => false
        | list_cons (x, r) => if x = n then true else mem (r, n)
      fun distinct (xs: List0(string), seen: List0(string)): bool =
        case+ xs of
        | list_nil () => true
        | list_cons (n, rest) =>
          if mem (seen, n) then false
          else distinct (rest, $UN.cast{List0(string)}(list_cons (n, seen)))
      fun intro_apply (e: env_vt, xs: List0(string)): env_vt =
        case+ xs of
        | list_nil () => e
        | list_cons (nm, rest) =>
          let
            val __r = env_is_bound (e, nm)
    val+ ~BE (bound, e) = __r
            val e =
              (if ~bound then env_introduce (e, nm, line, col, false) else e) : env_vt
            val __r = env_apply (e, nm, line, col)
    val+ ~BE (_, e) = __r
          in intro_apply (e, rest) end
      fun domains (xs: List0(string), acc: List0(string)): List0(string) =
        case+ xs of
        | list_nil () => acc
        | list_cons (n, rest) =>
          if n = "act" then domains (rest, acc)
          else domains (rest, $UN.cast{List0(string)}(list_cons (n, acc)))
      val doms = domains (names, list_nil ())
    in
      if distinct (doms, list_nil ()) then
        CKok (intro_apply (e, doms))
      else
        let
          val () = error_emit (mk_linearity_violation (line, col, file, "live",
            "INV VIOLATION: domains overlap"))
        in CKerr (e) end
    end
  | SDeferred (_, _, _) => CKok (e)
  | SOnpie (loc, args) =>
    let
      fun nth (xs: List0(string), i: int): Option(string) =
        case+ xs of
        | list_nil () => None ()
        | list_cons (x, rest) =>
          if i = 0 then Some (x) else nth (rest, g0int_sub_int (i, 1))
      val odname = nth (args, 3)
    in
      case+ odname of
      | None () => CKok (e)
      | Some (dname) =>
        let
          val __r = env_get_dim (e, dname)
          val+ ~DOE (od, e) = __r
        in
          case+ od of
          | None () => CKok (e)
          | Some (@(_, depth)) =>
            if depth <= 0 then
              let
                val () = error_emit (mk_dimension_underflow (loc_line (loc), loc_col (loc), loc_file (loc), dname))
              in CKerr (e) end
            else CKok (e)
        end
    end
  | SGath (_, _) => CKok (e)
  | SDisjoint (_, _) => CKok (e)
  | SDimDecl (_, dname, has_pole, depth) =>
    CKok (env_set_dim (e, dname, has_pole, depth))
  | SPole (loc, dname, _) =>
    let
      val __r = env_get_dim (e, dname)
    val+ ~DOE (od, e) = __r
      val depth =
        case+ od of
        | Some (@(_, d)) => d
        | None () => 0
    in
      CKok (env_set_dim (e, dname, true, depth))
    end
  | SBranch (loc, _, body) =>
    let
      val e = onpie_letrec_intro (e, body, loc_line (loc), loc_col (loc))
    in check_stmts (body, e) end
  | SDecompose (loc, _, _, _, dname, body) =>
    let
      val e = onpie_letrec_intro (e, body, loc_line (loc), loc_col (loc))
      val __r = env_get_dim (e, dname)
      val+ ~DOE (od, e) = __r
    in
      case+ od of
      | None () => check_stmts (body, e)
      | Some (@(_, _)) =>
        let
          val __r = env_step_dim (e, dname)
          val+ ~BE (ok, e) = __r
        in
          if ok then check_stmts (body, e)
          else let
            val () = error_emit (mk_dimension_underflow (loc_line (loc), loc_col (loc), loc_file (loc), dname))
          in CKerr (e) end
        end
    end
  | SLoop (_, _, _, _, _, body) => check_stmts (body, e)
  | SJoin (loc, _) =>
    let
      val __r = env_unreleased_locks (e)
    val+ ~LLE (locks, e) = __r
      val __r = env_unapplied_at_join (e)
    val+ ~SOE (unapp, e) = __r
      val ok1 =
        case+ unapp of
        | list_nil () => true
        | list_cons (opname, _) =>
          let
            val () = error_emit (mk_unapplied_operator (loc_line (loc), loc_col (loc), loc_file (loc), opname))
          in false end
      val ok2 =
        case+ locks of
        | list_nil () => true
        | list_cons (@(mtx, _), _) =>
          let
            val () = error_emit (mk_lock_not_released (loc_line (loc), loc_col (loc), loc_file (loc), mtx, ""))
          in false end
      val __r = env_get_dim (e, "d")
    val+ ~DOE (od, e) = __r
      val ok3 =
        case+ od of
        | Some (@(has_pole, _)) =>
          if has_pole then true
          else let
            val () = error_emit (mk_pole_unhandled (loc_line (loc), loc_col (loc), loc_file (loc), "d", "lambda"))
          in false end
        | None () => true
    in
      if ok1 andalso ok2 andalso ok3 then CKok (e) else CKerr (e)
    end
  | SGFun (loc, _, _, body) =>
    let
      val e = onpie_letrec_intro (e, body, loc_line (loc), loc_col (loc))
    in check_stmts (body, e) end
  | SCompressBlk (_, _, body) => check_stmts (body, e)

implement check_stmts (ss, e) =
  let
    fun loop (e: env_vt, xs: List0(stmt_t), inv_ok: bool): check_res =
      case+ xs of
      | list_nil () => CKok (e)
      | list_cons (SInv (loc, _), rest) =>
        (
          case+ check_stmt (SInv (loc, list_nil ()), e) of
          | ~CKok (e) => loop (e, rest, true)
          | ~CKerr (e) => CKerr (e)
        )
      | list_cons (SExit (loc, _), rest) =>
        if ~inv_ok then
          let
            val () = error_emit (mk_parse_error (loc_line (loc), loc_col (loc), loc_file (loc), "@exit without prior @inv"))
          in CKerr (e) end
        else
          (
            case+ check_stmt (SExit (loc, list_nil ()), e) of
            | ~CKok (e) => loop (e, rest, inv_ok)
            | ~CKerr (e) => CKerr (e)
          )
      | list_cons (s, rest) =>
        (
          case+ check_stmt (s, e) of
          | ~CKok (e) => loop (e, rest, inv_ok)
          | ~CKerr (e) => CKerr (e)
        )
  in
    loop (e, ss, false)
  end

implement check_func (Func (loc, name, params, body), e) =
  let
    fun intro_params (e: env_vt, ps: List0(param_t)): env_vt =
      case+ ps of
      | list_nil () => e
      | list_cons (PLinearOperator (l, n), rest) =>
        intro_params (env_introduce (e, n, loc_line (l), loc_col (l), false), rest)
      | list_cons (PSharedOperator (l, n), rest) =>
        intro_params (env_introduce (e, n, loc_line (l), loc_col (l), true), rest)
      | list_cons (PVector (l, n), rest) =>
        intro_params (env_ghost (e, n, loc_line (l), loc_col (l)), rest)
      | list_cons (PSize (l, n), rest) =>
        intro_params (env_introduce (e, n, loc_line (l), loc_col (l), true), rest)
      | list_cons (_, rest) => intro_params (e, rest)
    val e = intro_params (e, params)
    val e = onpie_letrec_intro (e, body, loc_line (loc), loc_col (loc))
  in
    check_stmts (body, e)
  end

implement validate_program (Program (funcs)) =
  let
    val e = env_create ()
    fun loop (e: env_vt, fs: List0(func_t)): check_res =
      case+ fs of
      | list_nil () => CKok (e)
      | list_cons (f, rest) =>
        (
          case+ check_func (f, e) of
          | ~CKok (e) => loop (e, rest)
          | ~CKerr (e) => CKerr (e)
        )
  in
    case+ loop (e, funcs) of
    | ~CKok (e) => let val () = env_destroy (e) in true end
    | ~CKerr (e) => let val () = env_destroy (e) in false end
  end

implement apply_program (Program (funcs), e) =
  let
    fun loop (e: env_vt, fs: List0(func_t)): env_vt =
      case+ fs of
      | list_nil () => e
      | list_cons (f, rest) =>
        (
          case+ check_func (f, e) of
          | ~CKok (e) => loop (e, rest)
          | ~CKerr (e) => loop (e, rest)
        )
  in
    loop (e, funcs)
  end

implement check_program (p) =
  if validate_program (p) then
    let
      val e = env_create ()
      val e = apply_program (p, e)
      val () = env_destroy (e)
    in true end
  else false

implement validate_stmt (s, e) = check_stmt (s, e)

implement apply_stmt (s, e) =
  case+ check_stmt (s, e) of
  | ~CKok (e) => e
  | ~CKerr (e) => e
