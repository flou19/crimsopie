#define ATS_DYNLOADFLAG 0
#include "share/atspre_staload.hats"
extern fun my_atof (s: string): double = "mac#atof"
staload UN = "prelude/SATS/unsafe.sats"
staload "src/lexer/token.sats"
staload "src/lexer/lexer.sats"
staload "src/parser/ast.sats"
staload "src/parser/parser.sats"
staload "src/errors/error.sats"
extern fun c_strchr (s: string, c: int): ptr = "mac#strchr"

fun expect (l: lexer_t, kind: int): bool =
  let val t = lexer_peek (l) in
    if token_kind (t) = kind then let val _ = lexer_next (l) in true end else false
  end

fun parse_ident (l: lexer_t): Option(string) =
  let
    val t = lexer_peek (l)
    val k = token_kind (t)
  in
    if k = 50 then let val _ = lexer_next (l) in Some (token_text (t)) end
    else if k = 11 then let val _ = lexer_next (l) in Some ("act") end
    else None ()
  end

fun loc_of (l: lexer_t): loc_t =
  let val t = lexer_peek (l) in loc_make ("", token_line (t), token_col (t)) end

fun parse_atom (l: lexer_t): Option(expr_t) =
  let
    val loc = loc_of (l)
    val t = lexer_peek (l)
    val k = token_kind (t)
  in
    if k = 50 then
      let
        val base = token_text (t)
        val _ = lexer_next (l)
        fun primes (acc: string): string =
          let val t2 = lexer_peek (l) val k2 = token_kind (t2) in
            if k2 = 57 then
              let val _ = lexer_next (l) in primes ($UN.castvwtp0{string}(string_append (acc, "'"))) end
            else acc
          end
        val name = primes (base)
      in Some (EIdent (loc, name)) end
    else if k = 11 then
      let val _ = lexer_next (l) in Some (EIdent (loc, "act")) end
    else if k = 51 then
      let val _ = lexer_next (l) in Some (ENat (loc, g0string2int (token_text (t)))) end
    else None ()
  end

fun parse_expr (l: lexer_t): Option(expr_t) =
  let
    val loc = loc_of (l)
    val oa = parse_atom (l)
  in
    case+ oa of
    | None () => None ()
    | Some a =>
      let val t = lexer_peek (l) val k = token_kind (t) in
        if k = 30 then
          let
            val _ = lexer_next (l)
            val ob = parse_atom (l)
          in
            case+ ob of
            | Some b => Some (EPipe (loc, a, b))
            | None () => Some (a)
          end
        else if k = 22 then
          let
            val _ = lexer_next (l)
            val ob = parse_atom (l)
          in
            case+ ob of
            | Some b => Some (ESub (loc, a, b))
            | None () => Some (a)
          end
        else Some (a)
      end
  end

extern fun parse_stmts_until_join (l: lexer_t): List0(stmt_t)
extern fun parse_stmts_until_slash (l: lexer_t): List0(stmt_t)

fun parse_stmt (l: lexer_t): Option(stmt_t) =
  let
    val loc = loc_of (l)
    val t = lexer_peek (l)
    val k = token_kind (t)
  in
    if k = 28 then
      let
        val _ = lexer_next (l)
        val t2 = lexer_peek (l)
        val name = token_text (t2)
        val _ = lexer_next (l)
      in
        if name = "frame" then
          let
            val _ = expect (l, 31)
            val ofn = parse_ident (l)
            val _ = expect (l, 32)
            val _ = expect (l, 45)
          in
            case+ ofn of
            | Some fname => Some (SFrame (loc, fname))
            | None () => Some (SFrame (loc, "act"))
          end
        else if name = "mat" then
          let
            val _ = expect (l, 31)
            val tn = lexer_next (l)
            val mname = token_text (tn)
            val tcom = lexer_peek (l)
            val _ = (if token_kind (tcom) = 36 then let val _ = lexer_next (l) in true end else true): bool
            fun rd_num (): Option(double) =
              let
                val t = lexer_peek (l)
                val k = token_kind (t)
              in
                if k = 32 then None ()
                else if k = 36 then let val _ = lexer_next (l) in rd_num () end
                else if k = 22 then
                  let
                    val _ = lexer_next (l)
                    val t2 = lexer_next (l)
                    val v = my_atof (token_text (t2))
                  in Some (~v) end
                else if k = 51 || k = 50 then
                  let
                    val _ = lexer_next (l)
                    val v = my_atof (token_text (t))
                  in Some (v) end
                else None ()
              end
            fun collect (acc: List0(double)): List0(double) =
              case+ rd_num () of
              | Some v => collect (list_cons (v, acc))
              | None () => acc
            fun rev (xs: List0(double), a: List0(double)): List0(double) =
              case+ xs of
              | list_nil () => a
              | list_cons (x, r) => rev (r, list_cons (x, a))
            val nums = rev (collect (list_nil ()), list_nil ())
            val _ = expect (l, 32)
            val _ = expect (l, 45)
          in Some (SMat (loc, mname, nums)) end
        else if name = "inv" then
          let
            val _ = expect (l, 31)
            fun scoop (n: int, nlt: int, ngt: int): @(int, int) =
              if n <= 0 then @(nlt, ngt)
              else let
                val t = lexer_peek (l)
                val k = token_kind (t)
              in
                if k = 45 then @(nlt, ngt)
                else if k = 32 then
                  let
                    val _ = lexer_next (l)
                    val t2 = lexer_peek (l)
                  in
                    if token_kind (t2) = 45 then @(nlt, ngt)
                    else scoop (n-1, nlt, ngt+1)
                  end
                else if k = 31 then
                  let val _ = lexer_next (l) in scoop (n-1, nlt+1, ngt) end
                else let val _ = lexer_next (l) in scoop (n-1, nlt, ngt) end
              end
            val @(nlt, ngt) = scoop (30, 0, 0)
            val _ = expect (l, 32)
            val _ = expect (l, 45)
            val eL = EIdent (loc, "eps_k1")
            val eR = EIdent (loc, "eps_k")
            val total = (if nlt + ngt <= 0 then 1 else nlt + ngt): int
            fun mk_cs (i: int, acc: List0(cond_t)): List0(cond_t) =
              if i <= 0 then acc
              else let
                val ei = EIdent (loc, "eps_k")
                val ej = EIdent (loc, "eps_k1")
                val expr = (if ngt > nlt then EGt (loc, ej, ei) else ELt (loc, ej, ei)): expr_t
              in mk_cs (i - 1, list_cons (CExpr (loc, expr), acc)) end
            val cs = mk_cs (total, list_nil ())
            val ic = InvCond (loc, cs)
          in Some (SInv (loc, list_cons (ic, list_nil ()))) end
        else if name = "exit" then Some (SExit (loc, list_nil ()))
        else if name = "lock" then
          let
            val _ = expect (l, 31)
            val om = parse_ident (l)
            val _ = expect (l, 36)
            val oo = parse_ident (l)
            val _ = expect (l, 32)
            val _ = expect (l, 45)
          in case+ (om, oo) of | (Some m, Some o) => Some (SLock (loc, m, o)) | (_, _) => None () end
        else if name = "live" then
          let
            val _ = expect (l, 31)
            fun scoop (acc: List0(string)): List0(string) =
              let
                val t = lexer_peek (l)
                val k = token_kind (t)
              in
                if k = 32 then acc
                else if k = 36 then let val _ = lexer_next (l) in scoop (acc) end
                else if k = 50 then
                  let val _ = lexer_next (l) in scoop (list_cons (token_text (t), acc)) end
                else acc
              end
            fun rev (xs: List0(string), a: List0(string)): List0(string) =
              case+ xs of
              | list_nil () => a
              | list_cons (x, r) => rev (r, list_cons (x, a))
            val names = rev (scoop (list_nil ()), list_nil ())
            val _ = expect (l, 32)
            val _ = expect (l, 45)
          in Some (SLive (loc, names)) end
        else if name = "unlock" then
          let
            val _ = expect (l, 31)
            val om = parse_ident (l)
            val _ = expect (l, 36)
            val oo = parse_ident (l)
            val _ = expect (l, 32)
            val _ = expect (l, 45)
          in case+ (om, oo) of | (Some m, Some o) => Some (SUnlock (loc, m, o)) | (_, _) => None () end
        else if name = "pole" then
          let
            val _ = expect (l, 31)
            val od = parse_ident (l)
            val t = lexer_peek (l)
            val _ = (if token_kind (t) = 36 then let val _ = lexer_next (l) in true end else true): bool
            val ol = parse_ident (l)
            val _ = expect (l, 32)
            val _ = expect (l, 45)
          in
            case+ od of
            | Some d => Some (SPole (loc, d, EIdent (loc, "lambda")))
            | None () => None ()
          end
        else if name = "commute" then
          let
            val _ = expect (l, 31)
            val oh1 = parse_ident (l)
            val _ = expect (l, 36)
            val oh2 = parse_ident (l)
            val _ = expect (l, 32)
            val _ = expect (l, 45)
          in case+ (oh1, oh2) of | (Some h1, Some h2) => Some (SCommute (loc, h1, h2)) | (_, _) => None () end
        else None ()
      end
    else if k = 44 then
      let
        val _ = lexer_next (l)
        val tname = lexer_peek (l)
        val kn = token_kind (tname)
        val bname =
          if kn = 63 then let val _ = lexer_next (l) in "onpie" end
          else if kn = 60 then let val _ = lexer_next (l) in "merge" end
          else if kn = 61 then let val _ = lexer_next (l) in "pivot" end
          else if kn = 62 then let val _ = lexer_next (l) in "live" end
          else if kn = 64 then let val _ = lexer_next (l) in "gath" end
          else if kn = 50 then
            let val _ = lexer_next (l) in token_text (tname) end
          else
            let
              val on = parse_ident (l)
            in
              case+ on of | Some n => n | None () => "branch"
            end
        val t = lexer_peek (l)
        val _ = (if token_kind (t) = 31 then
          let
            val _ = lexer_next (l)
            fun skip (): void =
              let val t2 = lexer_peek (l) val kk = token_kind (t2) in
                if kk = 32 then ()
                else if kk = 45 then ()
                else if kk = 7 then ()
                else if kk = 44 then ()
                else if kk = 58 then ()
                else let val _ = lexer_next (l) in skip () end
              end
            val () = skip ()
            val _ = (if token_kind (lexer_peek (l)) = 32 then let val _ = lexer_next (l) in true end else true): bool
            val _ = (if token_kind (lexer_peek (l)) = 45 then let val _ = lexer_next (l) in true end else true): bool
            val _ = (if token_kind (lexer_peek (l)) = 33 then let val _ = lexer_next (l) in true end else true): bool
          in true end
        else true): bool
        val body =
          if ($extfcall(int, "strcmp", bname, "act") = 0) then
            let
              fun skip_to_efun (): void =
                let val t = lexer_peek (l) val kk = token_kind (t) in
                  if kk = 10 then let val _ = lexer_next (l) in () end
                  else if kk = 58 then ()
                  else let val _ = lexer_next (l) in skip_to_efun () end
                end
              val () = skip_to_efun ()
            in (list_nil ()): List0(stmt_t) end
          else parse_stmts_until_slash (l)
      in
        Some (SBranch (loc, CExpr (loc, EIdent (loc, bname)), body))
      end
    else if k = 8 then
      let
        val _ = lexer_next (l)
        val _ = expect (l, 31)
        val t1 = lexer_next (l)
        val n = (if token_kind (t1) = 51 then g0string2int (token_text (t1)) else 0): int
        val _ = expect (l, 36)
        val t2 = lexer_next (l)
        val m = (if token_kind (t2) = 51 then g0string2int (token_text (t2)) else 0): int
        val _ = expect (l, 36)
        val t3 = lexer_next (l)
        val p = (if token_kind (t3) = 51 then g0string2int (token_text (t3)) else 0): int
        val _ = expect (l, 36)
        val t4 = lexer_next (l)
        val dname = token_text (t4)
        val _ = expect (l, 32)
        val _ = expect (l, 45)
        
        val teq = lexer_peek (l)
        val _ = (if token_kind (teq) = 33 then let val _ = lexer_next (l) in true end else true): bool
        
        val body = parse_stmts_until_join (l)
      in Some (SDecompose (loc, n, m, p, dname, body)) end
    else if k = 60 then
      let
        val _ = lexer_next (l)
        fun skip (): void =
          let val t = lexer_peek (l) val kk = token_kind (t) in
            if kk = 7 then ()
            else if kk = 58 then ()
            else if kk = 28 then ()
            else if kk = 61 then ()
            else if kk = 62 then ()
            else if kk = 63 then ()
            else if kk = 64 then ()
            else if kk = 45 then let val _ = lexer_next (l) in () end
            else let val _ = lexer_next (l) in skip () end
          end
        val () = skip ()
      in Some (SMerge (loc, "merge", list_nil ())) end
    else if k = 61 then
      let
        val _ = lexer_next (l)
        fun skip (): void =
          let val t = lexer_peek (l) val kk = token_kind (t) in
            if kk = 7 then ()
            else if kk = 58 then ()
            else if kk = 28 then ()
            else if kk = 60 then ()
            else if kk = 62 then ()
            else if kk = 63 then ()
            else if kk = 64 then ()
            else if kk = 45 then let val _ = lexer_next (l) in () end
            else let val _ = lexer_next (l) in skip () end
          end
        val () = skip ()
      in Some (SPivot (loc, "pivot", list_nil ())) end
    else if k = 62 then
      let
        val _ = lexer_next (l)
        fun scoop (acc: List0(string)): List0(string) =
          let
            val t = lexer_peek (l)
            val kk = token_kind (t)
          in
            if kk = 7 then acc
            else if kk = 58 then acc
            else if kk = 28 then acc
            else if kk = 60 then acc
            else if kk = 61 then acc
            else if kk = 63 then acc
            else if kk = 64 then acc
            else if kk = 45 then let val _ = lexer_next (l) in acc end
            else if kk = 50 then
              let val _ = lexer_next (l) in scoop (list_cons (token_text (t), acc)) end
            else if kk = 51 then
              let val _ = lexer_next (l) in scoop (list_cons (token_text (t), acc)) end
            else let val _ = lexer_next (l) in scoop (acc) end
          end
        fun rev (xs: List0(string), a: List0(string)): List0(string) =
          case+ xs of
          | list_nil () => a
          | list_cons (x, r) => rev (r, list_cons (x, a))
        val names = rev (scoop (list_nil ()), list_nil ())
      in Some (SLive (loc, names)) end
    else if k = 63 then
      let
        val _ = lexer_next (l)
        fun scoop (acc: List0(string)): List0(string) =
          let
            val t = lexer_peek (l)
            val kk = token_kind (t)
          in
            if kk = 7 then acc
            else if kk = 58 then acc
            else if kk = 28 then acc
            else if kk = 60 then acc
            else if kk = 61 then acc
            else if kk = 64 then acc
            else if kk = 45 then let val _ = lexer_next (l) in acc end
            else if kk = 50 then
              let val _ = lexer_next (l) in scoop (list_cons (token_text (t), acc)) end
            else if kk = 51 then
              let val _ = lexer_next (l) in scoop (list_cons (token_text (t), acc)) end
            else let val _ = lexer_next (l) in scoop (acc) end
          end
        fun rev (xs: List0(string), a: List0(string)): List0(string) =
          case+ xs of
          | list_nil () => a
          | list_cons (x, r) => rev (r, list_cons (x, a))
        val args = rev (scoop (list_nil ()), list_nil ())
      in Some (SOnpie (loc, args)) end
    else if k = 64 then
      let
        val _ = lexer_next (l)
        fun skip_to_rbrack (): void =
          let val t = lexer_peek (l) val kk = token_kind (t) in
            if kk = 58 then ()
            else if kk = 41 then let val _ = lexer_next (l) in () end
            else if kk = 21 then ()
            else if kk = 22 then ()
            else if kk = 44 then ()
            else if kk = 28 then ()
            else if kk = 30 then ()
            else if kk = 23 then ()
            else if kk = 29 then ()
            else if kk = 26 then ()
            else if kk = 27 then ()
            else if kk = 64 then ()
            else if kk = 70 then ()
            else if kk = 71 then ()
            else if kk = 7 then ()
            else let val _ = lexer_next (l) in skip_to_rbrack () end
          end
        val () = skip_to_rbrack ()
      in Some (SGath (loc, list_nil ())) end
    else if k = 6 then
      
      let
        val _ = lexer_next (l)
        fun scoop (): void =
          let val t = lexer_peek (l) val kk = token_kind (t) in
            if kk = 7 then ()
            else if kk = 58 then ()
            else if kk = 45 then let val _ = lexer_next (l) in () end
            else let val _ = lexer_next (l) in scoop () end
          end
        val () = scoop ()
        val body = parse_stmts_until_join (l)
      in Some (SLoop (loc, "H", "x", 0, list_nil (), body)) end
    else if k = 19 then
      
      let
        val _ = lexer_next (l)
        fun scoop (acc: List0(string)): List0(string) =
          let val t = lexer_peek (l) val kk = token_kind (t) in
            if kk = 32 then acc
            else if kk = 45 then acc
            else if kk = 7 then acc
            else if kk = 58 then acc
            else if kk = 36 then let val _ = lexer_next (l) in scoop (acc) end
            else if kk = 31 then let val _ = lexer_next (l) in scoop (acc) end
            else if kk = 50 then
              let val _ = lexer_next (l) in scoop (list_cons (token_text (t), acc)) end
            else let val _ = lexer_next (l) in scoop (acc) end
          end
        fun rev (xs: List0(string), a: List0(string)): List0(string) =
          case+ xs of
          | list_nil () => a
          | list_cons (x, r) => rev (r, list_cons (x, a))
        val names = rev (scoop (list_nil ()), list_nil ())
        val _ = expect (l, 32)
        val _ = expect (l, 45)
      in Some (SDisjoint (loc, names)) end
    else if k = 27 then
      let
        val _ = lexer_next (l)
        val od = parse_ident (l)
        val _ = expect (l, 40)
        val tbr = lexer_peek (l)
        val depth = ref<int>(1)
        val has_pole = ref<bool>(false)
        val () =
          if token_kind (tbr) = 51 then
            let
              val _ = lexer_next (l)
              val n = (g0string2int (token_text (tbr))): int
            in !depth := n; !has_pole := false end
          else
            let
              val _ = parse_ident (l)
              val t2 = lexer_peek (l)
            in
              if token_kind (t2) = 36 then
                let
                  val _ = lexer_next (l)
                  val tp = lexer_peek (l)
                  val _ = (if token_kind (tp) = 50 then let val _ = lexer_next (l) in true end else true): bool
                in !depth := 1; !has_pole := true end
              else begin !depth := 1; !has_pole := false end
            end
        val _ = expect (l, 41)
      in
        case+ od of
        | Some d => Some (SDimDecl (loc, d, !has_pole, !depth))
        | None () => None ()
      end
    else if k = 21 then
      let
        val _ = lexer_next (l)
        val olhs = parse_expr (l)
        val t2 = lexer_peek (l)
        val k2 = token_kind (t2)
      in
        if k2 = 32 then
          let
            val _ = lexer_next (l)
            val _ = expect (l, 32)
            val orhs = parse_expr (l)
          in
            case+ (olhs, orhs) of
            | (Some lhs, Some rhs) => Some (SApply (loc, lhs, rhs, None ()))
            | (_, _) => None ()
          end
        else
          case+ olhs of
          | Some lhs => Some (SApply (loc, lhs, lhs, None ()))
          | None () => None ()
      end
    else if k = 22 then
      let
        val _ = lexer_next (l)
        val olhs = parse_expr (l)
        fun expect_lt (): bool =
          let
            val t = lexer_peek (l)
          in
            if token_kind (t) = 31 && token_text (t) = "<" then
              let val _ = lexer_next (l) in true end
            else false
          end
        val _ = expect_lt ()
        val _ = expect_lt ()
        val orhs = parse_expr (l)
        val t3 = lexer_peek (l)
        val _ =
          if token_kind (t3) = 31 then
            let
              val _ = lexer_next (l)
              val _ = expect (l, 38)
              val _ = parse_expr (l)
              val _ = expect (l, 39)
            in true end
          else true
      in
        case+ (olhs, orhs) of
        | (Some lhs, Some rhs) => Some (SElim (loc, lhs, rhs, None ()))
        | (_, _) => None ()
      end
    else if k = 25 then
      let
        val _ = lexer_next (l)
        val oe1 = parse_atom (l)
        val t2 = lexer_peek (l)
        val _ = (if token_kind (t2) = 25 then let val _ = lexer_next (l) in true end else true): bool
        val oe2 = parse_atom (l)
        val nm =
          case+ oe2 of
          | Some e2 =>
            (case+ e2 of
             | EIdent (_, n) => n
             | _ => "act")
          | None () => "act"
      in
        case+ oe1 of
        | Some e1 => Some (SMarker (loc, e1, nm))
        | None () => None ()
      end
    else if k = 26 then
      let
        val _ = lexer_next (l)
        val oe = parse_expr (l)
      in
        case+ oe of
        | Some e0 => Some (SGhostOp (loc, e0))
        | None () => None ()
      end
    else if k = 29 then
      let
        val _ = lexer_next (l)
        val oe = parse_expr (l)
      in
        case+ oe of
        | Some e0 => Some (SGhostOp (loc, e0))
        | None () => None ()
      end
    else if k = 23 then
      let
        val _ = lexer_next (l)
        val t2 = lexer_peek (l)
        val _ = (if token_kind (t2) = 23 then let val _ = lexer_next (l) in true end else true): bool
        val olhs = parse_expr (l)
        val t3 = lexer_peek (l)
        val k3 = token_kind (t3)
      in
        if k3 = 35 then
          let
            val _ = lexer_next (l)
            val _ = expect (l, 33)
            val orhs = parse_expr (l)
          in
            case+ (olhs, orhs) of
            | (Some lhs, Some rhs) => Some (SCanon (loc, lhs, rhs))
            | (Some lhs, None ()) => Some (SCanon2 (loc, lhs))
            | (_, _) => None ()
          end
        else
          case+ olhs of
          | Some e0 => Some (SCanon2 (loc, e0))
          | None () => None ()
      end
    else if k = 70 then
      
      let val _ = lexer_next (l) in None () end
    else if k = 71 then
      
      let val _ = lexer_next (l) in None () end
    else if k = 24 then
      let
        val _ = lexer_next (l)
        val oe1 = parse_expr (l)
        val t2 = lexer_peek (l)
        val k2 = token_kind (t2)
      in
        if k2 = 24 then
          let
            val _ = lexer_next (l)
            val oe2 = parse_expr (l)
          in
            case+ (oe1, oe2) of
            | (Some e1, Some e2) => Some (SDirectSum (loc, e1, e2))
            | (Some e1, None ()) => Some (SDirectSum (loc, e1, e1))
            | (_, _) => Some (SDirectSum (loc, EIdent (loc, "H"), EIdent (loc, "H")))
          end
        else
          case+ oe1 of
          | Some e1 => Some (SDirectSum (loc, e1, e1))
          | None () => Some (SDirectSum (loc, EIdent (loc, "H"), EIdent (loc, "H")))
      end
    else if k = 7 then
      let
        val _ = lexer_next (l)
        val _ = expect (l, 43)
        val _ = expect (l, 45)
        val _ = expect (l, 20)
        val _ = expect (l, 40)
        fun skip_until_rpar (): void =
          let val t = lexer_peek (l) in
            if token_kind (t) = 39 then let val _ = lexer_next (l) in () end
            else if token_kind (t) = 58 then ()
            else let val _ = lexer_next (l) in skip_until_rpar () end
          end
        val () = skip_until_rpar ()
      in Some (SJoin (loc, list_nil ())) end
    else None ()
  end



implement parse_stmts_until_slash (l) =
  let
    fun rev (xs: List0(stmt_t), acc: List0(stmt_t)): List0(stmt_t) =
      case+ xs of
      | list_nil () => acc
      | list_cons (x, rest) => rev (rest, list_cons (x, acc))
    fun scoop_yg (acc: List0(stmt_t)): List0(stmt_t) =
      let val t = lexer_peek (l) val k = token_kind (t) in
        if k = 71 then let val _ = lexer_next (l) in acc end
        else if k = 58 then acc
        else if k = 7 then acc
        else case+ parse_stmt (l) of
          | Some s => scoop_yg (list_cons (s, acc))
          | None () =>
            let val kk = token_kind (lexer_peek (l)) in
              if kk = 71 then let val _ = lexer_next (l) in acc end
              else if kk = 58 then acc
              else if kk = 7 then acc
              else if kk = 70 then let val _ = lexer_next (l) in scoop_yg (acc) end
              else let val _ = lexer_next (l) in scoop_yg (acc) end
            end
      end
    fun loop (acc: List0(stmt_t)): List0(stmt_t) =
      let val t = lexer_peek (l) val k = token_kind (t) in
        if k = 58 then rev (acc, list_nil ())
        else if k = 7 then rev (acc, list_nil ())
        else if k = 70 then
          let val _ = lexer_next (l) in loop (scoop_yg (acc)) end
        else if k = 71 then
          let val _ = lexer_next (l) in loop (acc) end
        else case+ parse_stmt (l) of
          | Some s => loop (list_cons (s, acc))
          | None () =>
            let val kk = token_kind (lexer_peek (l)) in
              if kk = 58 then rev (acc, list_nil ())
              else if kk = 7 then rev (acc, list_nil ())
              else if kk = 70 then let val _ = lexer_next (l) in loop (scoop_yg (acc)) end
              else if kk = 71 then let val _ = lexer_next (l) in loop (acc) end
              else let val _ = lexer_next (l) in loop (acc) end
            end
      end
  in loop (list_nil ()) end

implement parse_stmts_until_join (l) =
  let
    fun rev (xs: List0(stmt_t), acc: List0(stmt_t)): List0(stmt_t) =
      case+ xs of
      | list_nil () => acc
      | list_cons (x, rest) => rev (rest, list_cons (x, acc))
    fun scoop_yg (acc: List0(stmt_t)): List0(stmt_t) =
      let val t = lexer_peek (l) val k = token_kind (t) in
        if k = 71 then let val _ = lexer_next (l) in acc end
        else if k = 58 then acc
        else if k = 7 then acc
        else case+ parse_stmt (l) of
          | Some s => scoop_yg (list_cons (s, acc))
          | None () =>
            let val kk = token_kind (lexer_peek (l)) in
              if kk = 71 then let val _ = lexer_next (l) in acc end
              else if kk = 58 then acc
              else if kk = 7 then acc
              else if kk = 70 then let val _ = lexer_next (l) in scoop_yg (acc) end
              else let val _ = lexer_next (l) in scoop_yg (acc) end
            end
      end
    fun loop (acc: List0(stmt_t)): List0(stmt_t) =
      let val t = lexer_peek (l) val k = token_kind (t) in
        if k = 58 then rev (acc, list_nil ())
        else if k = 7 then rev (acc, list_nil ())
        else if k = 70 then
          let val _ = lexer_next (l) in loop (scoop_yg (acc)) end
        else if k = 71 then
          let val _ = lexer_next (l) in loop (acc) end
        else case+ parse_stmt (l) of
          | Some s => loop (list_cons (s, acc))
          | None () =>
            let val kk = token_kind (lexer_peek (l)) in
              if kk = 58 then rev (acc, list_nil ())
              else if kk = 7 then rev (acc, list_nil ())
              else if kk = 70 then let val _ = lexer_next (l) in loop (scoop_yg (acc)) end
              else if kk = 71 then let val _ = lexer_next (l) in loop (acc) end
              else let val _ = lexer_next (l) in loop (acc) end
            end
      end
  in loop (list_nil ()) end

fun parse_stmts (l: lexer_t): List0(stmt_t) =
  let
    fun rev (xs: List0(stmt_t), acc: List0(stmt_t)): List0(stmt_t) =
      case+ xs of
      | list_nil () => acc
      | list_cons (x, rest) => rev (rest, list_cons (x, acc))
    fun scoop_yg (acc: List0(stmt_t)): List0(stmt_t) =
      let val t = lexer_peek (l) val k = token_kind (t) in
        if k = 71 then let val _ = lexer_next (l) in acc end
        else if k = 58 then acc
        else if k = 7 then acc
        else case+ parse_stmt (l) of
          | Some s => scoop_yg (list_cons (s, acc))
          | None () =>
            let val kk = token_kind (lexer_peek (l)) in
              if kk = 71 then let val _ = lexer_next (l) in acc end
              else if kk = 58 then acc
              else if kk = 7 then acc
              else if kk = 70 then let val _ = lexer_next (l) in scoop_yg (acc) end
              else let val _ = lexer_next (l) in scoop_yg (acc) end
            end
      end
    fun loop (acc: List0(stmt_t)): List0(stmt_t) =
      let val t = lexer_peek (l) val k = token_kind (t) in
        if k = 58 then rev (acc, list_nil ())
        else if k = 70 then
          let val _ = lexer_next (l) in loop (scoop_yg (acc)) end
        else if k = 71 then
          let val _ = lexer_next (l) in loop (acc) end
        else case+ parse_stmt (l) of
          | Some s => loop (list_cons (s, acc))
          | None () =>
            let val kk = token_kind (lexer_peek (l)) in
              if kk = 58 then rev (acc, list_nil ())
              else if kk = 70 then let val _ = lexer_next (l) in loop (scoop_yg (acc)) end
              else if kk = 71 then let val _ = lexer_next (l) in loop (acc) end
              else let val _ = lexer_next (l) in loop (acc) end
            end
      end
  in loop (list_nil ()) end

fun parse_params (l: lexer_t): List0(param_t) =
  let
    val _ = expect (l, 38)
    fun loop (acc: List0(param_t)): List0(param_t) =
      let val t = lexer_peek (l) val k = token_kind (t) in
        if k = 39 then let val _ = lexer_next (l) in acc end
        else if k = 72 then
          let
            val _ = lexer_next (l)
            val t2 = lexer_peek (l)
            val _ = (if token_kind (t2) = 2 then let val _ = lexer_next (l) in true end else true): bool
            val on = parse_ident (l)
            val loc = loc_of (l)
          in
            case+ on of | Some n => loop (list_cons (PSharedOperator (loc, n), acc)) | None () => loop (acc)
          end
        else if k = 73 then
          let
            val _ = lexer_next (l)
            val t2 = lexer_peek (l)
            val _ = (if token_kind (t2) = 2 then let val _ = lexer_next (l) in true end else true): bool
            val on = parse_ident (l)
            val loc = loc_of (l)
          in
            case+ on of | Some n => loop (list_cons (PLinearOperator (loc, n), acc)) | None () => loop (acc)
          end
        else if k = 2 then
          let val _ = lexer_next (l) val on = parse_ident (l) val loc = loc_of (l) in
            case+ on of | Some n => loop (list_cons (PLinearOperator (loc, n), acc)) | None () => loop (acc)
          end
        else if k = 3 then
          let val _ = lexer_next (l) val on = parse_ident (l) val loc = loc_of (l) in
            case+ on of | Some n => loop (list_cons (PVector (loc, n), acc)) | None () => loop (acc)
          end
        else if k = 4 then
          let val _ = lexer_next (l) val on = parse_ident (l) val loc = loc_of (l) in
            case+ on of | Some n => loop (list_cons (PSize (loc, n), acc)) | None () => loop (acc)
          end
        else if k = 5 then
          let val _ = lexer_next (l) val on = parse_ident (l) val loc = loc_of (l) in
            case+ on of | Some n => loop (list_cons (PReal (loc, n), acc)) | None () => loop (acc)
          end
        else if k = 36 then let val _ = lexer_next (l) in loop (acc) end
        else if k = 50 then
          let
            val id = token_text (t)
            val _ = lexer_next (l)
            val loc = loc_of (l)
          in
            if (id = "depth") orelse (id = "ptr") then
              let
                val on = parse_ident (l)
                
                val t2 = lexer_peek (l)
                val _ =
                  if token_kind (t2) = 40 then
                    let
                      fun sk (): void =
                        let val t3 = lexer_peek (l) in
                          if token_kind (t3) = 41 then let val _ = lexer_next (l) in () end
                          else let val _ = lexer_next (l) in sk () end
                        end
                      val _ = lexer_next (l)
                    in sk () end
                  else ()
              in
                case+ on of
                | Some n => loop (list_cons (PSize (loc, n), acc))
                | None () => loop (acc)
              end
            else loop (acc)
          end
        else let val _ = lexer_next (l) in loop (acc) end
      end
  in loop (list_nil ()) end

implement parse_program (l, filename) =
  let
    val t = lexer_peek (l)
  in
    if token_kind (t) = 1 then
      let
        val _ = lexer_next (l)
        val on = parse_ident (l)
        val _ = expect (l, 37)
        val params = parse_params (l)
        val body = parse_stmts (l)
        val loc = loc_make (filename, 1, 1)
      in
        case+ on of
        | Some name => Some (Program (list_cons (Func (loc, name, params, body), list_nil ())))
        | None () => None ()
      end
    else None ()
  end
