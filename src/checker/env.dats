#define ATS_DYNLOADFLAG 0
#include "share/atspre_staload.hats"
staload UN = "prelude/SATS/unsafe.sats"
staload "src/parser/ast.sats"
staload "src/checker/env.sats"

fun find_op (ops: List0(op_entry), name: string): Option(op_entry) =
  case+ ops of
  | list_nil () => None ()
  | list_cons (OpEntry (n, s, l, c, a, sh), rest) =>
    if n = name then Some (OpEntry (n, s, l, c, a, sh)) else find_op (rest, name)

fun update_op (ops: List0(op_entry), name: string, status: op_status, apply_count: int): List0(op_entry) =
  case+ ops of
  | list_nil () => list_nil ()
  | list_cons (OpEntry (n, s, l, c, a, sh), rest) =>
    if n = name then
      $UN.cast{List0(op_entry)}(list_cons (OpEntry (n, status, l, c, apply_count, sh), rest))
    else
      $UN.cast{List0(op_entry)}(list_cons (OpEntry (n, s, l, c, a, sh), update_op (rest, name, status, apply_count)))

implement env_create () =
  Env (ref(list_nil()), ref(list_nil()), ref(list_nil()), ref(list_nil()))

implement env_destroy (e) =
  case+ e of | ~Env (_, _, _, _) => ()

implement env_introduce (e, name, line, col, is_shared) =
  case+ e of
  | ~Env (ops, locks, dims, mats) =>
    (
      case+ find_op (!ops, name) of
      | Some (_) => Env (ops, locks, dims, mats)
      | None () =>
        let
          val () = !ops :=
            $UN.cast{List0(op_entry)}(
              list_cons (OpEntry (name, OS_Introduced, line, col, 0, is_shared), !ops)
            )
        in
          Env (ops, locks, dims, mats)
        end
    )

implement env_apply (e, name, line, col) =
  case+ e of
  | ~Env (ops, locks, dims, mats) =>
    (
      case+ find_op (!ops, name) of
      | Some (OpEntry (_, _, _, _, a, _)) =>
        let
          val () = !ops := update_op (!ops, name, OS_Applied, g0int_add_int (a, 1))
        in
          BE (true, Env (ops, locks, dims, mats))
        end
      | None () => BE (false, Env (ops, locks, dims, mats))
    )

implement env_eliminate (e, name, line, col) =
  case+ e of
  | ~Env (ops, locks, dims, mats) =>
    (
      case+ find_op (!ops, name) of
      | Some (_) =>
        let
          val () = !ops := update_op (!ops, name, OS_Eliminated, 0)
        in
          BE (true, Env (ops, locks, dims, mats))
        end
      | None () => BE (false, Env (ops, locks, dims, mats))
    )

implement env_canonize (e, name, line, col) =
  case+ e of
  | ~Env (ops, locks, dims, mats) =>
    (
      case+ find_op (!ops, name) of
      | Some (_) =>
        let
          val () = !ops := update_op (!ops, name, OS_Canonized, 0)
        in
          BE (true, Env (ops, locks, dims, mats))
        end
      | None () => BE (false, Env (ops, locks, dims, mats))
    )

implement env_ghost (e, name, line, col) =
  case+ e of
  | ~Env (ops, locks, dims, mats) =>
    (
      case+ find_op (!ops, name) of
      | Some (_) => Env (ops, locks, dims, mats)
      | None () =>
        let
          val () = !ops :=
            $UN.cast{List0(op_entry)}(
              list_cons (OpEntry (name, OS_Ghost, line, col, 0, false), !ops)
            )
        in
          Env (ops, locks, dims, mats)
        end
    )

implement env_lock (e, mtx, opname, line, col) =
  case+ e of
  | ~Env (ops, locks, dims, mats) =>
    let
      val () = !locks :=
        $UN.cast{List0(lock_entry)}(list_cons (LockEntry (mtx, opname), !locks))
    in
      Env (ops, locks, dims, mats)
    end

implement env_unlock (e, mtx, opname, line, col) =
  case+ e of
  | ~Env (ops, locks, dims, mats) =>
    let
      fun rm (xs: List0(lock_entry)): List0(lock_entry) =
        case+ xs of
        | list_nil () => list_nil ()
        | list_cons (LockEntry (m, o), rest) =>
          if (m = mtx) andalso (o = opname) then rest
          else $UN.cast{List0(lock_entry)}(list_cons (LockEntry (m, o), rm (rest)))
      val () = !locks := rm (!locks)
    in
      BE (true, Env (ops, locks, dims, mats))
    end

implement env_is_bound (e, name) =
  case+ e of
  | ~Env (ops, locks, dims, mats) =>
    let
      val b =
        case+ find_op (!ops, name) of
        | Some (_) => true
        | None () => false
    in
      BE (b, Env (ops, locks, dims, mats))
    end

implement env_is_locked (e, name) =
  case+ e of
  | ~Env (ops, locks, dims, mats) =>
    let
      fun has (xs: List0(lock_entry)): bool =
        case+ xs of
        | list_nil () => false
        | list_cons (LockEntry (_, o), rest) => if o = name then true else has (rest)
      val b = has (!locks)
    in
      BE (b, Env (ops, locks, dims, mats))
    end

implement env_is_shared (e, name) =
  case+ e of
  | ~Env (ops, locks, dims, mats) =>
    let
      val b =
        case+ find_op (!ops, name) of
        | Some (OpEntry (_, _, _, _, _, sh)) => sh
        | None () => false
    in
      BE (b, Env (ops, locks, dims, mats))
    end

implement env_get_status (e, name) =
  case+ e of
  | ~Env (ops, locks, dims, mats) =>
    let
      val st =
        case+ find_op (!ops, name) of
        | Some (OpEntry (_, OS_Introduced (), _, _, _, _)) => 0
        | Some (OpEntry (_, OS_Applied (), _, _, _, _)) => 1
        | Some (OpEntry (_, OS_Eliminated (), _, _, _, _)) => 2
        | Some (OpEntry (_, OS_Canonized (), _, _, _, _)) => 3
        | Some (OpEntry (_, OS_Ghost (), _, _, _, _)) => 4
        | None () => ~1
    in
      IE (st, Env (ops, locks, dims, mats))
    end

implement env_push_frame (e) = e
implement env_pop_frame (e) = e

implement env_set_dim (e, dname, has_pole, depth) =
  case+ e of
  | ~Env (ops, locks, dims, mats) =>
    let
      fun rm (xs: List0(dim_entry)): List0(dim_entry) =
        case+ xs of
        | list_nil () => list_nil ()
        | list_cons (DimEntry (n, h, d), rest) =>
          if n = dname then rm (rest)
          else $UN.cast{List0(dim_entry)}(list_cons (DimEntry (n, h, d), rm (rest)))
      val () = !dims :=
        $UN.cast{List0(dim_entry)}(list_cons (DimEntry (dname, has_pole, depth), rm (!dims)))
    in
      Env (ops, locks, dims, mats)
    end

implement env_get_dim (e, dname) =
  case+ e of
  | ~Env (ops, locks, dims, mats) =>
    let
      fun find (xs: List0(dim_entry)): Option(@(bool, int)) =
        case+ xs of
        | list_nil () => None ()
        | list_cons (DimEntry (n, h, d), rest) =>
          if n = dname then Some (@(h, d)) else find (rest)
      val r = find (!dims)
    in
      DOE (r, Env (ops, locks, dims, mats))
    end

implement env_step_dim (e, dname) =
  case+ e of
  | ~Env (ops, locks, dims, mats) =>
    let
      fun upd (xs: List0(dim_entry)): @(bool, List0(dim_entry)) =
        case+ xs of
        | list_nil () => @(false, list_nil ())
        | list_cons (DimEntry (n, h, d), rest) =>
          if n = dname then
            if d <= 0 then
              @(false, $UN.cast{List0(dim_entry)}(list_cons (DimEntry (n, h, d), rest)))
            else
              @(true, $UN.cast{List0(dim_entry)}(list_cons (DimEntry (n, h, g0int_sub_int (d, 1)), rest)))
          else let
            val @(ok, rr) = upd (rest)
          in
            @(ok, $UN.cast{List0(dim_entry)}(list_cons (DimEntry (n, h, d), rr)))
          end
      val @(ok, xs) = upd (!dims)
      val () = !dims := xs
    in
      BE (ok, Env (ops, locks, dims, mats))
    end

implement env_unapplied_at_join (e) =
  case+ e of
  | ~Env (ops, locks, dims, mats) =>
    let
      fun loop (xs: List0(op_entry), acc: List0(string)): List0(string) =
        case+ xs of
        | list_nil () => acc
        | list_cons (entry, rest) =>
          case+ entry of
          | OpEntry (n, st, _, _, _, sh) =>
            (
              case+ st of
              | OS_Introduced () =>
                if sh then loop (rest, acc)
                else loop (rest, $UN.cast{List0(string)}(list_cons (n, acc)))
              | _ => loop (rest, acc)
            )
      val r = loop (!ops, list_nil ())
    in
      SOE (r, Env (ops, locks, dims, mats))
    end

implement env_unreleased_locks (e) =
  case+ e of
  | ~Env (ops, locks, dims, mats) =>
    let
      fun conv (xs: List0(lock_entry)): List0(@(string, string)) =
        case+ xs of
        | list_nil () => list_nil ()
        | list_cons (LockEntry (m, o), rest) =>
          $UN.cast{List0(@(string, string))}(list_cons (@(m, o), conv (rest)))
      val r = conv (!locks)
    in
      LLE (r, Env (ops, locks, dims, mats))
    end

implement env_applied_ops (e) =
  case+ e of
  | ~Env (ops, locks, dims, mats) =>
    let
      fun loop (xs: List0(op_entry), acc: List0(string)): List0(string) =
        case+ xs of
        | list_nil () => acc
        | list_cons (entry, rest) =>
          case+ entry of
          | OpEntry (n, st, _, _, _, _) =>
            (
              case+ st of
              | OS_Applied () =>
                loop (rest, $UN.cast{List0(string)}(list_cons (n, acc)))
              | _ => loop (rest, acc)
            )
      val r = loop (!ops, list_nil ())
    in
      SOE (r, Env (ops, locks, dims, mats))
    end

implement env_any_lock_held (e) =
  case+ e of
  | ~Env (ops, locks, dims, mats) =>
    let
      val b =
        case+ !locks of
        | list_nil () => false
        | list_cons (_, _) => true
    in
      BE (b, Env (ops, locks, dims, mats))
    end

implement env_set_mat (e, name, n, data) =
  case+ e of
  | ~Env (ops, locks, dims, mats) =>
    let
      fun rm (xs: List0(mat_entry)): List0(mat_entry) =
        case+ xs of
        | list_nil () => list_nil ()
        | list_cons (MatEntry (nm, nn, dd), rest) =>
          if nm = name then rm (rest)
          else $UN.cast{List0(mat_entry)}(list_cons (MatEntry (nm, nn, dd), rm (rest)))
      val () = !mats :=
        $UN.cast{List0(mat_entry)}(list_cons (MatEntry (name, n, data), rm (!mats)))
    in
      Env (ops, locks, dims, mats)
    end

implement env_get_mat (e, name) =
  case+ e of
  | ~Env (ops, locks, dims, mats) =>
    let
      fun find (xs: List0(mat_entry)): Option(@(int, List0(double))) =
        case+ xs of
        | list_nil () => None ()
        | list_cons (MatEntry (nm, n, d), rest) =>
          if nm = name then Some (@(n, d)) else find (rest)
      val r = find (!mats)
    in
      MOE (r, Env (ops, locks, dims, mats))
    end
