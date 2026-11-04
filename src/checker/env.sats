#define ATS_PACKNAME "crimsopie.checker.env"
staload "src/parser/ast.sats"

datatype op_status =
  | OS_Introduced | OS_Applied | OS_Eliminated | OS_Canonized | OS_Ghost

datatype op_entry = OpEntry of (string, op_status, int, int, int, bool)
datatype lock_entry = LockEntry of (string, string)
datatype dim_entry = DimEntry of (string, bool, int)
datatype mat_entry = MatEntry of (string, int, List0(double))

dataviewtype env_vt =
  | Env of (
      ref(List0(op_entry))
    , ref(List0(lock_entry))
    , ref(List0(dim_entry))
    , ref(List0(mat_entry))
    )

dataviewtype bool_env =
  | BE of (bool, env_vt)

dataviewtype int_env =
  | IE of (int, env_vt)

dataviewtype stropt_env =
  | SOE of (List0(string), env_vt)

dataviewtype locklist_env =
  | LLE of (List0(@(string, string)), env_vt)

dataviewtype dimopt_env =
  | DOE of (Option(@(bool, int)), env_vt)

dataviewtype matopt_env =
  | MOE of (Option(@(int, List0(double))), env_vt)

fun env_create  (): env_vt
fun env_destroy (e: env_vt): void

fun env_introduce  (e: env_vt, name: string, line: int, col: int, is_shared: bool): env_vt
fun env_apply      (e: env_vt, name: string, line: int, col: int): bool_env
fun env_eliminate  (e: env_vt, name: string, line: int, col: int): bool_env
fun env_canonize   (e: env_vt, name: string, line: int, col: int): bool_env
fun env_ghost      (e: env_vt, name: string, line: int, col: int): env_vt
fun env_lock       (e: env_vt, mtx: string, opname: string, line: int, col: int): env_vt
fun env_unlock     (e: env_vt, mtx: string, opname: string, line: int, col: int): bool_env
fun env_is_bound   (e: env_vt, name: string): bool_env
fun env_is_locked  (e: env_vt, name: string): bool_env
fun env_is_shared  (e: env_vt, name: string): bool_env
fun env_get_status (e: env_vt, name: string): int_env
fun env_push_frame (e: env_vt): env_vt
fun env_pop_frame  (e: env_vt): env_vt
fun env_set_dim    (e: env_vt, dname: string, has_pole: bool, depth: int): env_vt
fun env_get_dim    (e: env_vt, dname: string): dimopt_env
fun env_step_dim   (e: env_vt, dname: string): bool_env
fun env_unapplied_at_join (e: env_vt): stropt_env
fun env_unreleased_locks  (e: env_vt): locklist_env
fun env_applied_ops (e: env_vt): stropt_env
fun env_any_lock_held (e: env_vt): bool_env
fun env_set_mat (e: env_vt, name: string, n: int, data: List0(double)): env_vt
fun env_get_mat (e: env_vt, name: string): matopt_env
