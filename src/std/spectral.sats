#define ATS_PACKNAME "crimsopie.std.spectral"

absviewtype H_vt (t: type)
absviewtype mtx_vt
absviewtype lock_v

prfun apply_H   {t:type} (H: H_vt(t) | x: t): (H_vt(t) | t)
prfun elim_H    {t:type} (H: H_vt(t) | x: t): void
prfun canon_H   {t:type} (H1: H_vt(t), H2: H_vt(t)): void
prfun lock_H    {t:type} (mtx: mtx_vt | H: H_vt(t)): (lock_v | H_vt(t))
prfun unlock_H  {t:type} (lk: lock_v | H: H_vt(t)): (mtx_vt | H_vt(t))
