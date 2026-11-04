#define ATS_DYNLOADFLAG 0
#include "share/atspre_staload.hats"
staload "src/std/spectral.sats"

assume H_vt (t) = ptr
assume mtx_vt = ptr
assume lock_v = ptr

primplement {t} apply_H (H | x) = (H | x)
primplement {t} elim_H  (H | x) = ()
primplement {t} canon_H (H1, H2) = ()
primplement {t} lock_H  (mtx | H) = (mtx | H)
primplement {t} unlock_H (lk | H) = (lk | H)
