#define ATS_DYNLOADFLAG 0
#include "share/atspre_staload.hats"
staload UN = "prelude/SATS/unsafe.sats"
staload "src/parser/ast.sats"

datatype loc = Loc of (string, int, int)
assume loc_t = loc

implement loc_make (f, l, c) = Loc (f, l, c)
implement loc_file (Loc (f, _, _)) = f
implement loc_line (Loc (_, l, _)) = l
implement loc_col  (Loc (_, _, c)) = c
implement loc_to_string (Loc (f, l, c)) =
  let
    val s1 = $UN.castvwtp0{string}(string_append(f, ":"))
    val s2 = $UN.castvwtp0{string}(string_append(s1, tostring_int(l)))
    val s3 = $UN.castvwtp0{string}(string_append(s2, ":"))
  in $UN.castvwtp0{string}(string_append(s3, tostring_int(c))) end

implement expr_loc (e) =
  case+ e of
  | EIdent (l, _)     => l  | ESubscript (l, _, _) => l
  | ENat (l, _)       => l  | EReal (l, _)        => l
  | EOpRef (l, _, _)  => l  | EActRef (l, _)      => l
  | EPipe (l, _, _)   => l  | ENorm (l, _)        => l
  | EDiv (l, _, _)    => l  | EMul (l, _, _)      => l
  | EAdd (l, _, _)    => l  | ESub (l, _, _)      => l
  | EPow (l, _, _)    => l  | ELt (l, _, _)       => l
  | EGt (l, _, _)     => l  | EEq (l, _, _)       => l
  | EConj (l, _, _)   => l

implement stmt_loc (s) =
  case+ s of
  | SApply (l, _, _, _)      => l  | SElim (l, _, _, _)      => l
  | SCanon (l, _, _)         => l  | SCanon2 (l, _)          => l
  | SDirectSum (l, _, _)     => l  | SMarker (l, _, _)        => l
  | SGhostOp (l, _)          => l  | SRef (l, _)             => l
  | SFrame (l, _)            => l  | SMat (l, _, _)            => l  | SLock (l, _, _)          => l
  | SUnlock (l, _, _)        => l  | SCommute (l, _, _)       => l
  | SPole (l, _, _)          => l  | SInv (l, _)             => l
  | SExit (l, _)             => l  | SDisjoint (l, _)        => l
  | SDimDecl (l, _, _, _)    => l  | SBranch (l, _, _)        => l
  | SDecompose (l, _, _, _, _, _) => l | SLoop (l, _, _, _, _, _) => l
  | SMerge (l, _, _) => l | SPivot (l, _, _) => l | SLive (l, _) => l | SDeferred (l, _, _) => l
  | SOnpie (l, _) => l | SGath (l, _) => l
  | SJoin (l, _)             => l  | SGFun (l, _, _, _)        => l
  | SCompressBlk (l, _, _)   => l
