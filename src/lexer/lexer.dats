#define ATS_DYNLOADFLAG 0
#include "share/atspre_staload.hats"
staload "src/lexer/token.sats"
staload "src/lexer/lexer.sats"
staload UN = "prelude/SATS/unsafe.sats"

datatype lexer = Lexer of (List0(token_t), ref(int))
assume lexer_t = lexer

fun make_token (kind: int, line: int, col: int, text: string): token_t =
  token_create (kind, line, col, text)

fun is_alpha (c: int): bool =
  (c >= 97 && c <= 122) || (c >= 65 && c <= 90) || c = 95

fun is_digit (c: int): bool =
  c >= 48 && c <= 57

fun is_alnum (c: int): bool =
  is_alpha(c) || is_digit(c)

fun is_space (c: int): bool =
  c = 32 || c = 9 || c = 13

fun kw_kind (s: string): int =
  case+ s of
  | "indicate" => 1
  | "operator" => 2
  | "vector" => 3
  | "size_t" => 4
  | "real" => 5
  | "loop" => 6
  | "join" => 7
  | "decompose" => 8
  | "gFun" => 9
  | "eFun" => 10
  | "act" => 11
  | "frame" => 12
  | "lock" => 13
  | "unlock" => 14
  | "commute" => 15
  | "pole" => 16
  | "inv" => 17
  | "exit" => 18
  | "disjoint" => 19
  | "prt" => 20
  | "shared" => 72
  | "linear" => 73
  | _ => 0

fun char_kind (c: int): int =
  if c = 43 then 21 else if c = 45 then 22 else if c = 126 then 23
  else if c = 42 then 24 else if c = 37 then 25 else if c = 38 then 26
  else if c = 94 then 27 else if c = 64 then 28 else if c = 35 then 29
  else if c = 124 then 30 else if c = 60 then 31 else if c = 62 then 32
  else if c = 61 then 33 else if c = 63 then 34 else if c = 58 then 35
  else if c = 44 then 36 else if c = 59 then 37 else if c = 40 then 38
  else if c = 41 then 39 else if c = 91 then 40 else if c = 93 then 41
  else if c = 123 then 42 else if c = 125 then 43 else if c = 47 then 44
  else if c = 92 then 45 else if c = 46 then 46 else 57

fun ch_at (src: string, pos: int): int =
  if pos < 0 then 0
  else if pos >= g0u2i(string_length(src)) then 0
  else
    let
      val p = string2ptr(src)
      val c = $UN.ptr0_get_at<char>(p, pos)
    in char2int0(c) end

fun char_to_str (c: int): string =
  if c = 43 then "+" else if c = 45 then "-" else if c = 126 then "~"
  else if c = 42 then "*" else if c = 37 then "%" else if c = 38 then "&"
  else if c = 94 then "^" else if c = 64 then "@" else if c = 35 then "#"
  else if c = 124 then "|" else if c = 60 then "<" else if c = 62 then ">"
  else if c = 61 then "=" else if c = 63 then "?" else if c = 58 then ":"
  else if c = 44 then "," else if c = 59 then ";" else if c = 40 then "("
  else if c = 41 then ")" else if c = 91 then "[" else if c = 93 then "]"
  else if c = 123 then "{" else if c = 125 then "}" else if c = 47 then "/"
  else if c = 92 then "\\" else if c = 46 then "." else "?"

fun skip_spaces (src: string, pos: int, line: int, col: int): @(int, int, int) =
  let
    val n = g0u2i(string_length(src))
    fun loop (p: int, ln: int, cl: int): @(int, int, int) =
      if p >= n then @(p, ln, cl)
      else
        let val c = ch_at(src, p) in
          if is_space(c) then loop(p+1, ln, cl+1)
          else if c = 10 then loop(p+1, ln+1, 1)
          else @(p, ln, cl)
        end
  in loop(pos, line, col) end

fun skip_comment (src: string, pos: int, line: int, col: int): @(int, int, int) =
  let
    val n = g0u2i(string_length(src))
    fun loop (p: int, ln: int, cl: int): @(int, int, int) =
      if p >= n then @(p, ln, cl)
      else
        let val c = ch_at(src, p) in
          if c = 10 then @(p+1, ln+1, 1) else loop(p+1, ln, cl+1)
        end
  in loop(pos, line, col) end

fun match_kw (src: string, pos: int, kw: string): bool =
  let
    val kn = g0u2i(string_length(kw))
    val sn = g0u2i(string_length(src))
    fun loop (i: int): bool =
      if i >= kn then true
      else if pos + i >= sn then false
      else if ch_at(src, pos + i) = ch_at(kw, i) then loop(i+1)
      else false
  in loop(0) end

fun try_kw (src: string, pos: int, line: int, col: int): Option(@(token_t, int, int, int)) =
  let
    fun try1 (kw: string, kind: int): Option(@(token_t, int, int, int)) =
      let val kn = g0u2i(string_length(kw)) in
        if match_kw(src, pos, kw) then
          let val cnext = ch_at(src, pos + kn) in
            if is_alnum(cnext) then None()
            else Some(@(make_token(kind, line, col, kw), pos + kn, line, col + kn))
          end
        else None()
      end
  in
    case+ try1("indicate", 1) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("operator", 2) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("decompose", 8) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("disjoint", 19) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("commute", 15) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("unlock", 14) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("vector", 3) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("size_t", 4) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("frame", 12) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("lock", 13) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("pole", 16) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("loop", 6) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("shared", 72) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("linear", 73) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("join", 7) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("real", 5) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("gFun", 9) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("eFun", 10) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("act", 11) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("inv", 17) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("exit", 18) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("merge", 60) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("pivot", 61) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("live", 62) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("onpie", 63) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("gath", 64) of
    | Some(x) => Some(x)
    | None() =>
    case+ try1("prt", 20) of
    | Some(x) => Some(x)
    | None() => None()
  end

fun scan_ident (src: string, pos: int, line: int, col: int): @(token_t, int, int, int) =
  let
    val n = g0u2i(string_length(src))
    fun loop (p: int): int =
      if p >= n then p else if is_alnum(ch_at(src, p)) then loop(p+1) else p
    val endp = loop(pos)
    fun build (p: int, acc: string): string =
      if p >= endp then acc
      else
        let
          val c = ch_at(src, p)
          val s1 = $UN.castvwtp0{string}(string_sing($UN.cast{charNZ}(int2char0(c))))
          val acc2 = $UN.castvwtp0{string}(string_append(acc, s1))
        in build(p+1, acc2) end
    val text = build(pos, "")
  in
    @(make_token(50, line, col, text), endp, line, col + (endp - pos))
  end

fun scan_number (src: string, pos: int, line: int, col: int): @(token_t, int, int, int) =
  let
    val n = g0u2i(string_length(src))
    fun loop (p: int, has_dot: bool): int =
      if p >= n then p
      else
        let val c = ch_at(src, p) in
          if is_digit(c) then loop(p+1, has_dot)
          else if c = 46 && ~has_dot then loop(p+1, true)
          else p
        end
    val endp = loop(pos, false)
    fun build (p: int, acc: string): string =
      if p >= endp then acc
      else
        let
          val c = ch_at(src, p)
          val s1 = $UN.castvwtp0{string}(string_sing($UN.cast{charNZ}(int2char0(c))))
          val acc2 = $UN.castvwtp0{string}(string_append(acc, s1))
        in build(p+1, acc2) end
    val text = build(pos, "")
    fun has_dot (s: string, i: int): bool =
      if i >= g0u2i(string_length(s)) then false
      else if ch_at(s, i) = 46 then true else has_dot(s, i+1)
    val kind = if has_dot(text, 0) then 52 else 51
  in
    @(make_token(kind, line, col, text), endp, line, col + (endp - pos))
  end

fun raw_next (src: string, pos0: int, line0: int, col0: int): @(token_t, int, int, int) =
  let
    val @(pos, line, col) = skip_spaces(src, pos0, line0, col0)
    val n = g0u2i(string_length(src))
  in
    if pos >= n then @(make_token(58, line, col, ""), pos, line, col)
    else
      let val c = ch_at(src, pos) in
        if c = 59 && pos + 2 < n && ch_at(src, pos+1) = 59 && ch_at(src, pos+2) = 59 then
          let val @(p2, ln2, cl2) = skip_comment(src, pos+3, line, col+3) in
            raw_next(src, p2, ln2, cl2)
          end
        else if is_alpha(c) then
          case+ try_kw(src, pos, line, col) of
          | Some(x) => x
          | None() => scan_ident(src, pos, line, col)
        else if is_digit(c) then scan_number(src, pos, line, col)
        
        else if c = 92 && pos + 3 < n
                 && ch_at(src, pos+1) = 42
                 && ch_at(src, pos+2) = 121
                 && ch_at(src, pos+3) = 71 then
          @(make_token(70, line, col, "\\*yG"), pos+4, line, col+4)
        
        else if c = 47 && pos + 3 < n
                 && ch_at(src, pos+1) = 121
                 && ch_at(src, pos+2) = 71
                 && ch_at(src, pos+3) = 42 then
          @(make_token(71, line, col, "/yG*"), pos+4, line, col+4)
        else
          let
            val text = char_to_str(c)
            val kind = char_kind(c)
          in @(make_token(kind, line, col, text), pos+1, line, col+1) end
      end
  end

fun tokenize_all (src: string): List0(token_t) =
  let
    val acc = ref<List0(token_t)>(list_nil())
    fun collect (pos: int, line: int, col: int): void =
      let
        val @(t, p2, ln2, cl2) = raw_next(src, pos, line, col)
        val () = !acc := $UN.cast{List0(token_t)}(list_cons(t, !acc))
      in
        if token_is_eof(t) then () else collect(p2, ln2, cl2)
      end
    val () = collect(0, 1, 1)
  in list_vt2t(list_reverse(!acc)) end

implement lexer_create (src, filename) =
  Lexer(tokenize_all(src), ref(0))

implement lexer_destroy (Lexer(_, _)) = ()

fun list_nth_token (xs: List0(token_t), n: int): token_t =
  case+ xs of
  | list_nil() => make_token(58, 0, 0, "")
  | list_cons(x, xs2) => if n <= 0 then x else list_nth_token(xs2, n-1)

implement lexer_next (l) =
  let
    val Lexer(toks, idx) = l
    val i = !idx
    val t = list_nth_token(toks, i)
    val () = !idx := i + 1
  in t end

implement lexer_peek (l) =
  let val Lexer(toks, idx) = l in list_nth_token(toks, !idx) end

implement lexer_peek2 (l) =
  let val Lexer(toks, idx) = l in list_nth_token(toks, !idx + 1) end
