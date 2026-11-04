#define ATS_DYNLOADFLAG 0
#include "share/atspre_staload.hats"
staload UN = "prelude/SATS/unsafe.sats"
staload "src/utils/io.sats"

implement read_file (path) =
  let
    val opt = fileref_open_opt (path, file_mode_r)
  in
    case+ opt of
    | ~None_vt () => None ()
    | ~Some_vt (inp) =>
      let
        val acc = ref<string>("")
        fun loop (): void =
          let
            val c = fileref_getc (inp)
          in
            if c >= 0 then
              let
                val ch = int2char0 (c)
                val s1 = $UN.castvwtp0{string}(string_sing ($UN.cast{charNZ}(ch)))
                val () = !acc := $UN.castvwtp0{string}(string_append (!acc, s1))
              in loop () end
            else ()
          end
        val () = loop ()
        val () = fileref_close (inp)
      in Some (!acc) end
  end

implement write_file (path, content) =
  let
    val opt = fileref_open_opt (path, file_mode_w)
  in
    case+ opt of
    | ~Some_vt (out) =>
      let
        val () = fprint_string (out, content)
        val () = fileref_close (out)
      in true end
    | ~None_vt () => false
  end

implement path_replace_ext (path, new_ext) =
  let
    val n = g0u2i(string_length (path))
    fun find_dot (i: int): int =
      if i < 0 then ~1
      else if char2int0($UN.ptr0_get_at<char>(string2ptr(path), i)) = 46 then i
      else find_dot (i - 1)
    val dot = find_dot (n - 1)
    fun take (i: int, acc: string): string =
      if i >= (if dot < 0 then n else dot) then acc
      else
        let
          val c = char2int0($UN.ptr0_get_at<char>(string2ptr(path), i))
          val s1 = $UN.castvwtp0{string}(string_sing($UN.cast{charNZ}(int2char0(c))))
          val acc2 = $UN.castvwtp0{string}(string_append(acc, s1))
        in take (i + 1, acc2) end
    val base = take (0, "")
  in
    $UN.castvwtp0{string}(string_append (base, new_ext))
  end
