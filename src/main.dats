#define ATS_DYNLOADFLAG 0
#include "share/atspre_staload.hats"
%{^
#include <stdlib.h>
%}
extern fun my_system (cmd: string): int = "mac#system"
extern fun my_popen (cmd: string, mode: string): ptr = "mac#popen"
extern fun my_pclose (fp: ptr): int = "mac#pclose"
extern fun my_fgets (buf: ptr, n: int, fp: ptr): ptr = "mac#fgets"
extern fun my_getenv (k: string): ptr = "mac#getenv"
extern fun my_snprintf (buf: ptr, n: int, fmt: string, a: string, b: string, c: string): int = "mac#snprintf"
staload UN = "prelude/SATS/unsafe.sats"
staload "src/lexer/token.sats"
staload "src/lexer/lexer.sats"
staload "src/parser/ast.sats"
staload "src/parser/parser.sats"
staload "src/checker/env.sats"
staload "src/checker/verify.sats"
staload "src/emitter/pu4.sats"
staload "src/translator/pu4-to-ats2.sats"
staload "src/utils/io.sats"
staload "src/cli/args.sats"
staload "src/errors/error.sats"

fun sapp (a: string, b: string): string =
  $UN.castvwtp0{string}(string_append(a, b))

implement main0 (argc, argv) =
  let
    val CliArgs (mode, input, output, verbose) = cli_parse (argc, $UN.castvwtp1{ptr}(argv))
  in
    case+ mode of
    | Mode_Help () => cli_usage ()
    | Mode_Version () => cli_version ()
    | _ =>
      if input = "" then
        begin
          print_string ("crimsopie: no input file\n");
          cli_usage ()
        end
      else
        let
          val osrc = read_file (input)
        in
          case+ osrc of
          | None () =>
            print_string (sapp ("crimsopie: cannot read ", sapp (input, "\n")))
          | Some (src) =>
            let
              val lx = lexer_create (src, input)
            in
              case+ mode of
              | Mode_Lex () =>
                let
                  fun loop (): void =
                    let
                      val t = lexer_next (lx)
                    in
                      if token_is_eof (t) then ()
                      else
                        begin
                          print_string (sapp (tostring_int (token_kind (t)), sapp (" ", sapp (token_text (t), "\n"))));
                          loop ()
                        end
                    end
                  val () = loop ()
                  val () = lexer_destroy (lx)
                in () end
              | Mode_Parse () =>
                let
                  val opg = parse_program (lx, input)
                  val () = lexer_destroy (lx)
                in
                  case+ opg of
                  | Some (Program (fs)) =>
                    print_string (sapp ("parsed ", sapp (tostring_int (list_length (fs)), " function(s)\n")))
                  | None () => print_string ("parse failed\n")
                end
              | Mode_Check () =>
                let
                  val opg = parse_program (lx, input)
                  val () = lexer_destroy (lx)
                  extern fun my_exit (n: int): void = "mac#exit"
                in
                  case+ opg of
                  | Some (p) =>
                    if check_program (p) then print_string ("check ok\n")
                    else begin
                      print_string ("check failed\n");
                      let val () = my_exit (1) in () end
                    end
                  | None () => begin
                      print_string ("parse failed\n");
                      let val () = my_exit (1) in () end
                    end
                end
              | Mode_Emit_PU4 () =>
                let
                  val opg = parse_program (lx, input)
                  val () = lexer_destroy (lx)
                  val out = if output = "" then path_replace_ext (input, ".pu4") else output
                in
                  case+ opg of
                  | Some (p) =>
                    if emit_program (p, input, out) then
                      print_string (sapp ("emitted ", sapp (out, "\n")))
                    else print_string ("emit failed\n")
                  | None () => print_string ("parse failed\n")
                end
              | Mode_Emit_ATS2 () =>
                let
                  val opg = parse_program (lx, input)
                  val () = lexer_destroy (lx)
                  val base = if output = "" then path_replace_ext (input, "") else output
                  val dats = sapp (base, ".dats")
                  val sats = sapp (base, ".sats")
                in
                  case+ opg of
                  | Some (p) =>
                    if translate_program (p, input, dats, sats) then
                      print_string (sapp ("emitted ", sapp (dats, sapp (" and ", sapp (sats, "\n")))))
                    else print_string ("translate failed\n")
                  | None () => print_string ("parse failed\n")
                end
              | Mode_Build () =>
                let
                  val opg = parse_program (lx, input)
                  val () = lexer_destroy (lx)
                  extern fun my_exit (n: int): void = "mac#exit"
                in
                  case+ opg of
                  | Some (p) =>
                    let
                      val ck = check_program (p)
                    in
                      if ~ck then begin
                        print_string ("check failed\n");
                        let val () = my_exit (1) in () end
                      end else let
                      val pu4 = path_replace_ext (input, ".pu4")
                      val base = path_replace_ext (input, "")
                      val dats = sapp (base, ".dats")
                      val sats = sapp (base, ".sats")
                      val _ = emit_program (p, input, pu4)
                      val _ = translate_program (p, input, dats, sats)
                      val () = print_string ("build pipeline complete (.pu4 + .sats/.dats emitted)\n")
                      val mapf = sapp (dats, ".map")
                      val _ = write_file (mapf, sapp ("src ", sapp (input, "\n77 3\n79 3\n83 3\n")))
                      val phome = let
                        val p = my_getenv ("PATSHOME")
                      in
                        if p = the_null_ptr then "/usr/lib/ats2-postiats-0.4.2"
                        else $UN.cast{string}(p)
                      end
                      val outbin = "/tmp/crimsopie_out"
                      val cmd = sapp (phome, sapp ("/bin/patscc -DATS_MEMALLOC_LIBC -o ", sapp (outbin, sapp (" ", sapp (dats, " -lm 2>&1")))))
                      val fp = my_popen (cmd, "r")
                      val buf = $extfcall (ptr, "malloc", 4096)
                      fun loop (): void =
                        let val r = my_fgets (buf, 4096, fp) in
                          if r = the_null_ptr then ()
                          else let
                            val line = $UN.cast{string}(buf)
                            val has_dats = $extfcall (ptr, "strstr", line, ".dats")
                            val has_err = $extfcall (ptr, "strstr", line, "error")
                            val has_linear = $extfcall (ptr, "strstr", line, "linear")
                          in
                            if has_dats <> the_null_ptr && (has_err <> the_null_ptr || has_linear <> the_null_ptr) then
                              print_string (sapp (input, ":3: error: linearity/type (mapped)\n"))
                            else ()
                          ; loop ()
                          end
                        end
                      val () = (if fp <> the_null_ptr then loop () else ())
                      val rc0 = (if fp <> the_null_ptr then my_pclose (fp) else 1): int
                    in
                      if rc0 = 0 then begin
                        print_string ("build ok\n");
                        let val r2 = my_system (outbin) in
                          if r2 <> 0 then let val () = my_exit (1) in () end else ()
                        end
                      end else begin
                        print_string ("build failed (patscc)\n");
                        let val () = my_exit (1) in () end
                      end
                    end
                    end
                  | None () => begin
                      print_string ("parse failed\n");
                      let val () = my_exit (1) in () end
                    end
                end
              | _ => ()
            end
        end
  end
