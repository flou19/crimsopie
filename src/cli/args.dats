#define ATS_DYNLOADFLAG 0
#include "share/atspre_staload.hats"
staload "src/cli/args.sats"
staload UN = "prelude/SATS/unsafe.sats"

implement cli_usage () =
  begin
    print_string ("crimsopie - CM/P compiler for computational operator theory\n");
    print_string ("Usage: crimsopie [mode] <input.pu8> [-o <output>]\n");
    print_string ("Modes:\n");
    print_string ("  --build      full pipeline .pu8 -> .p0\n");
    print_string ("  --check      parse + verify only\n");
    print_string ("  --lex        tokenize only\n");
    print_string ("  --parse      parse to AST only\n");
    print_string ("  --emit-pu4   emit .pu4 intermediate\n");
    print_string ("  --emit-ats2  emit .sats/.dats\n");
    print_string ("  --help       this help\n");
    print_string ("  --version    version info\n")
  end

implement cli_version () =
  print_string ("crimsopie (CM/P) 0.0.1-alpha - pure ATS2\n")

implement cli_parse (argc, argv) =
  let
    fun get_arg (i: int): string =
      if i >= argc then ""
      else $UN.ptr0_get_at<string> ($UN.cast{ptr}(argv), i)
    fun loop (i: int, mode: build_mode, input: string, output: string, verbose: bool): cli_args_t =
      if i >= argc then CliArgs (mode, input, output, verbose)
      else
        let
          val a = get_arg (i)
        in
          if a = "--build" orelse a = "build" then loop (i + 1, Mode_Build, input, output, verbose)
          else if a = "--check" orelse a = "check" then loop (i + 1, Mode_Check, input, output, verbose)
          else if a = "--lex" orelse a = "lex" then loop (i + 1, Mode_Lex, input, output, verbose)
          else if a = "--parse" orelse a = "parse" then loop (i + 1, Mode_Parse, input, output, verbose)
          else if a = "--emit-pu4" then loop (i + 1, Mode_Emit_PU4, input, output, verbose)
          else if a = "--emit-ats2" then loop (i + 1, Mode_Emit_ATS2, input, output, verbose)
          else if a = "--help" orelse a = "help" then loop (i + 1, Mode_Help, input, output, verbose)
          else if a = "--version" orelse a = "version" then loop (i + 1, Mode_Version, input, output, verbose)
          else if a = "-o" then loop (i + 2, mode, input, get_arg (i + 1), verbose)
          else if a = "-v" then loop (i + 1, mode, input, output, true)
          else loop (i + 1, mode, a, output, verbose)
        end
  in
    loop (1, Mode_Build, "", "", false)
  end
