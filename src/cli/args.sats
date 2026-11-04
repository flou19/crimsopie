#define ATS_PACKNAME "crimsopie.cli"

datatype build_mode =
  | Mode_Build | Mode_Check | Mode_Lex | Mode_Parse
  | Mode_Emit_PU4 | Mode_Emit_ATS2 | Mode_Help | Mode_Version

datatype cli_args_t = CliArgs of (build_mode, string, string, bool)

fun cli_parse (argc: int, argv: ptr): cli_args_t
fun cli_usage (): void
fun cli_version (): void
