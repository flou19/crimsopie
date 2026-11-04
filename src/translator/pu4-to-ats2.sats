#define ATS_PACKNAME "crimsopie.translator"
staload "src/parser/ast.sats"

fun translate_program (p: program_t, src_file: string, dats_out: string, sats_out: string): bool
