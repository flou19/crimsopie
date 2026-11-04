#define ATS_PACKNAME "crimsopie.utils.io"

fun read_file  (path: string): Option(string)
fun write_file (path: string, content: string): bool
fun path_replace_ext (path: string, new_ext: string): string
