type format =
  | PNG
  | JPEG
  | GIF
  | WebP
  | HEIF
  | AVIF

type t =
  { format : format
  ; width : int
  ; height : int
  ; depth : int
  }

type error =
  | Unknown_format
  | Truncated
  | Malformed of string
  | Io_error of string

exception Imgmeta_error of error

val pp_error : Format.formatter -> error -> unit
val format_to_string : format -> string
