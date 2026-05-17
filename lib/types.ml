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
  ; orientation : int
  }

type error =
  | Unknown_format
  | Truncated
  | Malformed of string
  | Io_error of string

exception Imgmeta_error of error

let format_to_string = function
  | PNG -> "png"
  | JPEG -> "jpeg"
  | GIF -> "gif"
  | WebP -> "webp"
  | HEIF -> "heif"
  | AVIF -> "avif"
;;

let pp_error fmt = function
  | Unknown_format -> Format.pp_print_string fmt "unknown_format"
  | Truncated -> Format.pp_print_string fmt "truncated"
  | Malformed s -> Format.fprintf fmt "malformed(%s)" s
  | Io_error s -> Format.fprintf fmt "io_error(%s)" s
;;
