(** Types shared by every format reader. *)

(** An image format, as identified by magic bytes. *)
type format =
  | PNG
  | JPEG
  | GIF
  | WebP
  | HEIF
  | AVIF
  | TIFF
  | JXL
  | BMP
  | ICO
  | QOI

(** Metadata read from an image header. *)
type t =
  { format : format
  ; width : int (** Display width in pixels, with the orientation already applied. *)
  ; height : int (** Display height in pixels, with the orientation already applied. *)
  ; depth : int
    (** Bits per channel. BMP and ICO count bits per pixel, mapped to 8 for
            the packed truecolour depths and reported unchanged for indexed
            images. *)
  ; orientation : int
    (** EXIF orientation, 1 to 8, where 1 means no rotation. Values 5 to 8
            mean [width] and [height] are swapped relative to the stored pixel
            buffer. *)
  }

(** Why a read failed. *)
type error =
  | Unknown_format (** No format matched the leading bytes. *)
  | Truncated (** The source ended before the metadata did. *)
  | Malformed of string (** A header field was present but not usable. *)
  | Io_error of string (** The source could not be opened or read. *)

(** Raised by the [_exn] entry points in place of returning an error. *)
exception Imgmeta_error of error

(** [pp_error] prints an error for diagnostics. The wording is not stable. *)
val pp_error : Format.formatter -> error -> unit

(** [format_to_string] returns the lowercase short name, such as ["png"]. *)
val format_to_string : format -> string
