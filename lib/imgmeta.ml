include Types
module Reader = Reader
module Magic = Magic
module Formats = Formats

let pp fmt m =
  Format.fprintf
    fmt
    "{format=%s width=%d height=%d depth=%d orientation=%d}"
    (format_to_string m.format)
    m.width
    m.height
    m.depth
    m.orientation
;;

let detect_format = Magic.of_bytes

let dispatch r = function
  | PNG -> Formats.Png.read_metadata r
  | JPEG -> Formats.Jpeg.read_metadata r
  | GIF -> Formats.Gif.read_metadata r
  | WebP -> Formats.Webp.read_metadata r
  | HEIF -> Formats.Heif.read_metadata r
  | AVIF -> Formats.Avif.read_metadata r
;;

let read r =
  match Magic.detect r with
  | Error e -> Error e
  | Ok format -> dispatch r format
;;

let of_bytes b = read (Reader.of_bytes b)

let of_file path =
  match Reader.of_file path with
  | Error e -> Error e
  | Ok r ->
    let result = read r in
    Reader.close r;
    result
;;

let of_in_channel ic =
  match Reader.of_in_channel ic with
  | Error e -> Error e
  | Ok r -> read r
;;

let raise_or_return = function
  | Ok v -> v
  | Error e -> raise (Imgmeta_error e)
;;

let of_bytes_exn b = raise_or_return (of_bytes b)
let of_file_exn p = raise_or_return (of_file p)
let of_in_channel_exn ic = raise_or_return (of_in_channel ic)
