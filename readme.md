# imgmeta

Metadata reader for PNG, JPEG, GIF, WebP, HEIF, and AVIF. Reads "just enough"
bytes to extract dimensions, depth, and orientation without decoding the image.

Put this in dune:

```dune
(libraries imgmeta)
```

API:

```ocaml
Imgmeta.of_file       : string     -> (t, error) result
Imgmeta.of_bytes      : bytes      -> (t, error) result
Imgmeta.of_in_channel : in_channel -> (t, error) result
```

`*_exn` variants are also exposed and raise `Imgmeta_error` on failure (see
below).

```ocaml
match Imgmeta.of_file "photo.avif" with
| Ok { format; width; height; depth; orientation } ->
  Printf.printf "%dx%d (depth=%d, orient=%d)\n" width height depth orientation
| Error e -> Format.eprintf "%a@." Imgmeta.pp_error e
```

You get:

```ocaml
type t = {
  format      : format;  (* PNG | JPEG | GIF | WebP | HEIF | AVIF *)
  width       : int;     (* display width  (post-rotation)        *)
  height      : int;     (* display height (post-rotation)        *)
  depth       : int;     (* bits per channel                      *)
  orientation : int;     (* EXIF 1..8, 1 means no rotation        *)
}
```

`width` and `height` are the dimensions a browser will actually render. If the
image carries an EXIF `Orientation` tag of 5/6/7/8 (or an HEIF/AVIF `irot` box
equivalent), the dimensions are swapped from the raw pixel buffer. Use
`orientation` if you need the original tag.

Error type:

```ocaml
type error =
  | Unknown_format
  | Truncated
  | Malformed of string
  | Io_error of string
```

Format detection sniffs the first 16 bytes, returns `None` for unrecognized
inputs:

```ocaml
Imgmeta.detect_format : bytes -> format option
```
