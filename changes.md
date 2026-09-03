# Changelog

## Unreleased

- Add TIFF, JPEG XL, BMP, ICO, and QOI readers.
- Detect HEIF and AVIF from the full compatible brand list rather than the major
  brand alone.
- Select HEIF and AVIF properties through `pitm` and `ipma` instead of taking
  the first `ispe`, `pixi`, and `irot` under `ipco`.
- Bound every read against the source size. A malformed length can no longer
  raise `Invalid_argument` out of the result-returning API or drive an unbounded
  allocation.
- Guarantee forward progress in the PNG, WebP, and ISOBMFF walkers, which could
  loop forever on a negative chunk or box length.
- Close the file descriptor in `of_file` even when parsing raises.
- Support `of_in_channel` on non-seekable channels such as pipes.
- Declare the `ocaml` lower bound and the test dependency in the opam package.

## 2026.517.1

- Initial PNG, JPEG, GIF, WebP, HEIF, and AVIF support.
