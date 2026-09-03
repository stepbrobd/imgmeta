(lang dune 3.21)

(generate_opam_files true)
(executables_implicit_empty_intf true)

(name imgmeta)
(version 2026.517.1)
(license MIT)
(authors "Yifei Sun <ysun@hey.com>")
(maintainers "Yifei Sun <ysun@hey.com>")

(source
 (github stepbrobd/imgmeta))

(package
 (name imgmeta)
 (synopsis "Header-only image metadata reader")
 (description
  "Reads dimensions, bit depth and orientation from PNG, JPEG, GIF, WebP, HEIF, AVIF, TIFF, JPEG XL, BMP, ICO and QOI without decoding pixels.")
 (tags
  (image metadata exif png jpeg gif webp heif avif tiff jxl))
 (depends
  (ocaml
   (>= 4.14))
  (alcotest :with-test)
  (odoc :with-doc)))
