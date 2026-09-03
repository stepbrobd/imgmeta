(** Reads dimensions, bit depth and orientation from an image header without
    decoding pixel data.

    Every entry point identifies the format from magic bytes alone. A file name
    or extension is never consulted. *)

include module type of Types

(** [of_file path] reads metadata from the file at [path]. The descriptor is
    closed before returning, including when parsing fails. *)
val of_file : string -> (t, error) result

(** [of_bytes b] reads metadata from an image already held in memory. *)
val of_bytes : bytes -> (t, error) result

(** [of_in_channel ic] reads metadata from [ic], which need not be seekable. A
    pipe is read forward only as far as the metadata requires. The channel is
    left open and its position is unspecified afterwards. *)
val of_in_channel : In_channel.t -> (t, error) result

(** [of_file_exn] is {!of_file}, raising {!Imgmeta_error} instead of returning
    an error. *)
val of_file_exn : string -> t

(** [of_bytes_exn] is {!of_bytes}, raising {!Imgmeta_error} instead of returning
    an error. *)
val of_bytes_exn : bytes -> t

(** [of_in_channel_exn] is {!of_in_channel}, raising {!Imgmeta_error} instead of
    returning an error. *)
val of_in_channel_exn : In_channel.t -> t

(** [detect_format b] identifies the format from the leading bytes of [b],
    returning [None] when nothing matches. Up to 64 bytes are examined, enough
    to read the ISOBMFF compatible brand list. *)
val detect_format : bytes -> format option

(** [pp] prints a metadata record for debugging. The layout is not stable. *)
val pp : Format.formatter -> t -> unit

(** Positioned reads over bytes, a file, or a channel. Every parser below takes
    one. The type is abstract, and reads are bounded by the source size. *)
module Reader = Reader

(** Format identification from magic bytes. {!detect_format} wraps
    {!Magic.of_bytes} for the common case. *)
module Magic = Magic

(** The per-format parsers. Reach for one only when the format is already known
    from outside the file, such as a verified Content-Type. Detection through
    {!of_bytes} stays the safe default. *)
module Formats = Formats
