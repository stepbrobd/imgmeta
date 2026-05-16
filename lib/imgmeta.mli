include module type of Types
module Reader = Reader
module Magic = Magic
module Formats = Formats

val of_file : string -> (t, error) result
val of_bytes : bytes -> (t, error) result
val of_in_channel : In_channel.t -> (t, error) result
val of_file_exn : string -> t
val of_bytes_exn : bytes -> t
val of_in_channel_exn : In_channel.t -> t
val detect_format : bytes -> format option
val pp : Format.formatter -> t -> unit
