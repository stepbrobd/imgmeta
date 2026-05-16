type t

val of_bytes : bytes -> t
val of_file : string -> (t, Types.error) result
val of_in_channel : In_channel.t -> (t, Types.error) result
val read : t -> len:int -> bytes
val read_at : t -> pos:int -> len:int -> bytes
val seek : t -> int -> unit
val pos : t -> int
val size : t -> int option
val close : t -> unit
