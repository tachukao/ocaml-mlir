module Context : sig
  type t

  (** Creates an MLIR context and transfers its ownership to the caller. *)
  val create : unit -> t

  val num_loaded_dialects : t -> int
end
