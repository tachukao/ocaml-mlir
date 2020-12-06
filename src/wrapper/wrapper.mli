module Context : sig
  type t

  (** Creates an MLIR context and transfers its ownership to the caller. *)
  val create : unit -> t

  (** Checks if two contexts are equal. *)
  val equal : t -> t -> bool

  (** Checks whether a context is null. *)
  val is_null : t -> bool

  (** Returns the number of dialects loaded by the context.*)
  val num_loaded_dialects : t -> int
end
