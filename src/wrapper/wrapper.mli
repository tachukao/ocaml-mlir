module Context : sig
  type t

  (** Creates an MLIR context and transfers its ownership to the caller. *)
  val create : unit -> t

  (** Checks if two contexts are equal. *)
  val equal : t -> t -> bool

  (** Checks whether a context is null. *)
  val is_null : t -> bool

  (** Returns the number of dialects registered with the given context. A registered dialect will be loaded if needed by the parser. *)
  val num_registered_dialects : t -> int

  (** Returns the number of dialects loaded by the context.*)
  val num_loaded_dialects : t -> int
end

module Block : sig
  type t
end

module Location : sig
  type t

  (** Creates a location with unknown position owned by the given context. *)
  val unknown_get : Context.t -> t
end

module Module : sig
  type t

  (** Creates a new, empty module and transfers ownership to the caller. *)
  val empty : Location.t -> t

  (** Gets the body of the module, i.e. the only block it contains. *)
  val body : t -> Block.t
end

module Type : sig
  type t

  (** Parses a type. The type is owned by the context. *)
  val parse : Context.t -> string -> t
end

(** Registers all dialects known to core MLIR with the provided Context.
   This is needed before creating IR for these Dialects. *)
val register_all_dialects : Context.t -> unit
