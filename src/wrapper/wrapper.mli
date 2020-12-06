module rec Context : sig
  type t

  (** Creates an MLIR context and transfers its ownership to the caller. *)
  val create : unit -> t

  (** Checks if two contexts are equal. *)
  val equal : t -> t -> bool

  (* Takes an MLIR context owned by the caller and destroys it. *)
  val destroy : t -> unit

  (** Checks whether a context is null. *)
  val is_null : t -> bool

  (** Returns the number of dialects registered with the given context. A registered dialect will be loaded if needed by the parser. *)
  val num_registered_dialects : t -> int

  (** Returns the number of dialects loaded by the context.*)
  val num_loaded_dialects : t -> int
end

and Type : sig
  type t

  (** Parses a type. The type is owned by the context. *)
  val parse : Context.t -> string -> t
end

and Region : sig
  type t

  (** Creates a new empty region and transfers ownership to the caller. *)
  val create : unit -> t

  (* Takes a region owned by the caller and destroys it. *)
  val destroy : t -> unit

  (** Takes a block owned by the caller and appends it to the given region. *)
  val append_block : t -> Block.t -> unit
end

and Location : sig
  type t

  (** Creates a location with unknown position owned by the given context. *)
  val unknown_get : Context.t -> t
end

and NamedAttribute : sig
  type t
end

and Attribute : sig
  type t

  (* Parses an attribute. The attribute is owned by the context. *)
  val parse : Context.t -> string -> t

  (* Associates an attribute with the name. Takes ownership of neither. *)
  val name : string -> t -> NamedAttribute.t
end

and OperationState : sig
  type t

  (** Constructs an operation state from a name and a location. *)
  val get : string -> Location.t -> t

  (** Adds a list of results to the operation state. *)
  val add_results : t -> Type.t list -> unit

  (** Adds a list of named attributes to the operation state. *)
  val add_named_attributes : t -> NamedAttribute.t list -> unit

  (** Adds a list of regions to the operation state. *)
  val add_regions : t -> Region.t list -> unit

  (** Adds a list of operands to the operation state. *)
  val add_operands : t -> Value.t list -> unit
end

and Operation : sig
  type t

  (** Creates an operation and transfers ownership to the caller. *)
  val create : OperationState.t -> t

  (* Returns `pos`-th result of the operation. *)
  val result : t -> int -> Value.t

  (** Prints an operation to stderr. *)
  val dump : t -> unit
end

and Value : sig
  type t
end

and Block : sig
  type t

  (** Creates a new empty block with the given argument types and transfers ownership to the caller. *)
  val create : Type.t list -> t

  (** Takes a block owned by the caller and destroys it. *)
  val destroy : t -> unit

  (** Returns `pos`-th argument of the block. *)
  val argument : t -> int -> Value.t

  (** Takes an operation owned by the caller and inserts it as `pos` to the block. This is an expensive operation that scans the block linearly, prefer insertBefore/After instead. *)
  val insert_operation : t -> int -> Operation.t -> unit

  (** Takes an operation owned by the caller and appends it to the block. *)
  val append_operation : t -> Operation.t -> unit
end

and Module : sig
  type t

  (** Creates a new, empty module and transfers ownership to the caller. *)
  val empty : Location.t -> t

  (** Gets the body of the module, i.e. the only block it contains. *)
  val body : t -> Block.t

  (* Views the module as a generic operation. *)
  val operation : t -> Operation.t
end

(** Registers all dialects known to core MLIR with the provided Context.
   This is needed before creating IR for these Dialects. *)
val register_all_dialects : Context.t -> unit

(* [with_context f]  creates a context [ctx], applies [f] to it, destroys it and returns the result of applying [f] *)
val with_context : (Context.t -> 'a) -> 'a

(* [with_region f]  creates a region [reg], applies [f] to it, destroys it and returns the result of applying [f] *)
val with_region : (Region.t -> 'a) -> 'a

(* [with_block typs f]  creates a block [blk] with types [typs], applies [f] to it, destroys it and returns the result of applying [f] *)
val with_block : Type.t list -> (Block.t -> 'a) -> 'a
