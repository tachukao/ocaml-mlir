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

  (** Prints the type to the standard error stream. *)
  val dump : t -> unit
end

and Region : sig
  type t

  (** Creates a new empty region and transfers ownership to the caller. *)
  val create : unit -> t

  (** Takes a region owned by the caller and destroys it. *)
  val destroy : t -> unit

  (** Gets the first block in the region. *)
  val first_block : t -> Block.t

  (** Takes a block owned by the caller and appends it to the given region. *)
  val append_owned_block : t -> Block.t -> unit

  (** Takes a block owned by the caller and inserts it at `pos` to the given region. This is an expensive operation that linearly scans the region, prefer insertAfter/Before instead. *)
  val insert_owned_block_before : t -> Block.t -> Block.t -> unit

  (** Takes a block owned by the caller and inserts it after the (non-owned)
 * reference block in the given region. The reference block must belong to the
 * region. If the reference block is null, prepends the block to the region. *)
  val insert_owned_block_after : t -> Block.t -> Block.t -> unit
end

and Location : sig
  type t

  (** Creates a location with unknown position owned by the given context. *)
  val unknown : Context.t -> t
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
  val add_owned_regions : t -> Region.t list -> unit

  (** Adds a list of operands to the operation state. *)
  val add_operands : t -> Value.t list -> unit
end

and Operation : sig
  type t

  (** Creates an operation and transfers ownership to the caller. *)
  val create : OperationState.t -> t

  (** Takes an operation owned by the caller and destroys it. *)
  val destroy : t -> unit

  (** Checks whether the underlying operation is null. *)
  val is_null : Operation.t -> bool

  (** Returns `pos`-th region attached to the operation. *)
  val region : t -> int -> Region.t

  (** Returns `pos`-th result of the operation. *)
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

  (** Returns the first operation in the block. *)
  val first_operation : t -> Operation.t

  (** Takes an operation owned by the caller and inserts it as `pos` to the block. This is an expensive operation that scans the block linearly, prefer insertBefore/After instead. *)
  val insert_owned_operation : t -> int -> Operation.t -> unit

  (** Takes an operation owned by the caller and inserts it before the (non-owned) reference operation in the given block. If the reference is null, appends the operation. Otherwise, the reference must belong to the block. *)
  val insert_owned_operation_before : t -> Operation.t -> Operation.t -> unit

  (** Takes an operation owned by the caller and inserts it after the (non-owned) reference operation in the given block. If the reference is null, prepends the operation. Otherwise, the reference must belong to the block. *)
  val insert_owned_operation_after : t -> Operation.t -> Operation.t -> unit

  (** Takes an operation owned by the caller and appends it to the block. *)
  val append_owned_operation : t -> Operation.t -> unit
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

and BuiltinTypes : sig
  module Integer : sig
    (** Checks whether the given type is an integer type. *)
    val is_integer : Type.t -> bool

    (** Creates a signless integer type of the given bitwidth in the context. The type is owned by the context. *)
    val get : Context.t -> int -> Type.t

    (** Creates a signed integer type of the given bitwidth in the context. The type is owned by the context. *)
    val signed : Context.t -> int -> Type.t

    (** Creates an unsigned integer type of the given bitwidth in the context. The type is owned by the context. *)
    val unsigned : Context.t -> int -> Type.t

    (** Returns the bitwidth of an integer type. *)
    val width : Type.t -> int

    (** Checks whether the given integer type is signless. *)
    val is_signless : Type.t -> bool

    (** Checks whether the given integer type is signed. *)
    val is_signed : Type.t -> bool

    (** Checks whether the given integer type is unsigned. *)
    val is_unsigned : Type.t -> bool
  end

  module Float : sig
    (** Checks whether the given type is a bf16 type. *)
    val is_bf16 : Type.t -> bool

    (** Creates a bf16 type in the given context. The type is owned by the context. *)
    val bf16 : Context.t -> Type.t

    (** Checks whether the given type is an f16 type. *)
    val is_f16 : Type.t -> bool

    (** Creates an f16 type in the given context. The type is owned by the context. *)
    val f16 : Context.t -> Type.t

    (** Checks whether the given type is an f32 type. *)
    val is_f32 : Type.t -> bool

    (** Creates an f32 type in the given context. The type is owned by the context. *)
    val f32 : Context.t -> Type.t

    (** Checks whether the given type is an f64 type. *)
    val is_f64 : Type.t -> bool

    (** Creates a f64 type in the given context. The type is owned by the context. *)
    val f64 : Context.t -> Type.t
  end

  module Index : sig
    (** Checks whether the given type is an index type. *)
    val is_index : Type.t -> bool

    (** Creates an index type in the given context. The type is owned by the context. *)
    val get : Context.t -> Type.t
  end

  module None : sig
    (** Checks whether the given type is a None type. *)
    val is_none : Type.t -> bool

    (** Creates a None type in the given context. The type is owned by the context. *)
    val get : Context.t -> Type.t
  end

  module Complex : sig
    (** Checks whether the given type is a Complex type. *)
    val is_complex : Type.t -> bool

    (** Creates a complex type with the given element type in the same context as the element type. The type is owned by the context. *)
    val get : Type.t -> Type.t

    (** Returns the element type of the given complex type. *)
    val element_type : Type.t -> Type.t
  end
end

(** Registers all dialects known to core MLIR with the provided Context.
   This is needed before creating IR for these Dialects. *)
val register_all_dialects : Context.t -> unit

(* [with_context f]  creates a context [ctx], applies [f] to it, destroys it and returns the result of applying [f] *)
val with_context : (Context.t -> 'a) -> 'a
