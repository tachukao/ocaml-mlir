module IR : sig
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

    (** Sets whether unregistered dialects are allowed in this context. *)
    val set_allow_unregistered_dialects : t -> bool -> unit

    (** Returns whether the context allows unregistered dialects. *)
    val get_allow_unregistered_dialects : t -> bool

    (** Gets the dialect instance owned by the given context using the dialect namespace to identify it, loads (i.e., constructs the instance of) the dialect if necessary. If the dialect is not registered with the context, returns null. Use mlirContextLoad<Name>Dialect to load an unregistered dialect. *)
    val get_or_load_dialect : t -> string -> Dialect.t
  end

  and Dialect : sig
    type t

    (** Returns the context that owns the dialect. *)
    val context : t -> Context.t

    (** Checks if the dialect is null. *)
    val is_null : t -> bool

    (** Checks if two dialects that belong to the same context are equal. Dialects from different contexts will not compare equal. *)
    val equal : t -> t -> bool

    (** Returns the namespace of the given dialect. *)
    val namespace : t -> string
  end

  and Type : sig
    type t

    (** Checks if two types are equal. *)
    val equal : t -> t -> bool

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

    (** Parses a module from the string and transfers ownership to the caller. *)
    val parse : Context.t -> string -> t

    (** Checks whether a module is null. *)
    val is_null : t -> bool

    (** Takes a module owned by the caller and deletes it. *)
    val destroy : t -> unit

    (** Gets the context that a module was created with. *)
    val context : t -> Context.t

    (** Gets the body of the module, i.e. the only block it contains. *)
    val body : t -> Block.t

    (* Views the module as a generic operation. *)
    val operation : t -> Operation.t
  end
end

module rec BuiltinTypes : sig
  open IR

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

  module Vector : sig
    (** Checks whether the given type is a Vector type. *)
    val is_vector : Type.t -> bool

    (** Creates a vector type of the shape identified by its rank and dimensions, with the given element type in the same context as the element type. The type is owned by the context. *)
    val get : int array -> Type.t -> Type.t

    (** Same as "mlirVectorTypeGet" but returns a nullptr wrapping MlirType on illegal arguments, emitting appropriate diagnostics. *)
    val get_checked : int array -> Type.t -> Location.t -> Type.t
  end

  module Tensor : sig
    (** Checks whether the given type is a Tensor type. *)
    val is_tensor : Type.t -> bool

    (** Checks whether the given type is a ranked tensor type. *)
    val is_ranked_tensor : Type.t -> bool

    (** Checks whether the given type is an unranked tensor type. *)
    val is_unranked_tensor : Type.t -> bool

    (** Creates a tensor type of a fixed rank with the given shape and element type in the same context as the element type. The type is owned by the context. *)
    val ranked : int array -> Type.t -> Type.t

    (** Same as "mlirRankedTensorTypeGet" but returns a nullptr wrapping MlirType on illegal arguments, emitting appropriate diagnostics. *)
    val ranked_checked : int array -> Type.t -> Location.t -> Type.t

    (** Creates an unranked tensor type with the given element type in the same context as the element type. The type is owned by the context. *)
    val unranked : Type.t -> Type.t

    (* Same as "mlirUnrankedTensorTypeGet" but returns a nullptr wrapping MlirType on illegal arguments, emitting appropriate diagnostics. *)
    val unranked_checked : Type.t -> Location.t -> Type.t
  end

  module MemRef : sig
    (** Checks whether the given type is a MemRef type. *)
    val is_memref : Type.t -> bool

    (** Checks whether the given type is an UnrankedMemRef type. *)
    val is_unranked_memref : Type.t -> bool

    (** Creates a MemRef type with the given rank and shape, a potentially empty list of affine layout maps, the given memory space and element type, in the same context as element type. The type is owned by the context. *)
    val get : Type.t -> int array -> AffineMap.t list -> int -> Type.t

    (** Creates a MemRef type with the given rank, shape, memory space and element type in the same context as the element type. The type has no affine maps, i.e. represents a default row-major contiguous memref. The type is owned by the context. *)
    val contiguous : Type.t -> int array -> int -> Type.t

    (** Same as "mlirMemRefTypeContiguousGet" but returns a nullptr wrapping MlirType on illegal arguments, emitting appropriate diagnostics. *)
    val contiguous_checked : Type.t -> int array -> int -> Location.t -> Type.t

    (** Creates an Unranked MemRef type with the given element type and in the given memory space. The type is owned by the context of element type. *)
    val unranked : Type.t -> int -> Type.t

    (** Same as "mlirUnrankedMemRefTypeGet" but returns a nullptr wrapping MlirType on illegal arguments, emitting appropriate diagnostics. *)
    val unranked_checked : Type.t -> int -> Location.t -> Type.t

    (** Returns the number of affine layout maps in the given MemRef type. *)
    val num_affine_maps : Type.t -> int

    (** Returns the pos-th affine map of the given MemRef type. *)
    val affine_map : Type.t -> int -> AffineMap.t

    (** Returns the memory space of the given MemRef type. *)
    val memory_space : Type.t -> int

    (** Returns the memory spcae of the given Unranked MemRef type. *)
    val unranked_memory_space : Type.t -> int
  end

  module Tuple : sig
    (** Checks whether the given type is a tuple type. *)
    val is_tuple : Type.t -> bool

    (** Creates a tuple type that consists of the given list of elemental types. The type is owned by the context. *)
    val get : Context.t -> Type.t list -> Type.t

    (** Returns the number of types contained in a tuple. *)
    val num_types : Type.t -> int

    (** Returns the pos-th type in the tuple type. *)
    val nth : Type.t -> int -> Type.t
  end

  module Function : sig
    (** Checks whether the given type is a function type. *)
    val is_function : Type.t -> bool

    (** Creates a function type, mapping a list of input types to result types. *)
    val get : inputs:Type.t list -> results:Type.t list -> Context.t -> Type.t

    (* Returns the number of input types. *)
    val num_inputs : Type.t -> int

    (* Returns the number of result types. *)
    val num_results : Type.t -> int

    (* Returns the pos-th input type. *)
    val input : Type.t -> int -> Type.t

    (* Returns the pos-th result type. *)
    val result : Type.t -> int -> Type.t
  end
end

and AffineMap : sig
  open IR

  type t

  (** Gets the context that the given affine map was created with *)
  val context : t -> Context.t

  (** Checks whether an affine map is null. *)
  val is_null : t -> bool

  (** Checks if two affine maps are equal. *)
  val equal : t -> t -> bool

  (** Prints an affine map by sending chunks of the string representation and forwarding `userData to `callback`. Note that the callback may be called several times with consecutive chunks of the string. *)

  (* val print : callback:(string -> 'a -> unit) -> 'a -> t -> unit *)

  (** Prints the affine map to the standard error stream. *)
  val dump : t -> unit

  (** Creates a zero result affine map with no dimensions or symbols in the context. The affine map is owned by the context. *)
  val empty : Context.t -> t

  (** Creates a zero result affine map of the given dimensions and symbols in the context. The affine map is owned by the context. *)
  val get : Context.t -> int -> int -> t

  (** Creates a single constant result affine map in the context. The affine map is owned by the context. *)
  val constant : Context.t -> int -> t

  (** Creates an affine map with 'numDims' identity in the context. The affine map is owned by the context. *)
  val multi_dim_identity : Context.t -> int -> t

  (** Creates an identity affine map on the most minor dimensions in the context. The affine map is owned by the context. The function asserts that the number of dimensions is greater or equal to the number of results. *)
  val minor_identity : Context.t -> int -> int -> t

  (** Creates an affine map with a permutation expression and its size in the context. The permutation expression is a non-empty vector of integers. The elements of the permutation vector must be continuous from 0 and cannot be repeated (i.e. `[1,2,0]` is a valid permutation. `[2,0]` or `[1,1,2]` is an invalid invalid permutation.) The affine map is owned by the context. *)

  (* val permutation : Context.t -> int array -> t *)

  (** Checks whether the given affine map is an identity affine map. The function asserts that the number of dimensions is greater or equal to the number of results. *)
  val is_identity : t -> bool

  (** Checks whether the given affine map is a minor identity affine map. *)
  val is_minor_identity : t -> bool

  (** Checks whether the given affine map is a empty affine map. *)
  val is_empty : t -> bool

  (** Checks whether the given affine map is a single result constant affine map. *)
  val is_single_constant : t -> bool

  (** Returns the constant result of the given affine map. The function asserts
   * that the map has a single constant result. *)
  val single_constant_result : t -> int

  (** Returns the number of dimensions of the given affine map. *)
  val num_dims : t -> int

  (** Returns the number of symbols of the given affine map. *)
  val num_symbols : t -> int

  (** Returns the number of results of the given affine map. *)
  val num_results : t -> int

  (** Returns the number of inputs (dimensions + symbols) of the given affine map. *)
  val num_inputs : t -> int

  (** Checks whether the given affine map represents a subset of a symbol-less permutation map. *)
  val is_projected_permutation : t -> bool

  (** Checks whether the given affine map represents a symbol-less permutation map. *)
  val is_permutation : t -> bool

  (** Returns the affine map consisting of the `resultPos` subset. *)

  (* val sub_map : t -> int array -> t *)

  (* Returns the affine map consisting of the most major `numResults` results. Returns the null AffineMap if the `numResults` is equal to zero. Returns the `affineMap` if `numResults` is greater or equals to number of results of the given affine map. *)
  val major_sub_map : t -> int -> t

  (* Returns the affine map consisting of the most minor `numResults` results. Returns the null AffineMap if the `numResults` is equal to zero. Returns the `affineMap` if `numResults` is greater or equals to number of results of the given affine map. *)
  val minor_sub_map : t -> int -> t
end

module StandardDialect : sig
  open IR

  (** Registers the Standard dialect with the given context. This allows the dialect to be loaded dynamically if needed when parsing. *)
  val register_standard_dialect : Context.t -> unit

  (** Loads the Standard dialect into the given context. The dialect does _not_ have to be registered in advance. *)
  val load_standard_dialect : Context.t -> Dialect.t

  (** Returns the namespace of the Standard dialect, suitable for loading it. *)
  val namespace : unit -> string
end

module rec Pass : sig
  type t
end

and PassManager : sig
  open IR

  type t

  (** Create a new top-level PassManager. *)
  val create : Context.t -> t

  (** Destroy the provided PassManager. *)
  val destroy : t -> unit

  (** Checks if a PassManager is null. *)
  val is_null : t -> bool

  (** Cast a top-level PassManager to a generic OpPassManager. *)
  val to_op_pass_manager : t -> OpPassManager.t

  (** Run the provided `passManager` on the given `module`. *)
  val run : t -> Module.t -> bool

  (** Nest an OpPassManager under the top-level PassManager, the nested passmanager will only run on operations matching the provided name. The returned OpPassManager will be destroyed when the parent is destroyed. To further nest more OpPassManager under the newly returned one, see `mlirOpPassManagerNest` below. *)
  val nested_under : t -> string -> OpPassManager.t

  (** Add a pass and transfer ownership to the provided top-level mlirPassManager. If the pass is not a generic operation pass or a ModulePass, a new OpPassManager is implicitly nested under the provided PassManager. *)
  val add_owned_pass : t -> Pass.t -> unit
end

and OpPassManager : sig
  type t

  (** Nest an OpPassManager under the provided OpPassManager, the nested passmanager will only run on operations matching the provided name. The returned OpPassManager will be destroyed when the parent is destroyed. *)
  val nested_under : t -> string -> t

  (** Add a pass and transfer ownership to the provided mlirOpPassManager. If the pass is not a generic operation pass or matching the type of the provided PassManager, a new OpPassManager is implicitly nested under the provided PassManager. *)
  val add_owned_pass : t -> Pass.t -> unit

  (** Print a textual MLIR pass pipeline by sending chunks of the string representation and forwarding `userData to `callback`. Note that the callback may be called several times with consecutive chunks of the string. *)

  (* val print_pass_pipeline : t -> (string -> 'a -> unit) -> 'a -> unit *)

  (** Parse a textual MLIR pass pipeline and add it to the provided OpPassManager. *)
  val parse_pass_pipeline : t -> string -> bool
end

module Transforms : sig
  val register_passes : unit -> unit

  module AffineLoopFusion : Transforms_intf.Sig with type t := Pass.t
  module AffinePipelineDataTransfer : Transforms_intf.Sig with type t := Pass.t
  module BufferDeallocation : Transforms_intf.Sig with type t := Pass.t
  module BufferHoisting : Transforms_intf.Sig with type t := Pass.t
  module BufferLoopHoisting : Transforms_intf.Sig with type t := Pass.t
  module BufferResultsToOutParams : Transforms_intf.Sig with type t := Pass.t
  module CSE : Transforms_intf.Sig with type t := Pass.t
  module Canonicalizer : Transforms_intf.Sig with type t := Pass.t
  module CopyRemoval : Transforms_intf.Sig with type t := Pass.t
  module FinalizingBufferize : Transforms_intf.Sig with type t := Pass.t
  module Inliner : Transforms_intf.Sig with type t := Pass.t
  module LocationSnapshot : Transforms_intf.Sig with type t := Pass.t
  module LoopCoalescing : Transforms_intf.Sig with type t := Pass.t
  module LoopInvariantCodeMotion : Transforms_intf.Sig with type t := Pass.t
  module MemRefDataFlowOpt : Transforms_intf.Sig with type t := Pass.t
  module NormlizeMemRefs : Transforms_intf.Sig with type t := Pass.t
  module ParallelLoopCollapsing : Transforms_intf.Sig with type t := Pass.t
  module PrintCFG : Transforms_intf.Sig with type t := Pass.t
  module PrintOp : Transforms_intf.Sig with type t := Pass.t
  module PrintOpStats : Transforms_intf.Sig with type t := Pass.t
  module PromoteBuffersToStack : Transforms_intf.Sig with type t := Pass.t
  module SCCP : Transforms_intf.Sig with type t := Pass.t
  module StripDebugInfo : Transforms_intf.Sig with type t := Pass.t
  module SymbolDCE : Transforms_intf.Sig with type t := Pass.t
end

(** Registers all dialects known to core MLIR with the provided Context.
   This is needed before creating IR for these Dialects. *)
val register_all_dialects : IR.Context.t -> unit

(* [with_context f]  creates a context [ctx], applies [f] to it, destroys it and returns the result of applying [f] *)
val with_context : (IR.Context.t -> 'a) -> 'a
