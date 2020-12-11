module IR : sig
  module rec Context : sig
    type t

    (** Creates an MLIR context and transfers its ownership to the caller. *)
    val create : unit -> t

    (** Checks if two contexts are equal. *)
    val equal : t -> t -> bool

    (** Takes an MLIR context owned by the caller and destroys it. *)
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

    (** Takes a block owned by the caller and inserts it after the (non-owned) reference block in the given region. The reference block must belong to the region. If the reference block is null, prepends the block to the region. *)
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

    (** Parses an attribute. The attribute is owned by the context. *)
    val parse : Context.t -> string -> t

    (** Gets the context that an attribute was created with. *)
    val context : t -> Context.t

    (** Gets the type of this attribute. *)
    val get_type : t -> Type.t

    (** Checks whether an attribute is null. *)
    val is_null : t -> bool

    (** Checks if two attributes are equal. *)
    val equal : t -> t -> bool

    (** Prints an attribute by sending chunks of the string representation and forwarding `userData to `callback`. Note that the callback may be called several times with consecutive chunks of the string. *)
    val print : callback:(string -> unit) -> t -> unit

    (** Prints the attribute to the standard error stream. *)
    val dump : t -> unit

    (** Associates an attribute with the name. Takes ownership of neither. *)
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

    (** Views the module as a generic operation. *)
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

    (** Same as "mlirUnrankedTensorTypeGet" but returns a nullptr wrapping MlirType on illegal arguments, emitting appropriate diagnostics. *)
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

    (** Returns the number of input types. *)
    val num_inputs : Type.t -> int

    (** Returns the number of result types. *)
    val num_results : Type.t -> int

    (** Returns the pos-th input type. *)
    val input : Type.t -> int -> Type.t

    (** Returns the pos-th result type. *)
    val result : Type.t -> int -> Type.t
  end
end

and BuiltinAttributes : sig
  open IR

  module AffineMap : sig
    (** Checks whether the given attribute is an affine map attribute. *)
    val is_affine_map : Attribute.t -> bool

    (** Creates an affine map attribute wrapping the given map. The attribute belongs to the same context as the affine map. *)
    val get : AffineMap.t -> Attribute.t

    (** Returns the affine map wrapped in the given affine map attribute. *)
    val value : Attribute.t -> AffineMap.t
  end

  module Array : sig
    (* Checks whether the given attribute is an array attribute. *)
    val is_array : Attribute.t -> bool

    (** Creates an array element containing the given list of elements in the given context. *)
    val get : Context.t -> Attribute.t list -> Attribute.t

    (** Returns the number of elements stored in the given array attribute. *)
    val num_elements : Attribute.t -> int

    (** Returns pos-th element stored in the given array attribute. *)
    val element : Attribute.t -> int -> Attribute.t
  end

  module Dictionary : sig
    (** Checks whether the given attribute is a dictionary attribute. *)
    val is_dicationary : Attribute.t -> bool

    (** Creates a dictionary attribute containing the given list of elements in the provided context. *)
    val get : Context.t -> NamedAttribute.t list -> Attribute.t

    (** Returns the number of attributes contained in a dictionary attribute. *)
    val num_elements : Attribute.t -> int

    (** Returns pos-th element of the given dictionary attribute. *)
    val element : Attribute.t -> int -> NamedAttribute.t

    (** Returns the dictionary attribute element with the given name or NULL if the given name does not exist in the dictionary. *)
    val element_by_name : Attribute.t -> string -> Attribute.t
  end

  module Float : sig
    (* TODO: add support for APFloat and APInt to LLVM IR C API, then expose the relevant functions here. *)

    (** Checks whether the given attribute is a floating point attribute. *)
    val is_float : Attribute.t -> bool

    (** Creates a floating point attribute in the given context with the given double value and double-precision FP semantics. *)
    val get : Context.t -> Type.t -> float -> Attribute.t

    (** Same as "mlirFloatAttrDoubleGet", but if the type is not valid for a construction of a FloatAttr, returns a null MlirAttribute. *)
    val get_checked : Type.t -> float -> Location.t -> Attribute.t

    (** Returns the value stored in the given floating point attribute, interpreting
     * the value as double. *)
    val value : Attribute.t -> float
  end

  module Integer : sig
    (* TODO: add support for APFloat and APInt to LLVM IR C API, then expose the relevant functions here. *)

    (** Checks whether the given attribute is an integer attribute. *)
    val is_integer : Attribute.t -> bool

    (** Creates an integer attribute of the given type with the given integer value. *)
    val get : Type.t -> int -> Attribute.t

    (** Returns the value stored in the given integer attribute, assuming the value fits into a 64-bit integer. *)
    val value : Attribute.t -> int
  end

  module Bool : sig
    (** Checks whether the given attribute is a bool attribute. *)
    val is_bool : Attribute.t -> bool

    (** Creates a bool attribute in the given context with the given value. *)
    val get : Context.t -> int -> Attribute.t

    (** Returns the value stored in the given bool attribute. *)
    val value : Attribute.t -> bool
  end

  module IntegerSet : sig
    (** Checks whether the given attribute is an integer set attribute. *)
    val is_integer_set : Attribute.t -> bool
  end

  module Opaque : sig
    (** Checks whether the given attribute is an opaque attribute. *)
    val is_opaque : Attribute.t -> bool

    (** Creates an opaque attribute in the given context associated with the dialect identified by its namespace. The attribute contains opaque byte data of the specified length (data need not be null-terminated). *)
    val get : Context.t -> string -> int -> string -> Type.t -> Attribute.t

    (** Returns the namespace of the dialect with which the given opaque attribute is associated. The namespace string is owned by the context. *)
    val namespace : Attribute.t -> string

    (** Returns the raw data as a string reference. The data remains live as long as the context in which the attribute lives. *)
    val data : Attribute.t -> string
  end

  module String : sig
    (** checks whether the given attribute is a string attribute. *)
    val is_string : Attribute.t -> bool

    (** Creates a string attribute in the given context containing the given string. *)
    val get : Context.t -> string -> Attribute.t

    (** Creates a string attribute in the given context containing the given string. Additionally, the attribute has the given type. *)

    val typed_get : Type.t -> string -> Attribute.t

    (** Returns the attribute values as a string reference. The data remains live as long as the context in which the attribute lives. *)
    val value : Attribute.t -> string
  end

  module SymbolRef : sig
    (** Checks whether the given attribute is a symbol reference attribute. *)
    val is_symbol_ref : Attribute.t -> bool

    (** Creates a symbol reference attribute in the given context referencing a symbol identified by the given string inside a list of nested references. Each of the references in the list must not be nested. *)
    val get : Context.t -> string -> Attribute.t list -> Attribute.t

    (** Returns the string reference to the root referenced symbol. The data remains live as long as the context in which the attribute lives. *)
    val root_ref : Attribute.t -> string

    (** Returns the string reference to the leaf referenced symbol. The data remains live as long as the context in which the attribute lives. *)
    val leaf_ref : Attribute.t -> string

    (** Returns the number of references nested in the given symbol reference attribute. *)
    val num_nested_refs : Attribute.t -> int

    (** Returns pos-th reference nested in the given symbol reference attribute. *)
    val nested_ref : Attribute.t -> int -> Attribute.t
  end

  module FlatSymbolRef : sig
    (** Checks whether the given attribute is a flat symbol reference attribute. *)
    val is_flat_symbol_ref : Attribute.t -> bool

    (** Creates a flat symbol reference attribute in the given context referencing a symbol identified by the given string. *)
    val get : Context.t -> string -> Attribute.t

    (** Returns the referenced symbol as a string reference. The data remains live as long as the context in which the attribute lives. *)
    val value : Attribute.t -> string
  end

  module Type : sig
    (** Checks whether the given attribute is a type attribute. *)
    val is_type : Attribute.t -> bool

    (** Creates a type attribute wrapping the given type in the same context as the type. *)
    val get : Type.t -> Attribute.t

    (** Returns the type stored in the given type attribute. *)
    val value : Attribute.t -> Type.t
  end

  module Unit : sig
    (** Checks whether the given attribute is a unit attribute. *)
    val is_unit : Attribute.t -> bool

    (** Creates a unit attribute in the given context. *)
    val get : Context.t -> Attribute.t
  end

  module Elements : sig
    (** Checks whether the given attribute is an elements attribute. *)
    val is_elements : Attribute.t -> bool

    (** Returns the element at the given rank-dimensional index. *)
    val get : Attribute.t -> int list -> Attribute.t

    (** Checks whether the given rank-dimensional index is valid in the given elements attribute. *)
    val is_valid_index : Attribute.t -> int list -> bool

    (** Gets the total number of elements in the given elements attribute. In order to iterate over the attribute, obtain its type, which must be a statically shaped type and use its sizes to build a multi-dimensional index. *)
    val num_elements : Attribute.t -> int

    module Dense : sig
      open IR

      (* TODO: decide on the interface and add support for complex elements. *)
      (* TODO: add support for APFloat and APInt to LLVM IR C API, then expose the relevant functions here. *)

      (** Checks whether the given attribute is a dense elements attribute. *)
      val is_dense : Attribute.t -> bool

      val is_dense_int : Attribute.t -> bool
      val is_dense_fpe : Attribute.t -> bool

      (** Creates a dense elements attribute with the given Shaped type and elements in the same context as the type. *)
      val get : Type.t -> Attribute.t list -> Attribute.t

      (** Creates a dense elements attribute with the given Shaped type containing a single replicated element (splat). *)
      val splat_get : Type.t -> Attribute.t -> Attribute.t

      val bool_splat_get : Type.t -> bool -> Attribute.t
      val uint32_splat_get : Type.t -> int -> Attribute.t
      val int32_splat_get : Type.t -> int -> Attribute.t
      val uint64_splat_get : Type.t -> int -> Attribute.t
      val int64_splat_get : Type.t -> int -> Attribute.t
      val float_splat_get : Type.t -> float -> Attribute.t
      val double_splat_get : Type.t -> float -> Attribute.t

      (** Creates a dense elements attribute with the given shaped type from elements of a specific type. Expects the element type of the shaped type to match the * data element type. *)
      val bool_get : Type.t -> int list -> Attribute.t

      val uint32_get : Type.t -> int list -> Attribute.t
      val int32_get : Type.t -> int list -> Attribute.t
      val uint64_get : Type.t -> int list -> Attribute.t
      val int64_get : Type.t -> int list -> Attribute.t
      val float_get : Type.t -> float list -> Attribute.t
      val double_get : Type.t -> float list -> Attribute.t

      (** Creates a dense elements attribute with the given shaped type from string elements. *)
      val string_get : Type.t -> string list -> Attribute.t

      (** Creates a dense elements attribute that has the same data as the given dense elements attribute and a different shaped type. The new type must have the same total number of elements. *)
      val reshape_get : Attribute.t -> Type.t -> Attribute.t

      (** Checks whether the given dense elements attribute contains a single replicated value (splat). *)
      val is_splat : Attribute.t -> bool

      (** Returns the single replicated value (splat) of a specific type contained by the given dense elements attribute. *)
      val splat_value : Attribute.t -> Attribute.t

      val bool_splat_value : Attribute.t -> int
      val int32_splat_value : Attribute.t -> int
      val uint32_splat_value : Attribute.t -> int
      val int64_splat_value : Attribute.t -> int
      val uint64_splat_value : Attribute.t -> int
      val float_splat_value : Attribute.t -> float
      val double_splat_value : Attribute.t -> float

      (** Returns the pos-th value (flat contiguous indexing) of a specific type contained by the given dense elements attribute. *)
      val bool_value : Attribute.t -> int -> bool

      val int32_value : Attribute.t -> int -> int
      val uint32_value : Attribute.t -> int -> int
      val int64_value : Attribute.t -> int -> int
      val uint64_value : Attribute.t -> int -> int
      val float_value : Attribute.t -> int -> float
      val double_value : Attribute.t -> int -> float
      val string_value : Attribute.t -> int -> string

      (* Returns the raw data of the given dense elements attribute. *)
      (* val raw_data : Attribute.t -> unit *)
    end

    module Opaque : sig
      (* TODO: expose Dialect to the bindings and implement accessors here. *)

      (** Checks whether the given attribute is an opaque elements attribute. *)
      val is_opaque : Attribute.t -> bool
    end

    module Sparse : sig
      open IR

      (** Checks whether the given attribute is a sparse elements attribute. *)
      val is_sparse : Attribute.t -> bool

      (** Creates a sparse elements attribute of the given shape from a list of indices and a list of associated values. Both lists are expected to be dense elements attributes with the same number of elements. The list of indices is expected to contain 64-bit integers. The attribute is created in the same context as the type. *)
      val create : Type.t -> Attribute.t -> Attribute.t -> Attribute.t

      (** Returns the dense elements attribute containing 64-bit integer indices of non-null elements in the given sparse elements attribute. *)
      val indices : Attribute.t -> Attribute.t

      (** Returns the dense elements attribute containing the non-null elements in the given sparse elements attribute. *)
      val values : Attribute.t -> Attribute.t
    end
  end
end

and AffineExpr : sig
  open IR

  type t

  (** Gets the context that owns the affine expression. *)
  val context : t -> Context.t

  (** Prints an affine expression by sending chunks of the string representation and forwarding `userData to `callback`. Note that the callback may be called several times with consecutive chunks of the string. *)
  val print : callback:(string -> unit) -> t -> unit

  (** Prints the affine expression to the standard error stream. *)
  val dump : t -> unit

  (** Checks whether the given affine expression is made out of only symbols and constants. *)
  val is_symbolic_or_constant : t -> bool

  (** Checks whether the given affine expression is a pure affine expression, i.e. mul, floordiv, ceildic, and mod is only allowed w.r.t constants. *)
  val is_pure_affine : t -> bool

  (** Returns the greatest known integral divisor of this affine expression. The result is always positive. *)
  val largest_known_divisor : t -> int

  (** Checks whether the given affine expression is a multiple of 'factor'. *)
  val is_multiple_of : t -> int -> bool

  (** Checks whether the given affine expression involves AffineDimExpr 'position'. *)
  val is_function_of_dim : t -> int -> bool

  module Dimension : sig
    (** Creates an affine dimension expression with 'position' in the context. *)
    val get : Context.t -> int -> t

    (** Returns the position of the given affine dimension expression. *)
    val position : t -> int
  end

  module Symbol : sig
    (** Creates an affine symbol expression with 'position' in the context. *)
    val get : Context.t -> int -> t

    (** Returns the position of the given affine symbol expression. *)
    val position : t -> int
  end

  module Constant : sig
    (** Creates an affine constant expression with 'constant' in the context. *)
    val get : Context.t -> int -> t

    (** Returns the value of the given affine constant expression. *)
    val value : t -> int
  end

  module Add : sig
    (** Checks whether the given affine expression is an add expression. *)
    val is_add : t -> bool

    (** Creates an affine add expression with 'lhs' and 'rhs'. *)
    val get : t -> t -> t
  end

  module Mul : sig
    (** Checks whether the given affine expression is an mul expression. *)
    val is_mul : t -> bool

    (** Creates an affine mul expression with 'lhs' and 'rhs'. *)
    val get : t -> t -> t
  end

  module Mod : sig
    (** Checks whether the given affine expression is an mod expression. *)
    val is_mod : t -> bool

    (** Creates an affine mod expression with 'lhs' and 'rhs'. *)
    val get : t -> t -> t
  end

  module FloorDiv : sig
    (** Checks whether the given affine expression is an floordiv expression. *)
    val is_floor_div : t -> bool

    (** Creates an affine floordiv expression with 'lhs' and 'rhs'. *)
    val get : t -> t -> t
  end

  module CeilDiv : sig
    (** Checks whether the given affine expression is an ceildiv expression. *)
    val is_ceildiv : t -> bool

    (** Creates an affine ceildiv expression with 'lhs' and 'rhs'. *)
    val get : t -> t -> t
  end

  module BinaryOp : sig
    (** Returns the left hand side affine expression of the given affine binary operation expression. *)
    val lhs : t -> t

    (** Returns the right hand side affine expression of the given affine binary operation expression. *)
    val rhs : t -> t
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
  val print : callback:(string -> unit) -> t -> unit

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

  (** Returns the affine map consisting of the most major `numResults` results. Returns the null AffineMap if the `numResults` is equal to zero. Returns the `affineMap` if `numResults` is greater or equals to number of results of the given affine map. *)
  val major_sub_map : t -> int -> t

  (** Returns the affine map consisting of the most minor `numResults` results. Returns the null AffineMap if the `numResults` is equal to zero. Returns the `affineMap` if `numResults` is greater or equals to number of results of the given affine map. *)
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
  val print_pass_pipeline : callback:(string -> unit) -> t -> unit

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

(** [with_context f]  creates a context [ctx], applies [f] to it, destroys it and returns the result of applying [f] *)
val with_context : (IR.Context.t -> 'a) -> 'a

(** [with_pass_manager f ctx]  creates a Pass Manager [pm] for the given context [ctx], applies [f] to it, destroys it and returns the result of applying [f] *)
val with_pass_manager : f:(PassManager.t -> 'a) -> IR.Context.t -> 'a
