open Ctypes

module Bindings (F : FOREIGN) = struct
  open F

  (*===----------------------------------------------------------------------===
   * Integer types.
   *===----------------------------------------------------------------------===*)

  module Integer = struct
    (* Checks whether the given type is an integer type. *)
    let is_integer = foreign "mlirTypeIsAInteger" (Typs.Type.t @-> returning bool)

    (* Creates a signless integer type of the given bitwidth in the context. The
     * type is owned by the context. *)
    let get =
      foreign "mlirIntegerTypeGet" (Typs.Context.t @-> uint @-> returning Typs.Type.t)


    (* Creates a signed integer type of the given bitwidth in the context. The type
     * is owned by the context. *)
    let signed_get =
      foreign
        "mlirIntegerTypeSignedGet"
        (Typs.Context.t @-> uint @-> returning Typs.Type.t)
  end

  (*
/** Creates an unsigned integer type of the given bitwidth in the context. The
 * type is owned by the context. */
MLIR_CAPI_EXPORTED MlirType mlirIntegerTypeUnsignedGet(MlirContext ctx,
                                                       unsigned bitwidth);

/// Returns the bitwidth of an integer type.
MLIR_CAPI_EXPORTED unsigned mlirIntegerTypeGetWidth(MlirType type);

/// Checks whether the given integer type is signless.
MLIR_CAPI_EXPORTED bool mlirIntegerTypeIsSignless(MlirType type);

/// Checks whether the given integer type is signed.
MLIR_CAPI_EXPORTED bool mlirIntegerTypeIsSigned(MlirType type);

/// Checks whether the given integer type is unsigned.
MLIR_CAPI_EXPORTED bool mlirIntegerTypeIsUnsigned(MlirType type);

//===----------------------------------------------------------------------===//
// Index type.
//===----------------------------------------------------------------------===//

/// Checks whether the given type is an index type.
MLIR_CAPI_EXPORTED bool mlirTypeIsAIndex(MlirType type);

/** Creates an index type in the given context. The type is owned by the
 * context. */
MLIR_CAPI_EXPORTED MlirType mlirIndexTypeGet(MlirContext ctx);

//===----------------------------------------------------------------------===//
// Floating-point types.
//===----------------------------------------------------------------------===//

/// Checks whether the given type is a bf16 type.
MLIR_CAPI_EXPORTED bool mlirTypeIsABF16(MlirType type);

/** Creates a bf16 type in the given context. The type is owned by the
 * context. */
MLIR_CAPI_EXPORTED MlirType mlirBF16TypeGet(MlirContext ctx);

/// Checks whether the given type is an f16 type.
MLIR_CAPI_EXPORTED bool mlirTypeIsAF16(MlirType type);

/** Creates an f16 type in the given context. The type is owned by the
 * context. */
MLIR_CAPI_EXPORTED MlirType mlirF16TypeGet(MlirContext ctx);

/// Checks whether the given type is an f32 type.
MLIR_CAPI_EXPORTED bool mlirTypeIsAF32(MlirType type);

/** Creates an f32 type in the given context. The type is owned by the
 * context. */
MLIR_CAPI_EXPORTED MlirType mlirF32TypeGet(MlirContext ctx);

/// Checks whether the given type is an f64 type.
MLIR_CAPI_EXPORTED bool mlirTypeIsAF64(MlirType type);

/** Creates a f64 type in the given context. The type is owned by the
 * context. */
MLIR_CAPI_EXPORTED MlirType mlirF64TypeGet(MlirContext ctx);

//===----------------------------------------------------------------------===//
// None type.
//===----------------------------------------------------------------------===//

/// Checks whether the given type is a None type.
MLIR_CAPI_EXPORTED bool mlirTypeIsANone(MlirType type);

/** Creates a None type in the given context. The type is owned by the
 * context. */
MLIR_CAPI_EXPORTED MlirType mlirNoneTypeGet(MlirContext ctx);

//===----------------------------------------------------------------------===//
// Complex type.
//===----------------------------------------------------------------------===//

/// Checks whether the given type is a Complex type.
MLIR_CAPI_EXPORTED bool mlirTypeIsAComplex(MlirType type);

/** Creates a complex type with the given element type in the same context as
 * the element type. The type is owned by the context. */
MLIR_CAPI_EXPORTED MlirType mlirComplexTypeGet(MlirType elementType);

/// Returns the element type of the given complex type.
MLIR_CAPI_EXPORTED MlirType mlirComplexTypeGetElementType(MlirType type);

//===----------------------------------------------------------------------===//
// Shaped type.
//===----------------------------------------------------------------------===//

/// Checks whether the given type is a Shaped type.
MLIR_CAPI_EXPORTED bool mlirTypeIsAShaped(MlirType type);

/// Returns the element type of the shaped type.
MLIR_CAPI_EXPORTED MlirType mlirShapedTypeGetElementType(MlirType type);

/// Checks whether the given shaped type is ranked.
MLIR_CAPI_EXPORTED bool mlirShapedTypeHasRank(MlirType type);

/// Returns the rank of the given ranked shaped type.
MLIR_CAPI_EXPORTED int64_t mlirShapedTypeGetRank(MlirType type);

/// Checks whether the given shaped type has a static shape.
MLIR_CAPI_EXPORTED bool mlirShapedTypeHasStaticShape(MlirType type);

/// Checks wither the dim-th dimension of the given shaped type is dynamic.
MLIR_CAPI_EXPORTED bool mlirShapedTypeIsDynamicDim(MlirType type, intptr_t dim);

/// Returns the dim-th dimension of the given ranked shaped type.
MLIR_CAPI_EXPORTED int64_t mlirShapedTypeGetDimSize(MlirType type,
                                                    intptr_t dim);

/** Checks whether the given value is used as a placeholder for dynamic sizes
 * in shaped types. */
MLIR_CAPI_EXPORTED bool mlirShapedTypeIsDynamicSize(int64_t size);

/** Checks whether the given value is used as a placeholder for dynamic strides
 * and offsets in shaped types. */
MLIR_CAPI_EXPORTED bool mlirShapedTypeIsDynamicStrideOrOffset(int64_t val);

//===----------------------------------------------------------------------===//
// Vector type.
//===----------------------------------------------------------------------===//

/// Checks whether the given type is a Vector type.
MLIR_CAPI_EXPORTED bool mlirTypeIsAVector(MlirType type);

/** Creates a vector type of the shape identified by its rank and dimensions,
 * with the given element type in the same context as the element type. The type
 * is owned by the context. */
MLIR_CAPI_EXPORTED MlirType mlirVectorTypeGet(intptr_t rank,
                                              const int64_t *shape,
                                              MlirType elementType);

/** Same as "mlirVectorTypeGet" but returns a nullptr wrapping MlirType on
 * illegal arguments, emitting appropriate diagnostics. */
MLIR_CAPI_EXPORTED MlirType mlirVectorTypeGetChecked(intptr_t rank,
                                                     const int64_t *shape,
                                                     MlirType elementType,
                                                     MlirLocation loc);

//===----------------------------------------------------------------------===//
// Ranked / Unranked Tensor type.
//===----------------------------------------------------------------------===//

/// Checks whether the given type is a Tensor type.
MLIR_CAPI_EXPORTED bool mlirTypeIsATensor(MlirType type);

/// Checks whether the given type is a ranked tensor type.
MLIR_CAPI_EXPORTED bool mlirTypeIsARankedTensor(MlirType type);

/// Checks whether the given type is an unranked tensor type.
MLIR_CAPI_EXPORTED bool mlirTypeIsAUnrankedTensor(MlirType type);

/** Creates a tensor type of a fixed rank with the given shape and element type
 * in the same context as the element type. The type is owned by the context. */
MLIR_CAPI_EXPORTED MlirType mlirRankedTensorTypeGet(intptr_t rank,
                                                    const int64_t *shape,
                                                    MlirType elementType);

/** Same as "mlirRankedTensorTypeGet" but returns a nullptr wrapping MlirType on
 * illegal arguments, emitting appropriate diagnostics. */
MLIR_CAPI_EXPORTED MlirType mlirRankedTensorTypeGetChecked(intptr_t rank,
                                                           const int64_t *shape,
                                                           MlirType elementType,
                                                           MlirLocation loc);

/** Creates an unranked tensor type with the given element type in the same
 * context as the element type. The type is owned by the context. */
MLIR_CAPI_EXPORTED MlirType mlirUnrankedTensorTypeGet(MlirType elementType);

/** Same as "mlirUnrankedTensorTypeGet" but returns a nullptr wrapping MlirType
 * on illegal arguments, emitting appropriate diagnostics. */
MLIR_CAPI_EXPORTED MlirType
mlirUnrankedTensorTypeGetChecked(MlirType elementType, MlirLocation loc);

   *)
  (*===----------------------------------------------------------------------===
   * Ranked / Unranked MemRef type.
   *===----------------------------------------------------------------------===*)

  module MemRef = struct
    (* Checks whether the given type is a MemRef type. *)
    let is_memref = foreign "mlirTypeIsAMemRef" (Typs.Type.t @-> returning bool)

    (* Checks whether the given type is an UnrankedMemRef type. *)
    let is_unranked_memref =
      foreign "mlirTypeIsAUnrankedMemRef" (Typs.Type.t @-> returning bool)


    (* Creates a MemRef type with the given rank and shape, a potentially empty
     * list of affine layout maps, the given memory space and element type, in the
     * same context as element type. The type is owned by the context. *)
    let get =
      foreign
        "mlirMemRefTypeGet"
        (Typs.Type.t
        @-> intptr_t
        @-> ptr int64_t
        @-> intptr_t
        @-> ptr Typs.AffineMap.t
        @-> uint
        @-> returning Typs.Type.t)


    (* Creates a MemRef type with the given rank, shape, memory space and element
     * type in the same context as the element type. The type has no affine maps,
     * i.e. represents a default row-major contiguous memref. The type is owned by
     * the context. *)
    let contiguous_get =
      foreign
        "mlirMemRefTypeContiguousGet"
        (Typs.Type.t @-> intptr_t @-> ptr int64_t @-> uint @-> returning Typs.Type.t)


    (* Same as "mlirMemRefTypeContiguousGet" but returns a nullptr wrapping
     * MlirType on illegal arguments, emitting appropriate diagnostics. *)
    let contiguous_get_checked =
      foreign
        "mlirMemRefTypeContiguousGetChecked"
        (Typs.Type.t
        @-> intptr_t
        @-> ptr int64_t
        @-> uint
        @-> Typs.Location.t
        @-> returning Typs.Type.t)


    (* Creates an Unranked MemRef type with the given element type and in the given
     * memory space. The type is owned by the context of element type. *)
    let unranked_get =
      foreign "mlirUnrankedMemRefTypeGet" (Typs.Type.t @-> uint @-> returning Typs.Type.t)


    (* Same as "mlirUnrankedMemRefTypeGet" but returns a nullptr wrapping
     * MlirType on illegal arguments, emitting appropriate diagnostics. *)
    let unranked_get_checked =
      foreign
        "mlirUnrankedMemRefTypeGetChecked"
        (Typs.Type.t @-> uint @-> Typs.Location.t @-> returning Typs.Type.t)


    (* Returns the number of affine layout maps in the given MemRef type. *)
    let num_affine_maps =
      foreign "mlirMemRefTypeGetNumAffineMaps" (Typs.Type.t @-> returning intptr_t)


    (* Returns the pos-th affine map of the given MemRef type. *)
    let affine_map =
      foreign
        "mlirMemRefTypeGetAffineMap"
        (Typs.Type.t @-> intptr_t @-> returning Typs.AffineMap.t)


    (* Returns the memory space of the given MemRef type. *)
    let memory_space =
      foreign "mlirMemRefTypeGetMemorySpace" (Typs.Type.t @-> returning uint)


    (* Returns the memory spcae of the given Unranked MemRef type. *)
    let unranked_memory_space =
      foreign "mlirUnrankedMemRefGetMemorySpace" (Typs.Type.t @-> returning uint)
  end

  (*===----------------------------------------------------------------------===
   * Tuple type.
   *===----------------------------------------------------------------------===*)
  module Tuple = struct
    (* Checks whether the given type is a tuple type. *)
    let is_tuple = foreign "mlirTypeIsATuple" (Typs.Type.t @-> returning bool)

    (* Creates a tuple type that consists of the given list of elemental types. The
     * type is owned by the context. *)
    let get =
      foreign
        "mlirTupleTypeGet"
        (Typs.Context.t @-> intptr_t @-> ptr Typs.Type.t @-> returning Typs.Type.t)


    (* Returns the number of types contained in a tuple. *)
    let num_types = foreign "mlirTupleTypeGetNumTypes" (Typs.Type.t @-> returning intptr_t)

    (* Returns the pos-th type in the tuple type. *)
    let get_type =
      foreign "mlirTupleTypeGetType" (Typs.Type.t @-> intptr_t @-> returning Typs.Type.t)
  end

  (*===----------------------------------------------------------------------===
   * Function type.
   *===----------------------------------------------------------------------===*)

  module Function = struct
    (* Checks whether the given type is a function type. *)
    let is_function = foreign "mlirTypeIsAFunction" (Typs.Type.t @-> returning bool)

    (* Creates a function type, mapping a list of input types to result types. *)
    let type_get =
      foreign
        "mlirFunctionTypeGet"
        (Typs.Context.t
        @-> intptr_t
        @-> ptr Typs.Type.t
        @-> intptr_t
        @-> ptr Typs.Type.t
        @-> returning Typs.Type.t)


    (* Returns the number of input types. *)
    let num_inputs =
      foreign "mlirFunctionTypeGetNumInputs" (Typs.Type.t @-> returning intptr_t)


    (* Returns the number of result types. *)
    let num_results =
      foreign "mlirFunctionTypeGetNumResults" (Typs.Type.t @-> returning intptr_t)


    (* Returns the pos-th input type. *)
    let input =
      foreign
        "mlirFunctionTypeGetInput"
        (Typs.Type.t @-> intptr_t @-> returning Typs.Type.t)


    (* Returns the pos-th result type. *)
    let result =
      foreign
        "mlirFunctionTypeGetResult"
        (Typs.Type.t @-> intptr_t @-> returning Typs.Type.t)
  end
end
