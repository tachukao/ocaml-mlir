open Ctypes

module Bindings (F : FOREIGN) = struct
  open F

  (* Context *)
  module Context = struct
    open Typs.IR.Context

    let create = foreign "mlirContextCreate" (void @-> returning t)
    let equal = foreign "mlirContextEqual" (t @-> t @-> returning bool)
    let is_null = foreign "mlirContextIsNull" (t @-> returning bool)
    let destroy = foreign "mlirContextDestroy" (t @-> returning void)

    let set_allow_unregistered_dialects =
      foreign "mlirContextSetAllowUnregisteredDialects" (t @-> bool @-> returning void)


    let get_allow_unregistered_dialects =
      foreign "mlirContextGetAllowUnregisteredDialects" (t @-> returning bool)


    let num_registered_dialects =
      foreign "mlirContextGetNumRegisteredDialects" (t @-> returning long)


    let num_loaded_dialects =
      foreign "mlirContextGetNumLoadedDialects" (t @-> returning long)


    let get_or_load_dialect =
      foreign
        "mlirContextGetOrLoadDialect"
        (t @-> Typs.Support.StringRef.t @-> returning Typs.IR.Dialect.t)
  end

  (* Dialect *)
  module Dialect = struct
    open Typs.IR.Dialect

    let context = foreign "mlirDialectGetContext" (t @-> returning Typs.IR.Context.t)
    let is_null = foreign "mlirDialectIsNull" (t @-> returning bool)
    let equal = foreign "mlirDialectEqual" (t @-> t @-> returning bool)

    let namespace =
      foreign "mlirDialectGetNamespace" (t @-> returning Typs.Support.StringRef.t)
  end

  (* Location *)
  module Location = struct
    open Typs.IR.Location

    let file_line_col_get =
      foreign
        "mlirLocationFileLineColGet"
        (Typs.IR.Context.t @-> Typs.Support.StringRef.t @-> uint @-> uint @-> returning t)


    let unknown_get = foreign "mlirLocationUnknownGet" (Typs.IR.Context.t @-> returning t)
    let context = foreign "mlirLocationGetContext" (t @-> returning Typs.IR.Context.t)
    let is_null = foreign "mlirLocationIsNull" (t @-> returning bool)
    let equal = foreign "mlirLocationEqual" (t @-> t @-> returning bool)

    let print =
      foreign
        "mlirLocationPrint"
        (t @-> Typs.stringref_callback @-> ptr void @-> returning void)
  end

  (* Module *)
  module Module = struct
    let empty =
      foreign "mlirModuleCreateEmpty" (Typs.IR.Location.t @-> returning Typs.IR.Module.t)


    let parse =
      foreign
        "mlirModuleCreateParse"
        (Typs.IR.Context.t @-> Typs.Support.StringRef.t @-> returning Typs.IR.Module.t)


    let context =
      foreign "mlirModuleGetContext" (Typs.IR.Module.t @-> returning Typs.IR.Context.t)


    let body = foreign "mlirModuleGetBody" (Typs.IR.Module.t @-> returning Typs.IR.Block.t)
    let is_null = foreign "mlirModuleIsNull" (Typs.IR.Module.t @-> returning bool)
    let destroy = foreign "mlirModuleDestroy" (Typs.IR.Module.t @-> returning void)

    let operation =
      foreign "mlirModuleGetOperation" (Typs.IR.Module.t @-> returning Typs.IR.Operation.t)
  end

  (* OperationState *)
  module OperationState = struct
    open Typs.IR.OperationState

    let get =
      foreign
        "mlirOperationStateGet"
        (Typs.Support.StringRef.t @-> Typs.IR.Location.t @-> returning t)


    let add_results =
      foreign
        "mlirOperationStateAddResults"
        (ptr t @-> intptr_t @-> ptr Typs.IR.Type.t @-> returning void)


    let add_operands =
      foreign
        "mlirOperationStateAddOperands"
        (ptr t @-> intptr_t @-> ptr Typs.IR.Value.t @-> returning void)


    let add_regions =
      foreign
        "mlirOperationStateAddOwnedRegions"
        (ptr t @-> intptr_t @-> ptr Typs.IR.Region.t @-> returning void)


    let add_successors =
      foreign
        "mlirOperationStateAddSuccessors"
        (ptr t @-> intptr_t @-> ptr Typs.IR.Block.t @-> returning void)


    let add_attributes =
      foreign
        "mlirOperationStateAddAttributes"
        (ptr t @-> intptr_t @-> ptr Typs.IR.NamedAttribute.t @-> returning void)
  end

  (* OpPrintingFlags *)
  module OpPrintingFlags = struct
    open Typs.IR.OpPrintingFlags

    let create =
      foreign "mlirOpPrintingFlagsCreate" (void @-> returning Typs.IR.OpPrintingFlags.t)


    let destroy = foreign "mlirOpPrintingFlagsDestroy" (t @-> returning void)

    let large_element_limit =
      foreign
        "mlirOpPrintingFlagsElideLargeElementsAttrs"
        (t @-> intptr_t @-> returning void)


    let enable_debug_info =
      foreign "mlirOpPrintingFlagsEnableDebugInfo" (t @-> bool @-> returning void)


    let print_generic_op_form =
      foreign "mlirOpPrintingFlagsPrintGenericOpForm" (t @-> returning void)


    let use_local_scope = foreign "mlirOpPrintingFlagsUseLocalScope" (t @-> returning void)
  end

  (* Operation API *)
  module Operation = struct
    let create =
      foreign
        "mlirOperationCreate"
        (ptr Typs.IR.OperationState.t @-> returning Typs.IR.Operation.t)


    let destroy = foreign "mlirOperationDestroy" (Typs.IR.Operation.t @-> returning void)
    let is_null = foreign "mlirOperationIsNull" (Typs.IR.Operation.t @-> returning bool)

    let equal =
      foreign
        "mlirOperationEqual"
        (Typs.IR.Operation.t @-> Typs.IR.Operation.t @-> returning bool)


    let name =
      foreign
        "mlirOperationGetName"
        (Typs.IR.Operation.t @-> returning Typs.IR.Identifier.t)


    let block =
      foreign "mlirOperationGetBlock" (Typs.IR.Operation.t @-> returning Typs.IR.Block.t)


    let parent =
      foreign
        "mlirOperationGetParentOperation"
        (Typs.IR.Operation.t @-> returning Typs.IR.Operation.t)


    let num_regions =
      foreign "mlirOperationGetNumRegions" (Typs.IR.Operation.t @-> returning intptr_t)


    let region =
      foreign
        "mlirOperationGetRegion"
        (Typs.IR.Operation.t @-> intptr_t @-> returning Typs.IR.Region.t)


    let next_in_block =
      foreign
        "mlirOperationGetNextInBlock"
        (Typs.IR.Operation.t @-> returning Typs.IR.Operation.t)


    let num_operands =
      foreign "mlirOperationGetNumOperands" (Typs.IR.Operation.t @-> returning intptr_t)


    let operand =
      foreign
        "mlirOperationGetOperand"
        (Typs.IR.Operation.t @-> intptr_t @-> returning Typs.IR.Value.t)


    let num_results =
      foreign "mlirOperationGetNumResults" (Typs.IR.Operation.t @-> returning intptr_t)


    let result =
      foreign
        "mlirOperationGetResult"
        (Typs.IR.Operation.t @-> intptr_t @-> returning Typs.IR.Value.t)


    let num_successors =
      foreign "mlirOperationGetNumSuccessors" (Typs.IR.Operation.t @-> returning intptr_t)


    let succesor =
      foreign
        "mlirOperationGetSuccessor"
        (Typs.IR.Operation.t @-> intptr_t @-> returning Typs.IR.Block.t)


    let num_attributes =
      foreign "mlirOperationGetNumAttributes" (Typs.IR.Operation.t @-> returning intptr_t)


    let attribute =
      foreign
        "mlirOperationGetAttribute"
        (Typs.IR.Operation.t @-> intptr_t @-> returning Typs.IR.NamedAttribute.t)


    let get_attribute_by_name =
      foreign
        "mlirOperationGetAttributeByName"
        (Typs.IR.Operation.t
        @-> Typs.Support.StringRef.t
        @-> returning Typs.IR.Attribute.t)


    let set_attribute_by_name =
      foreign
        "mlirOperationSetAttributeByName"
        (Typs.IR.Operation.t
        @-> Typs.Support.StringRef.t
        @-> Typs.IR.Attribute.t
        @-> returning void)


    let remove_attribute_by_name =
      foreign
        "mlirOperationRemoveAttributeByName"
        (Typs.IR.Operation.t @-> Typs.Support.StringRef.t @-> returning bool)


    let print =
      foreign
        "mlirOperationPrint"
        (Typs.IR.Operation.t @-> Typs.stringref_callback @-> ptr void @-> returning void)


    let print_with_flags =
      foreign
        "mlirOperationPrintWithFlags"
        (Typs.IR.Operation.t
        @-> Typs.IR.OpPrintingFlags.t
        @-> Typs.stringref_callback
        @-> ptr void
        @-> returning void)


    let dump = foreign "mlirOperationDump" (Typs.IR.Operation.t @-> returning void)
  end

  module Region = struct
    let create = foreign "mlirRegionCreate" (void @-> returning Typs.IR.Region.t)
    let destroy = foreign "mlirRegionDestroy" (Typs.IR.Region.t @-> returning void)
    let is_null = foreign "mlirRegionIsNull" (Typs.IR.Region.t @-> returning bool)

    let first_block =
      foreign "mlirRegionGetFirstBlock" (Typs.IR.Region.t @-> returning Typs.IR.Block.t)


    let append_owned_block =
      foreign
        "mlirRegionAppendOwnedBlock"
        (Typs.IR.Region.t @-> Typs.IR.Block.t @-> returning void)


    let insert_owned_block =
      foreign
        "mlirRegionInsertOwnedBlock"
        (Typs.IR.Region.t @-> intptr_t @-> Typs.IR.Block.t @-> returning void)


    let insert_owned_block_after =
      foreign
        "mlirRegionInsertOwnedBlockAfter"
        (Typs.IR.Region.t @-> Typs.IR.Block.t @-> Typs.IR.Block.t @-> returning void)


    let insert_owned_block_before =
      foreign
        "mlirRegionInsertOwnedBlockBefore"
        (Typs.IR.Region.t @-> Typs.IR.Block.t @-> Typs.IR.Block.t @-> returning void)
  end

  (* Block API *)
  module Block = struct
    let create =
      foreign
        "mlirBlockCreate"
        (intptr_t @-> ptr Typs.IR.Type.t @-> returning Typs.IR.Block.t)


    let destroy = foreign "mlirBlockDestroy" (Typs.IR.Block.t @-> returning void)
    let is_null = foreign "mlirBlockIsNull" (Typs.IR.Block.t @-> returning bool)

    let next_in_region =
      foreign "mlirBlockGetNextInRegion" (Typs.IR.Block.t @-> returning Typs.IR.Block.t)


    let first_operation =
      foreign
        "mlirBlockGetFirstOperation"
        (Typs.IR.Block.t @-> returning Typs.IR.Operation.t)


    let terminator =
      foreign "mlirBlockGetTerminator" (Typs.IR.Block.t @-> returning Typs.IR.Operation.t)


    let append_owned_operation =
      foreign
        "mlirBlockAppendOwnedOperation"
        (Typs.IR.Block.t @-> Typs.IR.Operation.t @-> returning void)


    let insert_owned_operation =
      foreign
        "mlirBlockInsertOwnedOperation"
        (Typs.IR.Block.t @-> intptr_t @-> Typs.IR.Operation.t @-> returning void)


    let insert_owned_operation_after =
      foreign
        "mlirBlockInsertOwnedOperationAfter"
        (Typs.IR.Block.t
        @-> Typs.IR.Operation.t
        @-> Typs.IR.Operation.t
        @-> returning void)


    let insert_owned_operation_before =
      foreign
        "mlirBlockInsertOwnedOperationBefore"
        (Typs.IR.Block.t
        @-> Typs.IR.Operation.t
        @-> Typs.IR.Operation.t
        @-> returning void)


    let num_arguments =
      foreign "mlirBlockGetNumArguments" (Typs.IR.Block.t @-> returning intptr_t)


    let argument =
      foreign
        "mlirBlockGetArgument"
        (Typs.IR.Block.t @-> intptr_t @-> returning Typs.IR.Value.t)


    let print =
      foreign
        "mlirBlockPrint"
        (Typs.IR.Block.t @-> Typs.stringref_callback @-> ptr void @-> returning void)
  end

  (*


//===----------------------------------------------------------------------===//
// Value API.
//===----------------------------------------------------------------------===//

/// Returns whether the value is null.
static inline bool mlirValueIsNull(MlirValue value) { return !value.ptr; }

/// Returns 1 if two values are equal, 0 otherwise.
bool mlirValueEqual(MlirValue value1, MlirValue value2);

/// Returns 1 if the value is a block argument, 0 otherwise.
MLIR_CAPI_EXPORTED bool mlirValueIsABlockArgument(MlirValue value);

/// Returns 1 if the value is an operation result, 0 otherwise.
MLIR_CAPI_EXPORTED bool mlirValueIsAOpResult(MlirValue value);

/** Returns the block in which this value is defined as an argument. Asserts if
   * the value is not a block argument. */
MLIR_CAPI_EXPORTED MlirBlock mlirBlockArgumentGetOwner(MlirValue value);

/// Returns the position of the value in the argument list of its block.
MLIR_CAPI_EXPORTED intptr_t mlirBlockArgumentGetArgNumber(MlirValue value);

/// Sets the type of the block argument to the given type.
MLIR_CAPI_EXPORTED void mlirBlockArgumentSetType(MlirValue value,
                                                 MlirType type);

/** Returns an operation that produced this value as its result. Asserts if the
   * value is not an op result. */
MLIR_CAPI_EXPORTED MlirOperation mlirOpResultGetOwner(MlirValue value);

/** Returns the position of the value in the list of results of the operation
   * that produced it. */
MLIR_CAPI_EXPORTED intptr_t mlirOpResultGetResultNumber(MlirValue value);

/// Returns the type of the value.
MLIR_CAPI_EXPORTED MlirType mlirValueGetType(MlirValue value);

/// Prints the value to the standard error stream.
MLIR_CAPI_EXPORTED void mlirValueDump(MlirValue value);

/** Prints a value by sending chunks of the string representation and
   * forwarding `userData to `callback`. Note that the callback may be called
   * several times with consecutive chunks of the string. */
MLIR_CAPI_EXPORTED void
mlirValuePrint(MlirValue value, MlirStringCallback callback, void *userData);

//===----------------------------------------------------------------------===//
// Type API.
//===----------------------------------------------------------------------===//

/// Parses a type. The type is owned by the context.
MLIR_CAPI_EXPORTED MlirType mlirTypeParseGet(MlirContext context,
                                             MlirStringRef type);

/// Gets the context that a type was created with.
MLIR_CAPI_EXPORTED MlirContext mlirTypeGetContext(MlirType type);

/// Checks whether a type is null.
static inline bool mlirTypeIsNull(MlirType type) { return !type.ptr; }

/// Checks if two types are equal.
MLIR_CAPI_EXPORTED bool mlirTypeEqual(MlirType t1, MlirType t2);

/** Prints a location by sending chunks of the string representation and
   * forwarding `userData to `callback`. Note that the callback may be called
   * several times with consecutive chunks of the string. */
MLIR_CAPI_EXPORTED void
mlirTypePrint(MlirType type, MlirStringCallback callback, void *userData);

/// Prints the type to the standard error stream.
MLIR_CAPI_EXPORTED void mlirTypeDump(MlirType type);

//===----------------------------------------------------------------------===//
// Attribute API.
//===----------------------------------------------------------------------===//

/// Parses an attribute. The attribute is owned by the context.
MLIR_CAPI_EXPORTED MlirAttribute mlirAttributeParseGet(MlirContext context,
                                                       MlirStringRef attr);

/// Gets the context that an attribute was created with.
MLIR_CAPI_EXPORTED MlirContext mlirAttributeGetContext(MlirAttribute attribute);

/// Gets the type of this attribute.
MLIR_CAPI_EXPORTED MlirType mlirAttributeGetType(MlirAttribute attribute);

/// Checks whether an attribute is null.
static inline bool mlirAttributeIsNull(MlirAttribute attr) { return !attr.ptr; }

/// Checks if two attributes are equal.
MLIR_CAPI_EXPORTED bool mlirAttributeEqual(MlirAttribute a1, MlirAttribute a2);

/** Prints an attribute by sending chunks of the string representation and
   * forwarding `userData to `callback`. Note that the callback may be called
   * several times with consecutive chunks of the string. */
MLIR_CAPI_EXPORTED void mlirAttributePrint(MlirAttribute attr,
                                           MlirStringCallback callback,
                                           void *userData);

/// Prints the attribute to the standard error stream.
MLIR_CAPI_EXPORTED void mlirAttributeDump(MlirAttribute attr);

/// Associates an attribute with the name. Takes ownership of neither.
MLIR_CAPI_EXPORTED MlirNamedAttribute mlirNamedAttributeGet(MlirStringRef name,
                                                            MlirAttribute attr);

   *)

  (* Identifier API *)
  module Identifier = struct
    let get =
      foreign
        "mlirIdentifierGet"
        (Typs.IR.Context.t @-> Typs.Support.StringRef.t @-> returning Typs.IR.Identifier.t)


    let equal =
      foreign
        "mlirIdentifierEqual"
        (Typs.IR.Identifier.t @-> Typs.IR.Identifier.t @-> returning bool)


    let to_string =
      foreign
        "mlirIdentifierStr"
        (Typs.IR.Identifier.t @-> returning Typs.Support.StringRef.t)
  end
end
