open Ctypes

module Bindings (F : FOREIGN) = struct
  open F

  (*===----------------------------------------------------------------------===
   * Context API.
   *===----------------------------------------------------------------------===*)

  module Context = struct
    open Typs.Context

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
        (t @-> Typs.StringRef.t @-> returning Typs.Dialect.t)
  end

  (*===----------------------------------------------------------------------===
   * Dialect API.
   *===----------------------------------------------------------------------===*)

  module Dialect = struct
    open Typs.Dialect

    let context = foreign "mlirDialectGetContext" (t @-> returning Typs.Context.t)
    let is_null = foreign "mlirDialectIsNull" (t @-> returning bool)
    let equal = foreign "mlirDialectEqual" (t @-> t @-> returning bool)
    let namespace = foreign "mlirDialectGetNamespace" (t @-> returning Typs.StringRef.t)
  end

  (*===----------------------------------------------------------------------===
   * Location API.
   *===----------------------------------------------------------------------===*)

  module Location = struct
    open Typs.Location

    let file_line_col_get =
      foreign
        "mlirLocationFileLineColGet"
        (Typs.Context.t @-> Typs.StringRef.t @-> uint @-> uint @-> returning t)


    let unknown_get = foreign "mlirLocationUnknownGet" (Typs.Context.t @-> returning t)
    let context = foreign "mlirLocationGetContext" (t @-> returning Typs.Context.t)
    let is_null = foreign "mlirLocationIsNull" (t @-> returning bool)
    let equal = foreign "mlirLocationEqual" (t @-> t @-> returning bool)

    let print =
      foreign
        "mlirLocationPrint"
        (t @-> Typs.string_callback @-> ptr void @-> returning void)
  end

  (*===----------------------------------------------------------------------===
   * Module API.
   *===----------------------------------------------------------------------===*)

  module Module = struct
    let empty =
      foreign "mlirModuleCreateEmpty" (Typs.Location.t @-> returning Typs.Module.t)


    let parse =
      foreign
        "mlirModuleCreateParse"
        (Typs.Context.t @-> Typs.StringRef.t @-> returning Typs.Module.t)


    let context =
      foreign "mlirModuleGetContext" (Typs.Module.t @-> returning Typs.Context.t)


    let body = foreign "mlirModuleGetBody" (Typs.Module.t @-> returning Typs.Block.t)
    let is_null = foreign "mlirModuleIsNull" (Typs.Module.t @-> returning bool)
    let destroy = foreign "mlirModuleDestroy" (Typs.Module.t @-> returning void)

    let operation =
      foreign "mlirModuleGetOperation" (Typs.Module.t @-> returning Typs.Operation.t)
  end

  (*===----------------------------------------------------------------------===
   * OperationState API.
   *===----------------------------------------------------------------------===*)

  module OperationState = struct
    open Typs.OperationState

    let get =
      foreign
        "mlirOperationStateGet"
        (Typs.StringRef.t @-> Typs.Location.t @-> returning t)


    let add_results =
      foreign
        "mlirOperationStateAddResults"
        (ptr t @-> intptr_t @-> ptr Typs.Type.t @-> returning void)


    let add_operands =
      foreign
        "mlirOperationStateAddOperands"
        (ptr t @-> intptr_t @-> ptr Typs.Value.t @-> returning void)


    let add_regions =
      foreign
        "mlirOperationStateAddOwnedRegions"
        (ptr t @-> intptr_t @-> ptr Typs.Region.t @-> returning void)


    let add_successors =
      foreign
        "mlirOperationStateAddSuccessors"
        (ptr t @-> intptr_t @-> ptr Typs.Block.t @-> returning void)


    let add_attributes =
      foreign
        "mlirOperationStateAddAttributes"
        (ptr t @-> intptr_t @-> ptr Typs.NamedAttribute.t @-> returning void)
  end

  (*===----------------------------------------------------------------------===
   * OpPrintFlags API.
   *===----------------------------------------------------------------------===*)

  module OpPrintingFlags = struct
    open Typs.OpPrintingFlags

    let create =
      foreign "mlirOpPrintingFlagsCreate" (void @-> returning Typs.OpPrintingFlags.t)


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

  (*===----------------------------------------------------------------------===
   * Operation API.
   *===----------------------------------------------------------------------===*)

  module Operation = struct
    let create =
      foreign
        "mlirOperationCreate"
        (ptr Typs.OperationState.t @-> returning Typs.Operation.t)


    let destroy = foreign "mlirOperationDestroy" (Typs.Operation.t @-> returning void)
    let is_null = foreign "mlirOperationIsNull" (Typs.Operation.t @-> returning bool)

    let equal =
      foreign
        "mlirOperationEqual"
        (Typs.Operation.t @-> Typs.Operation.t @-> returning bool)


    let name =
      foreign "mlirOperationGetName" (Typs.Operation.t @-> returning Typs.Identifier.t)


    let block =
      foreign "mlirOperationGetBlock" (Typs.Operation.t @-> returning Typs.Block.t)


    let parent =
      foreign
        "mlirOperationGetParentOperation"
        (Typs.Operation.t @-> returning Typs.Operation.t)


    let num_regions =
      foreign "mlirOperationGetNumRegions" (Typs.Operation.t @-> returning intptr_t)


    let region =
      foreign
        "mlirOperationGetRegion"
        (Typs.Operation.t @-> intptr_t @-> returning Typs.Region.t)


    let next_in_block =
      foreign
        "mlirOperationGetNextInBlock"
        (Typs.Operation.t @-> returning Typs.Operation.t)


    let num_operands =
      foreign "mlirOperationGetNumOperands" (Typs.Operation.t @-> returning intptr_t)


    let operand =
      foreign
        "mlirOperationGetOperand"
        (Typs.Operation.t @-> intptr_t @-> returning Typs.Value.t)


    let num_results =
      foreign "mlirOperationGetNumResults" (Typs.Operation.t @-> returning intptr_t)


    let result =
      foreign
        "mlirOperationGetResult"
        (Typs.Operation.t @-> intptr_t @-> returning Typs.Value.t)


    let num_successors =
      foreign "mlirOperationGetNumSuccessors" (Typs.Operation.t @-> returning intptr_t)


    let succesor =
      foreign
        "mlirOperationGetSuccessor"
        (Typs.Operation.t @-> intptr_t @-> returning Typs.Block.t)


    let num_attributes =
      foreign "mlirOperationGetNumAttributes" (Typs.Operation.t @-> returning intptr_t)


    let attribute =
      foreign
        "mlirOperationGetAttribute"
        (Typs.Operation.t @-> intptr_t @-> returning Typs.NamedAttribute.t)


    let get_attribute_by_name =
      foreign
        "mlirOperationGetAttributeByName"
        (Typs.Operation.t @-> Typs.StringRef.t @-> returning Typs.Attribute.t)


    let set_attribute_by_name =
      foreign
        "mlirOperationSetAttributeByName"
        (Typs.Operation.t @-> Typs.StringRef.t @-> Typs.Attribute.t @-> returning void)


    let remove_attribute_by_name =
      foreign
        "mlirOperationRemoveAttributeByName"
        (Typs.Operation.t @-> Typs.StringRef.t @-> returning bool)


    let print =
      foreign
        "mlirOperationPrint"
        (Typs.Operation.t @-> Typs.string_callback @-> ptr void @-> returning void)


    let print_with_flags =
      foreign
        "mlirOperationPrintWithFlags"
        (Typs.Operation.t
        @-> Typs.OpPrintingFlags.t
        @-> Typs.string_callback
        @-> ptr void
        @-> returning void)


    let dump = foreign "mlirOperationDump" (Typs.Operation.t @-> returning void)
  end

  module Region = struct
    let create = foreign "mlirRegionCreate" (void @-> returning Typs.Region.t)
    let destroy = foreign "mlirRegionDestroy" (Typs.Region.t @-> returning void)
    let is_null = foreign "mlirRegionIsNull" (Typs.Region.t @-> returning bool)

    let first_block =
      foreign "mlirRegionGetFirstBlock" (Typs.Region.t @-> returning Typs.Block.t)


    let append_owned_block =
      foreign
        "mlirRegionAppendOwnedBlock"
        (Typs.Region.t @-> Typs.Block.t @-> returning void)


    let insert_owned_block =
      foreign
        "mlirRegionInsertOwnedBlock"
        (Typs.Region.t @-> intptr_t @-> Typs.Block.t @-> returning void)


    let insert_owned_block_after =
      foreign
        "mlirRegionInsertOwnedBlockAfter"
        (Typs.Region.t @-> Typs.Block.t @-> Typs.Block.t @-> returning void)


    let insert_owned_block_before =
      foreign
        "mlirRegionInsertOwnedBlockBefore"
        (Typs.Region.t @-> Typs.Block.t @-> Typs.Block.t @-> returning void)
  end

  (*===----------------------------------------------------------------------===
   * Block API.
   *===----------------------------------------------------------------------===*)

  module Block = struct
    let create =
      foreign "mlirBlockCreate" (intptr_t @-> ptr Typs.Type.t @-> returning Typs.Block.t)


    let destroy = foreign "mlirBlockDestroy" (Typs.Block.t @-> returning void)
    let is_null = foreign "mlirBlockIsNull" (Typs.Block.t @-> returning bool)

    let next_in_region =
      foreign "mlirBlockGetNextInRegion" (Typs.Block.t @-> returning Typs.Block.t)


    let first_operation =
      foreign "mlirBlockGetFirstOperation" (Typs.Block.t @-> returning Typs.Operation.t)


    let terminator =
      foreign "mlirBlockGetTerminator" (Typs.Block.t @-> returning Typs.Operation.t)


    let append_owned_operation =
      foreign
        "mlirBlockAppendOwnedOperation"
        (Typs.Block.t @-> Typs.Operation.t @-> returning void)


    let insert_owned_operation =
      foreign
        "mlirBlockInsertOwnedOperation"
        (Typs.Block.t @-> intptr_t @-> Typs.Operation.t @-> returning void)


    let insert_owned_operation_after =
      foreign
        "mlirBlockInsertOwnedOperationAfter"
        (Typs.Block.t @-> Typs.Operation.t @-> Typs.Operation.t @-> returning void)


    let insert_owned_operation_before =
      foreign
        "mlirBlockInsertOwnedOperationBefore"
        (Typs.Block.t @-> Typs.Operation.t @-> Typs.Operation.t @-> returning void)


    let num_arguments =
      foreign "mlirBlockGetNumArguments" (Typs.Block.t @-> returning intptr_t)


    let argument =
      foreign "mlirBlockGetArgument" (Typs.Block.t @-> intptr_t @-> returning Typs.Value.t)


    let print =
      foreign
        "mlirBlockPrint"
        (Typs.Block.t @-> Typs.string_callback @-> ptr void @-> returning void)
  end

  (*===----------------------------------------------------------------------===
   * Value API.
   *===----------------------------------------------------------------------===*)

  module Value = struct
    (* Returns whether the value is null. *)
    let is_null = foreign "mlirValueIsNull" (Typs.Value.t @-> returning bool)

    (* Returns 1 if two values are equal, 0 otherwise. *)
    (* mlirValueEqual does not seem to be exported *)
    let equal = foreign "mlirValueEqual" (Typs.Value.t @-> Typs.Value.t @-> returning bool)

    (* Returns 1 if the value is a block argument, 0 otherwise. *)
    let is_block_argument =
      foreign "mlirValueIsABlockArgument" (Typs.Value.t @-> returning bool)


    (* Returns 1 if the value is an operation result, 0 otherwise. *)
    let is_op_result = foreign "mlirValueIsAOpResult" (Typs.Value.t @-> returning bool)

    (* Returns the block in which this value is defined as an argument. Asserts if
     * the value is not a block argument. *)
    let block_argument_get_owner =
      foreign "mlirBlockArgumentGetOwner" (Typs.Value.t @-> returning Typs.Block.t)


    (* Returns the position of the value in the argument list of its block. *)
    let block_argument_arg_num =
      foreign "mlirBlockArgumentGetArgNumber" (Typs.Value.t @-> returning intptr_t)


    (* Sets the type of the block argument to the given type. *)
    let block_argument_set_type =
      foreign "mlirBlockArgumentSetType" (Typs.Value.t @-> Typs.Type.t @-> returning void)


    (* Returns an operation that produced this value as its result. Asserts if the
     * value is not an op result. *)
    let op_result_get_owner =
      foreign "mlirOpResultGetOwner" (Typs.Value.t @-> returning Typs.Operation.t)


    (* Returns the position of the value in the list of results of the operation
   * that produced it. *)
    let op_result_get_result_num =
      foreign "mlirOpResultGetResultNumber" (Typs.Value.t @-> returning intptr_t)


    (* Returns the type of the value. *)
    let get_type = foreign "mlirValueGetType" (Typs.Value.t @-> returning Typs.Type.t)

    (* Prints the value to the standard error stream. *)
    let dump = foreign "mlirValueDump" (Typs.Value.t @-> returning void)

    (* Prints a value by sending chunks of the string representation and
   * forwarding `userData to `callback`. Note that the callback may be called
   * several times with consecutive chunks of the string. *)
    let print =
      foreign
        "mlirValuePrint"
        (Typs.Value.t @-> Typs.string_callback @-> ptr void @-> returning void)
  end

  (*===----------------------------------------------------------------------===
   * Type API.
   *===----------------------------------------------------------------------===*)

  module Type = struct
    (* Parses a type. The type is owned by the context. *)
    let parse =
      foreign
        "mlirTypeParseGet"
        (Typs.Context.t @-> Typs.StringRef.t @-> returning Typs.Type.t)


    (* Gets the context that a type was created with. *)
    let context = foreign "mlirTypeGetContext" (Typs.Type.t @-> returning Typs.Context.t)

    (* Checks whether a type is null. *)
    let is_null = foreign "mlirTypeIsNull" (Typs.Type.t @-> returning bool)

    (* Checks if two types are equal. *)
    let equal = foreign "mlirTypeEqual" (Typs.Type.t @-> Typs.Type.t @-> returning bool)

    (* Prints a location by sending chunks of the string representation and
   * forwarding `userData to `callback`. Note that the callback may be called
   * several times with consecutive chunks of the string. *)
    let print =
      foreign
        "mlirTypePrint"
        (Typs.Type.t @-> Typs.string_callback @-> ptr void @-> returning void)


    (* Prints the type to the standard error stream. *)
    let dump = foreign "mlirTypeDump" (Typs.Type.t @-> returning void)
  end

  (*===----------------------------------------------------------------------===
   * Attribute API.
   *===----------------------------------------------------------------------===*)

  module Attribute = struct
    (* Parses an attribute. The attribute is owned by the context. *)
    let parse_get =
      foreign
        "mlirAttributeParseGet"
        (Typs.Context.t @-> Typs.StringRef.t @-> returning Typs.Attribute.t)


    (* Gets the context that an attribute was created with. *)
    let context =
      foreign "mlirAttributeGetContext" (Typs.Attribute.t @-> returning Typs.Context.t)


    (* Gets the type of this attribute. *)
    let get_type =
      foreign "mlirAttributeGetType" (Typs.Attribute.t @-> returning Typs.Type.t)


    (* Checks whether an attribute is null. *)
    let is_null = foreign "mlirAttributeIsNull" (Typs.Attribute.t @-> returning bool)

    (* Checks if two attributes are equal. *)
    let equal =
      foreign
        "mlirAttributeEqual"
        (Typs.Attribute.t @-> Typs.Attribute.t @-> returning bool)


    (* Prints an attribute by sending chunks of the string representation and
   * forwarding `userData to `callback`. Note that the callback may be called
   * several times with consecutive chunks of the string. *)
    let print =
      foreign
        "mlirAttributePrint"
        (Typs.Attribute.t @-> Typs.string_callback @-> ptr void @-> returning void)


    (* Prints the attribute to the standard error stream. *)
    let dump = foreign "mlirAttributeDump" (Typs.Attribute.t @-> returning void)

    (* Associates an attribute with the name. Takes ownership of neither. *)
    let get_named_attribute =
      foreign
        "mlirNamedAttributeGet"
        (Typs.StringRef.t @-> Typs.Attribute.t @-> returning Typs.NamedAttribute.t)
  end

  (* Identifier API *)
  module Identifier = struct
    let get =
      foreign
        "mlirIdentifierGet"
        (Typs.Context.t @-> Typs.StringRef.t @-> returning Typs.Identifier.t)


    let equal =
      foreign
        "mlirIdentifierEqual"
        (Typs.Identifier.t @-> Typs.Identifier.t @-> returning bool)


    let to_string =
      foreign "mlirIdentifierStr" (Typs.Identifier.t @-> returning Typs.StringRef.t)
  end
end
