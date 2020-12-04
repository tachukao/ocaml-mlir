open Ctypes

module Bindings (F : FOREIGN) = struct
  open F

  (*===----------------------------------------------------------------------===
   * Context API.
   *===----------------------------------------------------------------------===*)

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

  (*===----------------------------------------------------------------------===
   * Dialect API.
   *===----------------------------------------------------------------------===*)

  module Dialect = struct
    open Typs.IR.Dialect

    let context = foreign "mlirDialectGetContext" (t @-> returning Typs.IR.Context.t)
    let is_null = foreign "mlirDialectIsNull" (t @-> returning bool)
    let equal = foreign "mlirDialectEqual" (t @-> t @-> returning bool)

    let namespace =
      foreign "mlirDialectGetNamespace" (t @-> returning Typs.Support.StringRef.t)
  end

  (*===----------------------------------------------------------------------===
   * Location API.
   *===----------------------------------------------------------------------===*)

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
        (t @-> Typs.string_callback @-> ptr void @-> returning void)
  end

  (*===----------------------------------------------------------------------===
   * Module API.
   *===----------------------------------------------------------------------===*)

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

  (*===----------------------------------------------------------------------===
   * OperationState API.
   *===----------------------------------------------------------------------===*)

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

  (*===----------------------------------------------------------------------===
   * OpPrintFlags API.
   *===----------------------------------------------------------------------===*)

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

  (*===----------------------------------------------------------------------===
   * Operation API.
   *===----------------------------------------------------------------------===*)

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
        (Typs.IR.Operation.t @-> Typs.string_callback @-> ptr void @-> returning void)


    let print_with_flags =
      foreign
        "mlirOperationPrintWithFlags"
        (Typs.IR.Operation.t
        @-> Typs.IR.OpPrintingFlags.t
        @-> Typs.string_callback
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

  (*===----------------------------------------------------------------------===
   * Block API.
   *===----------------------------------------------------------------------===*)

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
        (Typs.IR.Block.t @-> Typs.string_callback @-> ptr void @-> returning void)
  end

  (*===----------------------------------------------------------------------===
   * Value API.
   *===----------------------------------------------------------------------===*)

  module Value = struct
    (* Returns whether the value is null. *)
    let is_null = foreign "mlirValueIsNull" (Typs.IR.Value.t @-> returning bool)

    (* Returns 1 if two values are equal, 0 otherwise. *)
    (* mlirValueEqual does not seem to be exported *)
    (* let equal =
      foreign "mlirValueEqual" (Typs.IR.Value.t @-> Typs.IR.Value.t @-> returning bool) *)

    (* Returns 1 if the value is a block argument, 0 otherwise. *)
    let is_block_argument =
      foreign "mlirValueIsABlockArgument" (Typs.IR.Value.t @-> returning bool)


    (* Returns 1 if the value is an operation result, 0 otherwise. *)
    let is_op_result = foreign "mlirValueIsAOpResult" (Typs.IR.Value.t @-> returning bool)

    (* Returns the block in which this value is defined as an argument. Asserts if
     * the value is not a block argument. *)
    let block_argument_get_owner =
      foreign "mlirBlockArgumentGetOwner" (Typs.IR.Value.t @-> returning Typs.IR.Block.t)


    (* Returns the position of the value in the argument list of its block. *)
    let block_argument_arg_num =
      foreign "mlirBlockArgumentGetArgNumber" (Typs.IR.Value.t @-> returning intptr_t)


    (* Sets the type of the block argument to the given type. *)
    let block_argument_set_type =
      foreign
        "mlirBlockArgumentSetType"
        (Typs.IR.Value.t @-> Typs.IR.Type.t @-> returning void)


    (* Returns an operation that produced this value as its result. Asserts if the
     * value is not an op result. *)
    let op_result_get_owner =
      foreign "mlirOpResultGetOwner" (Typs.IR.Value.t @-> returning Typs.IR.Operation.t)


    (* Returns the position of the value in the list of results of the operation
   * that produced it. *)
    let op_result_get_result_num =
      foreign "mlirOpResultGetResultNumber" (Typs.IR.Value.t @-> returning intptr_t)


    (* Returns the type of the value. *)
    let get_type =
      foreign "mlirValueGetType" (Typs.IR.Value.t @-> returning Typs.IR.Type.t)


    (* Prints the value to the standard error stream. *)
    let dump = foreign "mlirValueDump" (Typs.IR.Value.t @-> returning void)

    (* Prints a value by sending chunks of the string representation and
   * forwarding `userData to `callback`. Note that the callback may be called
   * several times with consecutive chunks of the string. *)
    let print =
      foreign
        "mlirValuePrint"
        (Typs.IR.Value.t @-> Typs.string_callback @-> ptr void @-> returning void)
  end

  (*===----------------------------------------------------------------------===
   * Type API.
   *===----------------------------------------------------------------------===*)

  module Type = struct
    (* Parses a type. The type is owned by the context. *)
    let parse_get =
      foreign
        "mlirTypeParseGet"
        (Typs.IR.Context.t @-> Typs.Support.StringRef.t @-> returning Typs.IR.Type.t)


    (* Gets the context that a type was created with. *)
    let context =
      foreign "mlirTypeGetContext" (Typs.IR.Type.t @-> returning Typs.IR.Context.t)


    (* Checks whether a type is null. *)
    let is_null = foreign "mlirTypeIsNull" (Typs.IR.Type.t @-> returning bool)

    (* Checks if two types are equal. *)
    let equal =
      foreign "mlirTypeEqual" (Typs.IR.Type.t @-> Typs.IR.Type.t @-> returning bool)


    (* Prints a location by sending chunks of the string representation and
   * forwarding `userData to `callback`. Note that the callback may be called
   * several times with consecutive chunks of the string. *)
    let print =
      foreign
        "mlirTypePrint"
        (Typs.IR.Type.t @-> Typs.string_callback @-> ptr void @-> returning void)


    (* Prints the type to the standard error stream. *)
    let dump = foreign "mlirTypeDump" (Typs.IR.Type.t @-> returning void)
  end

  (*===----------------------------------------------------------------------===
   * Attribute API.
   *===----------------------------------------------------------------------===*)

  module Attribute = struct
    (* Parses an attribute. The attribute is owned by the context. *)
    let parse_get =
      foreign
        "mlirAttributeParseGet"
        (Typs.IR.Context.t @-> Typs.Support.StringRef.t @-> returning Typs.IR.Attribute.t)


    (* Gets the context that an attribute was created with. *)
    let context =
      foreign
        "mlirAttributeGetContext"
        (Typs.IR.Attribute.t @-> returning Typs.IR.Context.t)


    (* Gets the type of this attribute. *)
    let get_type =
      foreign "mlirAttributeGetType" (Typs.IR.Attribute.t @-> returning Typs.IR.Type.t)


    (* Checks whether an attribute is null. *)
    let is_null = foreign "mlirAttributeIsNull" (Typs.IR.Attribute.t @-> returning bool)

    (* Checks if two attributes are equal. *)
    let equal =
      foreign
        "mlirAttributeEqual"
        (Typs.IR.Attribute.t @-> Typs.IR.Attribute.t @-> returning bool)


    (* Prints an attribute by sending chunks of the string representation and
   * forwarding `userData to `callback`. Note that the callback may be called
   * several times with consecutive chunks of the string. *)
    let print =
      foreign
        "mlirAttributePrint"
        (Typs.IR.Attribute.t @-> Typs.string_callback @-> ptr void @-> returning void)


    (* Prints the attribute to the standard error stream. *)
    let dump = foreign "mlirAttributeDump" (Typs.IR.Attribute.t @-> returning void)

    (* Associates an attribute with the name. Takes ownership of neither. *)
    let get_named_attribute =
      foreign
        "mlirNamedAttributeGet"
        (Typs.Support.StringRef.t
        @-> Typs.IR.Attribute.t
        @-> returning Typs.IR.NamedAttribute.t)
  end

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
