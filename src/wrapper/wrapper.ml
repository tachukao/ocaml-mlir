open Ctypes
module Typs = Stubs.Typs
module Bindings = Stubs.Bindings (Mlir_generated)

type 'a structured = ('a, [ `Struct ]) Ctypes_static.structured

module Context = struct
  type t = Typs.Context.t structured

  include Bindings.Context

  let num_registered_dialects ctx = num_registered_dialects ctx |> Signed.Long.to_int
  let num_loaded_dialects ctx = num_loaded_dialects ctx |> Signed.Long.to_int
end

module Type = struct
  type t = Typs.Type.t structured

  include Bindings.Type

  let parse ctx s =
    let s = Bindings.StringRef.of_string s in
    parse ctx s
end

module Location = struct
  type t = Typs.Location.t structured

  include Bindings.Location

  let unknown = unknown
end

module NamedAttribute = struct
  type t = Typs.NamedAttribute.t structured
end

module Attribute = struct
  type t = Typs.Attribute.t structured

  include Bindings.Attribute

  let parse ctx s =
    let s = Bindings.StringRef.of_string s in
    parse ctx s


  let name s attr =
    let s = Bindings.StringRef.of_string s in
    name s attr
end

module OperationState = struct
  type t = Typs.OperationState.t structured

  include Bindings.OperationState

  let get s loc =
    let s = Bindings.StringRef.of_string s in
    get s loc


  let add_results opstate results =
    let opstate = addr opstate in
    let n = List.length results |> Intptr.of_int in
    let results = CArray.(start (of_list Typs.Type.t results)) in
    add_results opstate n results


  let add_named_attributes opstate attrs =
    let opstate = addr opstate in
    let n = List.length attrs |> Intptr.of_int in
    let attrs = CArray.(start (of_list Typs.NamedAttribute.t attrs)) in
    add_attributes opstate n attrs


  let add_owned_regions opstate regions =
    let opstate = addr opstate in
    let n = List.length regions |> Intptr.of_int in
    let regions = CArray.(start (of_list Typs.Region.t regions)) in
    add_owned_regions opstate n regions


  let add_operands opstate operands =
    let opstate = addr opstate in
    let n = List.length operands |> Intptr.of_int in
    let operands = CArray.(start (of_list Typs.Value.t operands)) in
    add_operands opstate n operands
end

module Operation = struct
  type t = Typs.Operation.t structured

  include Bindings.Operation

  let create opstate =
    let opstate = addr opstate in
    Bindings.Operation.create opstate


  let region x pos = Bindings.Operation.region x Intptr.(of_int pos)
  let result x pos = Bindings.Operation.result x Intptr.(of_int pos)
end

module Value = struct
  type t = Typs.Value.t structured
end

module Block = struct
  type t = Typs.Block.t structured

  include Bindings.Block

  let create typs =
    let size = List.length typs |> Intptr.of_int in
    let typs = CArray.(start (of_list Typs.Type.t typs)) in
    Bindings.Block.create size typs


  let argument x pos =
    let pos = Intptr.of_int pos in
    Bindings.Block.argument x pos


  let insert_owned_operation blk pos f =
    let pos = Intptr.of_int pos in
    Bindings.Block.insert_owned_operation blk pos f
end

module Module = struct
  type t = Typs.Module.t structured

  include Bindings.Module
end

module Region = struct
  include Bindings.Region

  type t = Typs.Region.t structured
end

module BuiltinTypes = struct
  module Integer = struct
    include Bindings.BuiltinTypes.Integer

    let get ctx i = get ctx Unsigned.UInt.(of_int i)
    let signed ctx i = signed ctx Unsigned.UInt.(of_int i)
    let unsigned ctx i = unsigned ctx Unsigned.UInt.(of_int i)
    let width typ = width typ |> Unsigned.UInt.to_int
  end

  module Float = Bindings.BuiltinTypes.Float
end

let register_all_dialects = Bindings.register_all_dialects

let with_context f =
  let ctx = Context.create () in
  let result = f ctx in
  Context.destroy ctx;
  result
