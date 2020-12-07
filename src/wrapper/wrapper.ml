open Ctypes
module Typs = Stubs.Typs
module Bindings = Stubs.Bindings (Mlir_generated)

type 'a structured = ('a, [ `Struct ]) Ctypes_static.structured

module Context = struct
  type t = Typs.Context.t structured

  let create = Bindings.Context.create
  let equal = Bindings.Context.equal
  let destroy = Bindings.Context.destroy
  let is_null = Bindings.Context.is_null

  let num_registered_dialects ctx =
    Bindings.Context.num_registered_dialects ctx |> Signed.Long.to_int


  let num_loaded_dialects ctx =
    Bindings.Context.num_loaded_dialects ctx |> Signed.Long.to_int
end

module Type = struct
  type t = Typs.Type.t structured

  let parse ctx s =
    let s = Bindings.StringRef.of_string s in
    Bindings.Type.parse ctx s
end

module Location = struct
  type t = Typs.Location.t structured

  let unknown_get = Bindings.Location.unknown_get
end

module NamedAttribute = struct
  type t = Typs.NamedAttribute.t structured
end

module Attribute = struct
  type t = Typs.Attribute.t structured

  let parse ctx s =
    let s = Bindings.StringRef.of_string s in
    Bindings.Attribute.parse ctx s


  let name s attr =
    let s = Bindings.StringRef.of_string s in
    Bindings.Attribute.name s attr
end

module OperationState = struct
  type t = Typs.OperationState.t structured

  let get s loc =
    let s = Bindings.StringRef.of_string s in
    Bindings.OperationState.get s loc


  let add_results opstate results =
    let opstate = addr opstate in
    let n = List.length results |> Intptr.of_int in
    let results = CArray.(start (of_list Typs.Type.t results)) in
    Bindings.OperationState.add_results opstate n results


  let add_named_attributes opstate attrs =
    let opstate = addr opstate in
    let n = List.length attrs |> Intptr.of_int in
    let attrs = CArray.(start (of_list Typs.NamedAttribute.t attrs)) in
    Bindings.OperationState.add_attributes opstate n attrs


  let add_owned_regions opstate regions =
    let opstate = addr opstate in
    let n = List.length regions |> Intptr.of_int in
    let regions = CArray.(start (of_list Typs.Region.t regions)) in
    Bindings.OperationState.add_owned_regions opstate n regions


  let add_operands opstate operands =
    let opstate = addr opstate in
    let n = List.length operands |> Intptr.of_int in
    let operands = CArray.(start (of_list Typs.Value.t operands)) in
    Bindings.OperationState.add_operands opstate n operands
end

module Operation = struct
  type t = Typs.Operation.t structured

  let create opstate =
    let opstate = addr opstate in
    Bindings.Operation.create opstate


  let result x pos = Bindings.Operation.result x Intptr.(of_int pos)
  let dump = Bindings.Operation.dump
end

module Value = struct
  type t = Typs.Value.t structured
end

module Block = struct
  type t = Typs.Block.t structured

  let create typs =
    let size = List.length typs |> Intptr.of_int in
    let typs = CArray.(start (of_list Typs.Type.t typs)) in
    Bindings.Block.create size typs


  let destroy = Bindings.Block.destroy

  let argument x pos =
    let pos = Intptr.of_int pos in
    Bindings.Block.argument x pos


  let insert_owned_operation blk pos f =
    let pos = Intptr.of_int pos in
    Bindings.Block.insert_owned_operation blk pos f


  let append_owned_operation = Bindings.Block.append_owned_operation
end

module Module = struct
  type t = Typs.Module.t structured

  let empty = Bindings.Module.empty
  let body = Bindings.Module.body
  let operation = Bindings.Module.operation
end

module Region = struct
  type t = Typs.Region.t structured

  let create = Bindings.Region.create
  let destroy = Bindings.Region.destroy
  let append_owned_block = Bindings.Region.append_owned_block
end

let register_all_dialects = Bindings.register_all_dialects

let with_context f =
  let ctx = Context.create () in
  let result = f ctx in
  Context.destroy ctx;
  result


let with_region f =
  let reg = Region.create () in
  let result = f reg in
  Region.destroy reg;
  result


let with_block typs f =
  let blk = Block.create typs in
  let result = f blk in
  Block.destroy blk;
  result
