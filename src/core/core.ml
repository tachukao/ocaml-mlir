open Ctypes
open Wrapper

type 'a structured = ('a, [ `Struct ]) Ctypes_static.structured

module IR = struct
  module Context = struct
    type t = Typs.Context.t structured

    include Bindings.Context

    let num_registered_dialects ctx = num_registered_dialects ctx |> Signed.Long.to_int
    let num_loaded_dialects ctx = num_loaded_dialects ctx |> Signed.Long.to_int

    let get_or_load_dialect ctx s =
      get_or_load_dialect ctx Bindings.StringRef.(of_string s)
  end

  module Dialect = struct
    type t = Typs.Dialect.t structured

    include Bindings.Dialect

    let namespace dialect = getf (namespace dialect) Typs.StringRef.data
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

    let parse ctx str = parse ctx Bindings.StringRef.(of_string str)
  end

  module Region = struct
    include Bindings.Region

    type t = Typs.Region.t structured
  end
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
  module Index = Bindings.BuiltinTypes.Index
  module None = Bindings.BuiltinTypes.None
  module Complex = Bindings.BuiltinTypes.Complex

  module Vector = struct
    include Bindings.BuiltinTypes.Vector

    let get shp typ =
      let n = Array.length shp in
      let shp =
        let shp = shp |> Array.map Int64.of_int |> Array.to_list in
        CArray.(start (of_list int64_t shp))
      in
      get Intptr.(of_int n) shp typ


    let get_checked shp typ loc =
      let n = Array.length shp in
      let shp =
        let shp = shp |> Array.map Int64.of_int |> Array.to_list in
        CArray.(start (of_list int64_t shp))
      in
      get_checked Intptr.(of_int n) shp typ loc
  end

  module Tensor = struct
    include Bindings.BuiltinTypes.Tensor

    let ranked shp typ =
      let n = Array.length shp in
      let shp =
        let shp = shp |> Array.map Int64.of_int |> Array.to_list in
        CArray.(start (of_list int64_t shp))
      in
      ranked Intptr.(of_int n) shp typ


    let ranked_checked shp typ loc =
      let n = Array.length shp in
      let shp =
        let shp = shp |> Array.map Int64.of_int |> Array.to_list in
        CArray.(start (of_list int64_t shp))
      in
      ranked_checked Intptr.(of_int n) shp typ loc
  end

  module MemRef = struct
    include Bindings.BuiltinTypes.MemRef

    let get typ shape affmaps memspace =
      let rank = Array.length shape |> Intptr.of_int in
      let shape =
        let shp = shape |> Array.map Int64.of_int |> Array.to_list in
        CArray.(start (of_list int64_t shp))
      in
      let n = List.length affmaps |> Intptr.of_int in
      let affmaps = CArray.(start (of_list Typs.AffineMap.t affmaps)) in
      get typ rank shape n affmaps Unsigned.UInt.(of_int memspace)


    let contiguous typ shape memspace =
      let rank = Array.length shape |> Intptr.of_int in
      let shape =
        let shp = shape |> Array.map Int64.of_int |> Array.to_list in
        CArray.(start (of_list int64_t shp))
      in
      contiguous typ rank shape Unsigned.UInt.(of_int memspace)


    let contiguous_checked typ shape memspace loc =
      let rank = Array.length shape |> Intptr.of_int in
      let shape =
        let shp = shape |> Array.map Int64.of_int |> Array.to_list in
        CArray.(start (of_list int64_t shp))
      in
      contiguous_checked typ rank shape Unsigned.UInt.(of_int memspace) loc


    let unranked typ i = unranked typ Unsigned.UInt.(of_int i)
    let unranked_checked typ i loc = unranked_checked typ Unsigned.UInt.(of_int i) loc
    let num_affine_maps typ = num_affine_maps typ |> Intptr.to_int
    let affine_map typ i = affine_map typ Intptr.(of_int i)
    let memory_space typ = memory_space typ |> Unsigned.UInt.to_int
    let unranked_memory_space typ = unranked_memory_space typ |> Unsigned.UInt.to_int
  end

  module Tuple = struct
    include Bindings.BuiltinTypes.Tuple

    let get ctx typs =
      let n = List.length typs |> Intptr.of_int in
      let typs = CArray.(start (of_list Typs.Type.t typs)) in
      get ctx n typs


    let num_types typ = num_types typ |> Intptr.to_int
    let nth typ pos = get_type typ Intptr.(of_int pos)
  end

  module Function = struct
    include Bindings.BuiltinTypes.Function

    let get ~inputs ~results ctx =
      let n_inputs = List.length inputs |> Intptr.of_int in
      let inputs = CArray.(start (of_list Typs.Type.t inputs)) in
      let n_results = List.length results |> Intptr.of_int in
      let results = CArray.(start (of_list Typs.Type.t results)) in
      get ctx n_inputs inputs n_results results


    let num_inputs typ = num_inputs typ |> Intptr.to_int
    let num_results typ = num_results typ |> Intptr.to_int
    let input typ i = input typ (Intptr.of_int i)
    let result typ i = result typ (Intptr.of_int i)
  end
end

module AffineMap = struct
  type t = Typs.AffineMap.t structured

  include Bindings.AffineMap

  let get ctx i j = get ctx Intptr.(of_int i) Intptr.(of_int j)
  let constant ctx i = constant ctx Int64.(of_int i)
  let multi_dim_identity ctx i = multi_dim_identity ctx Intptr.(of_int i)
  let minor_identity ctx i j = minor_identity ctx Intptr.(of_int i) (Intptr.of_int j)
  let single_constant_result ctx = single_constant_result ctx |> Int64.to_int
  let num_dims ctx = num_dims ctx |> Intptr.to_int
  let num_symbols ctx = num_symbols ctx |> Intptr.to_int
  let num_results ctx = num_results ctx |> Intptr.to_int
  let num_inputs ctx = num_inputs ctx |> Intptr.to_int
  let major_sub_map ctx i = major_sub_map ctx (Intptr.of_int i)
  let minor_sub_map ctx i = minor_sub_map ctx (Intptr.of_int i)
end

module StandardDialect = struct
  include Bindings.StandardDialect

  let namespace () = getf (namespace ()) Typs.StringRef.data
end

module Pass = struct
  type t = Typs.Pass.t structured
end

module PassManager = struct
  type t = Typs.PassManager.t structured

  include Bindings.PassManager

  let run pass m = Bindings.LogicalResult.(is_success (run pass m))
  let nested_under pm s = nested_under pm Bindings.StringRef.(of_string s)
end

module OpPassManager = struct
  type t = Typs.OpPassManager.t structured

  include Bindings.OpPassManager
end

module Transforms = Bindings.Transforms

let register_all_dialects = Bindings.register_all_dialects

let with_context f =
  let ctx = IR.Context.create () in
  let result = f ctx in
  IR.Context.destroy ctx;
  result
