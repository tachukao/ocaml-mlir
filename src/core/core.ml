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

module BuiltinAttributes = struct
  module AffineMap = Bindings.BuiltinAttributes.AffineMap

  module Array = struct
    include Bindings.BuiltinAttributes.Array

    let get ctx x =
      let size = List.length x |> Intptr.of_int in
      let x = CArray.(start (of_list Typs.Attribute.t x)) in
      get ctx size x


    let num_elements x = num_elements x |> Intptr.to_int
    let element x pos = element x Intptr.(of_int pos)
  end

  module Dictionary = struct
    include Bindings.BuiltinAttributes.Dictionary

    let get ctx x =
      let size = List.length x |> Intptr.of_int in
      let x = CArray.(start (of_list Typs.NamedAttribute.t x)) in
      get ctx size x


    let num_elements x = num_elements x |> Intptr.to_int
    let element x pos = element x Intptr.(of_int pos)
    let element_by_name x key = element_by_name x Bindings.StringRef.(of_string key)
  end

  module Float = Bindings.BuiltinAttributes.Float

  module Integer = struct
    include Bindings.BuiltinAttributes.Integer

    let get x i = get x Int64.(of_int i)
    let value x = value x |> Int64.to_int
  end

  module Bool = Bindings.BuiltinAttributes.Bool
  module IntegerSet = Bindings.BuiltinAttributes.IntegerSet

  module Opaque = struct
    include Bindings.BuiltinAttributes.Opaque

    let get ctx s1 i s2 typs =
      get ctx Bindings.StringRef.(of_string s1) Intptr.(of_int i) s2 typs


    let namespace x = getf (namespace x) Typs.StringRef.data
    let data x = getf (data x) Typs.StringRef.data
  end

  module String = struct
    include Bindings.BuiltinAttributes.String

    let get ctx s = get ctx Bindings.StringRef.(of_string s)
    let typed_get typ s = typed_get typ Bindings.StringRef.(of_string s)
    let value s = getf (value s) Typs.StringRef.data
  end

  module SymbolRef = struct
    include Bindings.BuiltinAttributes.SymbolRef

    let get ctx s attrs =
      let size = List.length attrs |> Intptr.of_int in
      let attrs = CArray.(start (of_list Typs.Attribute.t attrs)) in
      get ctx Bindings.StringRef.(of_string s) size attrs


    let root_ref attr = getf (root_ref attr) Typs.StringRef.data
    let leaf_ref attr = getf (leaf_ref attr) Typs.StringRef.data
    let num_nested_refs x = num_nested_refs x |> Intptr.to_int
    let nested_ref x i = nested_ref x Intptr.(of_int i)
  end

  module FlatSymbolRef = struct
    include Bindings.BuiltinAttributes.FlatSymbolRef

    let get ctx s = get ctx Bindings.StringRef.(of_string s)
    let value s = getf (value s) Typs.StringRef.data
  end

  module Type = Bindings.BuiltinAttributes.Type
  module Unit = Bindings.BuiltinAttributes.Unit

  module Elements = struct
    include Bindings.BuiltinAttributes.Elements

    let get attr xs =
      let size = List.length xs |> Intptr.of_int in
      let xs =
        let xs = List.map Unsigned.UInt64.of_int xs in
        CArray.(start (of_list uint64_t xs))
      in
      get attr size xs


    let is_valid_index attr xs =
      let size = List.length xs |> Intptr.of_int in
      let xs =
        let xs = List.map Unsigned.UInt64.of_int xs in
        CArray.(start (of_list uint64_t xs))
      in
      is_valid_index attr size xs


    let num_elements attrs = num_elements attrs |> Intptr.to_int

    module Sparse = Bindings.BuiltinAttributes.Elements.Sparse
    module Opaque = Bindings.BuiltinAttributes.Elements.Opaque

    module Dense = struct
      include Bindings.BuiltinAttributes.Elements.Dense

      let get attr xs =
        let size = List.length xs |> Intptr.of_int in
        let xs = CArray.(start (of_list Typs.Attribute.t xs)) in
        get attr size xs


      let uint32_splat_get typ i = uint32_splat_get typ Unsigned.UInt32.(of_int i)
      let int32_splat_get typ i = int32_splat_get typ Int32.(of_int i)
      let uint64_splat_get typ i = uint64_splat_get typ Unsigned.UInt64.(of_int i)
      let int64_splat_get typ i = int64_splat_get typ Int64.(of_int i)

      let _wrapper_get f t g =
        let dummy typ xs =
          let xs = List.map g xs in
          let size = List.length xs |> Intptr.of_int in
          let xs = CArray.(start (of_list t xs)) in
          f typ size xs
        in
        dummy


      let bool_get = _wrapper_get bool_get int (fun x -> x)
      let uint32_get = _wrapper_get uint32_get uint32_t Unsigned.UInt32.of_int
      let int32_get = _wrapper_get int32_get int32_t Int32.of_int
      let uint64_get = _wrapper_get uint64_get uint64_t Unsigned.UInt64.of_int
      let int64_get = _wrapper_get int64_get int64_t Int64.of_int
      let float_get = _wrapper_get float_get float (fun x -> x)
      let double_get = _wrapper_get double_get double (fun x -> x)

      let string_get =
        _wrapper_get string_get Typs.StringRef.t Bindings.StringRef.of_string


      let int32_splat_value x = int32_splat_value x |> Int32.to_int
      let uint32_splat_value x = uint32_splat_value x |> Unsigned.UInt32.to_int
      let int64_splat_value x = int64_splat_value x |> Int64.to_int
      let uint64_splat_value x = uint64_splat_value x |> Unsigned.UInt64.to_int
      let bool_value x i = bool_value x Intptr.(of_int i)
      let int32_value x i = int32_value x Intptr.(of_int i) |> Int32.to_int
      let uint32_value x i = uint32_value x Intptr.(of_int i) |> Unsigned.UInt32.to_int
      let int64_value x i = int64_value x Intptr.(of_int i) |> Int64.to_int
      let uint64_value x i = uint64_value x Intptr.(of_int i) |> Unsigned.UInt64.to_int
      let float_value x i = float_value x Intptr.(of_int i)
      let double_value x i = double_value x Intptr.(of_int i)
      let string_value x i = getf (string_value x Intptr.(of_int i)) Typs.StringRef.data
    end
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

  let print ~callback x =
    let callback s _ = callback (getf s Typs.StringRef.data) in
    print x callback null
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

  let nested_under pm s = nested_under pm Bindings.StringRef.(of_string s)

  let print_pass_pipeline ~callback x =
    let callback s _ = callback (getf s Typs.StringRef.data) in
    print_pass_pipeline x callback null


  let parse_pass_pipeline pm s =
    parse_pass_pipeline pm Bindings.StringRef.(of_string s)
    |> Bindings.LogicalResult.is_success
end

module Transforms = Bindings.Transforms

let register_all_dialects = Bindings.register_all_dialects

let with_context f =
  let ctx = IR.Context.create () in
  let result = f ctx in
  IR.Context.destroy ctx;
  result


let with_pass_manager ~f ctx =
  let pm = PassManager.create ctx in
  let result = f pm in
  PassManager.destroy pm;
  result
