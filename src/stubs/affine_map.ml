open Ctypes

module Bindings (F : FOREIGN) = struct
  open F

  (* Gets the context that the given affine map was created with *)
  let get_context =
    foreign "mlirAffineMapGetContext" (Typs.AffineMap.t @-> returning Typs.Context.t)


  (* Checks whether an affine map is null. *)
  let is_null = foreign "mlirAffineMapIsNull" (Typs.AffineMap.t @-> returning bool)

  (* Checks if two affine maps are equal. *)
  let equal =
    foreign "mlirAffineMapEqual" (Typs.AffineMap.t @-> Typs.AffineMap.t @-> returning bool)


  (* Prints an affine map by sending chunks of the string representation and
   * forwarding `userData to `callback`. Note that the callback may be called
   * several times with consecutive chunks of the string. *)
  let print =
    foreign
      "mlirAffineMapPrint"
      (Typs.AffineMap.t @-> Typs.string_callback @-> ptr void @-> returning void)


  (* Prints the affine map to the standard error stream. *)
  let dump = foreign "mlirAffineMapDump" (Typs.AffineMap.t @-> returning void)

  (* Creates a zero result affine map with no dimensions or symbols in the
   * context. The affine map is owned by the context. *)
  let empty =
    foreign "mlirAffineMapEmptyGet" (Typs.Context.t @-> returning Typs.AffineMap.t)


  (* Creates a zero result affine map of the given dimensions and symbols in the
   * context. The affine map is owned by the context. *)
  let get =
    foreign
      "mlirAffineMapGet"
      (Typs.Context.t @-> intptr_t @-> intptr_t @-> returning Typs.AffineMap.t)


  (* Creates a single constant result affine map in the context. The affine map
   * is owned by the context. *)
  let constant =
    foreign
      "mlirAffineMapConstantGet"
      (Typs.Context.t @-> int64_t @-> returning Typs.AffineMap.t)


  (* Creates an affine map with 'numDims' identity in the context. The affine map
   * is owned by the context. *)
  let multi_dim_identity =
    foreign
      "mlirAffineMapMultiDimIdentityGet"
      (Typs.Context.t @-> intptr_t @-> returning Typs.AffineMap.t)


  (* Creates an identity affine map on the most minor dimensions in the context.
   * The affine map is owned by the context. The function asserts that the number
   * of dimensions is greater or equal to the number of results. *)
  let minor_identity =
    foreign
      "mlirAffineMapMinorIdentityGet"
      (Typs.Context.t @-> intptr_t @-> intptr_t @-> returning Typs.AffineMap.t)


  (* Creates an affine map with a permutation expression and its size in the
   * context. The permutation expression is a non-empty vector of integers.
   * The elements of the permutation vector must be continuous from 0 and cannot
   * be repeated (i.e. `[1,2,0]` is a valid permutation. `[2,0]` or `[1,1,2]` is
   * an invalid invalid permutation.) The affine map is owned by the context. *)
  let permutation =
    foreign
      "mlirAffineMapPermutationGet"
      (Typs.Context.t @-> intptr_t @-> ptr uint @-> returning Typs.AffineMap.t)


  (* Checks whether the given affine map is an identity affine map. The function
   * asserts that the number of dimensions is greater or equal to the number of
   * results. *)
  let is_identity = foreign "mlirAffineMapIsIdentity" (Typs.AffineMap.t @-> returning bool)

  (* Checks whether the given affine map is a minor identity affine map. *)
  let is_minor_identity =
    foreign "mlirAffineMapIsMinorIdentity" (Typs.AffineMap.t @-> returning bool)


  (* Checks whether the given affine map is a minor identity affine map. *)
  let is_empty = foreign "mlirAffineMapIsEmpty" (Typs.AffineMap.t @-> returning bool)

  (* Checks whether the given affine map is a single result constant affine
   * map. *)
  let is_single_constant =
    foreign "mlirAffineMapIsSingleConstant" (Typs.AffineMap.t @-> returning bool)


  (* Returns the constant result of the given affine map. The function asserts
   * that the map has a single constant result. *)
  let single_constant_result =
    foreign "mlirAffineMapGetSingleConstantResult" (Typs.AffineMap.t @-> returning int64_t)


  (* Returns the number of dimensions of the given affine map. *)
  let num_dims =
    foreign "mlirAffineMapGetNumDims" (Typs.AffineMap.t @-> returning intptr_t)


  (* Returns the number of symbols of the given affine map. *)
  let num_symbols =
    foreign "mlirAffineMapGetNumSymbols" (Typs.AffineMap.t @-> returning intptr_t)


  (* Returns the number of results of the given affine map. *)
  let num_results =
    foreign "mlirAffineMapGetNumResults" (Typs.AffineMap.t @-> returning intptr_t)


  (* Returns the number of inputs (dimensions + symbols) of the given affine
   * map. *)
  let num_inputs =
    foreign "mlirAffineMapGetNumInputs" (Typs.AffineMap.t @-> returning intptr_t)


  (* Checks whether the given affine map represents a subset of a symbol-less
   * permutation map. *)
  let is_projected_permutation =
    foreign "mlirAffineMapIsProjectedPermutation" (Typs.AffineMap.t @-> returning bool)


  (* Checks whether the given affine map represents a symbol-less permutation
   * map. *)
  let is_permutation =
    foreign "mlirAffineMapIsPermutation" (Typs.AffineMap.t @-> returning bool)


  (* Returns the affine map consisting of the `resultPos` subset. *)
  let sub_map =
    foreign
      "mlirAffineMapGetSubMap"
      (Typs.AffineMap.t @-> intptr_t @-> ptr intptr_t @-> returning Typs.AffineMap.t)


  (* Returns the affine map consisting of the most major `numResults` results.
   * Returns the null AffineMap if the `numResults` is equal to zero.
   * Returns the `affineMap` if `numResults` is greater or equals to number of
   * results of the given affine map. *)
  let major_sub_map =
    foreign
      "mlirAffineMapGetMajorSubMap"
      (Typs.AffineMap.t @-> intptr_t @-> returning Typs.AffineMap.t)


  (* Returns the affine map consisting of the most minor `numResults` results.
   * Returns the null AffineMap if the `numResults` is equal to zero.
   * Returns the `affineMap` if `numResults` is greater or equals to number of
   * results of the given affine map. *)
  let minor_sub_map =
    foreign
      "mlirAffineMapGetMinorSubMap"
      (Typs.AffineMap.t @-> intptr_t @-> returning Typs.AffineMap.t)
end
