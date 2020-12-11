open Ctypes

module Bindings (F : FOREIGN) = struct
  open F

  (* Gets the context that owns the affine expression. *)
  let context =
    foreign "mlirAffineExprGetContext" (Typs.AffineExpr.t @-> returning Typs.Context.t)


  (* Prints an affine expression by sending chunks of the string representation
   * and forwarding `userData to `callback`. Note that the callback may be called
   * several times with consecutive chunks of the string. *)
  let print =
    foreign
      "mlirAffineExprPrint"
      (Typs.AffineExpr.t @-> Typs.string_callback @-> ptr void @-> returning void)


  (* Prints the affine expression to the standard error stream. *)
  let dump = foreign "mlirAffineExprDump" (Typs.AffineExpr.t @-> returning void)

  (* Checks whether the given affine expression is made out of only symbols and
   * constants. *)
  let is_symbolic_or_constant =
    foreign "mlirAffineExprIsSymbolicOrConstant" (Typs.AffineExpr.t @-> returning bool)


  (* Checks whether the given affine expression is a pure affine expression, i.e.
   * mul, floordiv, ceildic, and mod is only allowed w.r.t constants. *)
  let is_pure_affine =
    foreign "mlirAffineExprIsPureAffine" (Typs.AffineExpr.t @-> returning bool)


  (* Returns the greatest known integral divisor of this affine expression. The
   * result is always positive. *)
  let largest_known_divisor =
    foreign
      "mlirAffineExprGetLargestKnownDivisor"
      (Typs.AffineExpr.t @-> returning int64_t)


  (* Checks whether the given affine expression is a multiple of 'factor'. *)
  let is_multiple_of =
    foreign "mlirAffineExprIsMultipleOf" (Typs.AffineExpr.t @-> int64_t @-> returning bool)


  (* Checks whether the given affine expression involves AffineDimExpr
   * 'position'. *)
  let is_function_of_dim =
    foreign
      "mlirAffineExprIsFunctionOfDim"
      (Typs.AffineExpr.t @-> intptr_t @-> returning bool)


  (*===----------------------------------------------------------------------===
   * Affine Dimension Expression.
   *===----------------------------------------------------------------------===*)

  module Dimension = struct
    (* Creates an affine dimension expression with 'position' in the context. *)
    let get =
      foreign
        "mlirAffineDimExprGet"
        (Typs.Context.t @-> intptr_t @-> returning Typs.AffineExpr.t)


    (* Returns the position of the given affine dimension expression. *)
    let position =
      foreign "mlirAffineDimExprGetPosition" (Typs.AffineExpr.t @-> returning intptr_t)
  end

  (*===----------------------------------------------------------------------===
   * Affine Symbol Expression.
   *===----------------------------------------------------------------------===*)
  module Symbol = struct
    (* Creates an affine symbol expression with 'position' in the context. *)
    let get =
      foreign
        "mlirAffineSymbolExprGet"
        (Typs.Context.t @-> intptr_t @-> returning Typs.AffineExpr.t)


    (* Returns the position of the given affine symbol expression. *)
    let position =
      foreign "mlirAffineSymbolExprGetPosition" (Typs.AffineExpr.t @-> returning intptr_t)
  end

  (*===----------------------------------------------------------------------===
   * Affine Constant Expression.
   *===----------------------------------------------------------------------===*)
  module Constant = struct
    (* Creates an affine constant expression with 'constant' in the context. *)
    let get =
      foreign
        "mlirAffineConstantExprGet"
        (Typs.Context.t @-> int64_t @-> returning Typs.AffineExpr.t)


    (* Returns the value of the given affine constant expression. *)
    let value =
      foreign "mlirAffineConstantExprGetValue" (Typs.AffineExpr.t @-> returning int64_t)
  end

  (*===----------------------------------------------------------------------===
   * Affine Add Expression.
   *===----------------------------------------------------------------------===*)

  module Add = struct
    (* Checks whether the given affine expression is an add expression. *)
    let is_add = foreign "mlirAffineExprIsAAdd" (Typs.AffineExpr.t @-> returning bool)

    (* Creates an affine add expression with 'lhs' and 'rhs'. *)
    let get =
      foreign
        "mlirAffineAddExprGet"
        (Typs.AffineExpr.t @-> Typs.AffineExpr.t @-> returning Typs.AffineExpr.t)
  end

  (*===----------------------------------------------------------------------===
   * Affine Mul Expression.
   *===----------------------------------------------------------------------===*)

  module Mul = struct
    (* Checks whether the given affine expression is an mul expression. *)
    let is_mul = foreign "mlirAffineExprIsAMul" (Typs.AffineExpr.t @-> returning bool)

    (* Creates an affine mul expression with 'lhs' and 'rhs'. *)
    let get =
      foreign
        "mlirAffineMulExprGet"
        (Typs.AffineExpr.t @-> Typs.AffineExpr.t @-> returning Typs.AffineExpr.t)
  end

  (*===----------------------------------------------------------------------===
   * Affine Mod Expression.
   *===----------------------------------------------------------------------===*)

  module Mod = struct
    (* Checks whether the given affine expression is an mod expression. *)
    let is_mod = foreign "mlirAffineExprIsAMod" (Typs.AffineExpr.t @-> returning bool)

    (* Creates an affine mod expression with 'lhs' and 'rhs'. *)
    let get =
      foreign
        "mlirAffineModExprGet"
        (Typs.AffineExpr.t @-> Typs.AffineExpr.t @-> returning Typs.AffineExpr.t)
  end

  (*===----------------------------------------------------------------------===
   * Affine FloorDiv Expression.
   *===----------------------------------------------------------------------===*)

  module FloorDiv = struct
    (* Checks whether the given affine expression is an floordiv expression. *)
    let is_floor_div =
      foreign "mlirAffineExprIsAFloorDiv" (Typs.AffineExpr.t @-> returning bool)


    (* Creates an affine floordiv expression with 'lhs' and 'rhs'. *)
    let get =
      foreign
        "mlirAffineFloorDivExprGet"
        (Typs.AffineExpr.t @-> Typs.AffineExpr.t @-> returning Typs.AffineExpr.t)
  end

  (*===----------------------------------------------------------------------===
   * Affine CeilDiv Expression.
   *===----------------------------------------------------------------------===*)

  module CeilDiv = struct
    (* Checks whether the given affine expression is an ceildiv expression. *)
    let is_ceildiv =
      foreign "mlirAffineExprIsACeilDiv" (Typs.AffineExpr.t @-> returning bool)


    (* Creates an affine ceildiv expression with 'lhs' and 'rhs'. *)
    let get =
      foreign
        "mlirAffineCeilDivExprGet"
        (Typs.AffineExpr.t @-> Typs.AffineExpr.t @-> returning Typs.AffineExpr.t)
  end

  (*===----------------------------------------------------------------------===
   * Affine Binary Operation Expression.
   *===----------------------------------------------------------------------===*)
  module BinaryOp = struct
    (* Returns the left hand side affine expression of the given affine binary
     * operation expression. *)
    let lhs =
      foreign
        "mlirAffineBinaryOpExprGetLHS"
        (Typs.AffineExpr.t @-> returning Typs.AffineExpr.t)


    (* Returns the right hand side affine expression of the given affine binary
     * operation expression. *)
    let rhs =
      foreign
        "mlirAffineBinaryOpExprGetRHS"
        (Typs.AffineExpr.t @-> returning Typs.AffineExpr.t)
  end
end
