open Ctypes
module Typs = Typs

module Bindings (F : FOREIGN) = struct
  module AffineExpr = Affine_expr.Bindings (F)
  module AffineMap = Affine_map.Bindings (F)
  module Diagonostics = Diagnostics.Bindings (F)
  module IR = Ir.Bindings (F)
  module Pass = Pass.Bindings (F)
  module Registration = Registration.Bindings (F)
  module StandardAttributes = Standard_attributes.Bindings (F)
  module StandardDialect = Standard_dialect.Bindings (F)
  module StandardTypes = Standard_types.Bindings (F)
  module Support = Support.Bindings (F)
end
