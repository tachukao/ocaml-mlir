open Ctypes
module Typs = Typs

module Bindings (F : FOREIGN) = struct
  module AffineExpr = Affine_expr.Bindings (F)
  module AffineMap = Affine_map.Bindings (F)
  module Diagnostics = Diagnostics.Bindings (F)
  include Ir.Bindings (F)
  include Pass.Bindings (F)
  include Registration.Bindings (F)
  module StandardDialect = Standard_dialect.Bindings (F)
  module BuiltinAttributes = Builtin_attributes.Bindings (F)
  module BuiltinTypes = Builtin_types.Bindings (F)
  module Transforms = Transforms.Bindings (F)
  include Support.Bindings (F)
end
