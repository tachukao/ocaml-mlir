module Typs = Stubs.Typs
module Bindings = Stubs.Bindings (Mlir_generated)

type 'a structured = ('a, [ `Struct ]) Ctypes_static.structured

