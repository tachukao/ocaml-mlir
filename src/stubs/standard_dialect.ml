open Ctypes

module Bindings (F : FOREIGN) = struct
  open F

  (* Registers the Standard dialect with the given context. This allows the
   * dialect to be loaded dynamically if needed when parsing. *)
  let register_standard_dialect =
    foreign "mlirContextRegisterStandardDialect" (Typs.Context.t @-> returning void)


  (* Loads the Standard dialect into the given context. The dialect does _not_
   * have to be registered in advance. *)
  let load_standard_dialect =
    foreign "mlirContextLoadStandardDialect" (Typs.Context.t @-> returning Typs.Dialect.t)


  (* Returns the namespace of the Standard dialect, suitable for loading it. *)
  let namespace =
    foreign "mlirStandardDialectGetNamespace" (void @-> returning Typs.StringRef.t)
end
