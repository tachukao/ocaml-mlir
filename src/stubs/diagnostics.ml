open Ctypes

module Bindings (F : FOREIGN) = struct
  open F

  (* Prints a diagnostic using the provided callback. *)
  let print =
    foreign
      "mlirDiagnosticPrint"
      (Typs.Diagnostic.t @-> Typs.string_callback @-> ptr void @-> returning void)


  (* Returns the location at which the diagnostic is reported. *)
  let location =
    foreign "mlirDiagnosticGetLocation" (Typs.Diagnostic.t @-> returning Typs.Location.t)


  (* Returns the severity of the diagnostic. *)
  let severtity =
    foreign
      "mlirDiagnosticGetSeverity"
      (Typs.Diagnostic.t @-> returning Typs.Diagnostic.severity)


  (* Returns the number of notes attached to the diagnostic. *)
  let num_notes =
    foreign "mlirDiagnosticGetNumNotes" (Typs.Diagnostic.t @-> returning intptr_t)


  (* Returns `pos`-th note attached to the diagnostic. Expects `pos` to be a
   * valid zero-based index into the list of notes. *)
  let note =
    foreign
      "mlirDiagnosticGetNote"
      (Typs.Diagnostic.t @-> intptr_t @-> returning Typs.Diagnostic.t)


  (* Attaches the diagnostic handler to the context. Handlers are invoked in the
   * reverse order of attachment until one of them processes the diagnostic
   * completely. When a handler is invoked it is passed the `userData` that was
   * provided when it was attached. If non-NULL, `deleteUserData` is called once
   * the system no longer needs to call the handler (for instance after the
   * handler is detached or the context is destroyed). Returns an identifier that
   * can be used to detach the handler.
   *)
  let attach =
    foreign
      "mlirContextAttachDiagnosticHandler"
      (Typs.Context.t
      @-> Typs.diagnostic_handler
      @-> ptr void
      @-> Ctypes.(Foreign.funptr (ptr void @-> returning void))
      @-> returning void)


  (* Detaches an attached diagnostic handler from the context given its
   * identifier. *)
  let detach =
    foreign
      "mlirContextDetachDiagnosticHandler"
      (Typs.Context.t @-> uint64_t @-> returning void)


  (* Emits an error at the given location through the diagnostics engine. Used
   * for testing purposes. *)
  let emit_error = foreign "mlirEmitError" (Typs.Location.t @-> string @-> returning void)
end
