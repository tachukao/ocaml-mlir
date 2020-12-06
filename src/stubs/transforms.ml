open Ctypes

module Make (S : sig
  val s : string
end)
(F : FOREIGN) =
struct
  open F

  let create =
    foreign Printf.(sprintf "mlirCreateTransforms%s" S.s) (void @-> returning Typs.Pass.t)


  let register =
    foreign Printf.(sprintf "mlirRegisterTransforms%s" S.s) (void @-> returning void)
end

module Bindings (F : FOREIGN) = struct
  open F

  (* Registration for the entire group *)
  let register_passes = foreign "mlirRegisterTransformsPasses" (void @-> returning void)

  module AffineLoopFusion =
    Make
      (struct
        let s = "AffineLoopFusion"
      end)
      (F)

  module AffinePipelineDataTransfer =
    Make
      (struct
        let s = "AffinePipelineDataTransfer"
      end)
      (F)

  module BufferDeallocation =
    Make
      (struct
        let s = "BufferDeallocation"
      end)
      (F)

  module BufferHoisting =
    Make
      (struct
        let s = "BufferHoisting"
      end)
      (F)

  module BufferLoopHoisting =
    Make
      (struct
        let s = "BufferLoopHoisting"
      end)
      (F)

  module BufferResultsToOutParams =
    Make
      (struct
        let s = "BufferResultsToOutParams"
      end)
      (F)

  module CSE =
    Make
      (struct
        let s = "CSE"
      end)
      (F)

  module Canonicalizer =
    Make
      (struct
        let s = "Canonicalizer"
      end)
      (F)

  module CopyRemoval =
    Make
      (struct
        let s = "CopyRemoval"
      end)
      (F)

  module FinalizingBufferize =
    Make
      (struct
        let s = "FinalizingBufferize"
      end)
      (F)

  module Inliner =
    Make
      (struct
        let s = "Inliner"
      end)
      (F)

  module LocationSnapshot =
    Make
      (struct
        let s = "LocationSnapshot"
      end)
      (F)

  module LoopCoalescing =
    Make
      (struct
        let s = "LoopCoalescing"
      end)
      (F)

  module LoopInvariantCodeMotion =
    Make
      (struct
        let s = "LoopInvariantCodeMotion"
      end)
      (F)

  module MemRefDataFlowOpt =
    Make
      (struct
        let s = "MemRefDataFlowOpt"
      end)
      (F)

  module NormlizeMemRefs =
    Make
      (struct
        let s = "NormalizeMemRefs"
      end)
      (F)

  module ParallelLoopCollapsing =
    Make
      (struct
        let s = "ParallelLoopCollapsing"
      end)
      (F)

  module PrintCFG =
    Make
      (struct
        let s = "PrintCFG"
      end)
      (F)

  module PrintOp =
    Make
      (struct
        let s = "PrintOp"
      end)
      (F)

  module PrintOpStats =
    Make
      (struct
        let s = "PrintOpStats"
      end)
      (F)

  module PromoteBuffersToStack =
    Make
      (struct
        let s = "PromoteBuffersToStack"
      end)
      (F)

  module SCCP =
    Make
      (struct
        let s = "SCCP"
      end)
      (F)

  module StripDebugInfo =
    Make
      (struct
        let s = "StripDebugInfo"
      end)
      (F)

  module SymbolDCE =
    Make
      (struct
        let s = "SymbolDCE"
      end)
      (F)
end
