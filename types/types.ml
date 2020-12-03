open Ctypes

module Bindings (S : Cstubs.Types.TYPE) = struct
  open S

  module Support = struct
    module StringRef = struct
      type t

      let t : t structure typ = structure "MlirStringRef"
      let data = field t "data" string
      let length = field t "length" size_t
      let () = seal t
    end

    module LogicalResult = struct
      type t

      let t : t structure typ = structure "MlirLogicalResult"
      let value = field t "value" int8_t
      let () = seal t
    end
  end

  module IR = struct
    module Context = struct
      type t

      let t : t structure typ = structure "MlirContext"
      let ptr = field t "ptr" (ptr void)
      let () = seal t
    end

    module Dialect = struct
      type t

      let t : t structure typ = structure "MlirDialect"
      let ptr = field t "ptr" (ptr void)
      let () = seal t
    end

    module Operation = struct
      type t

      let t : t structure typ = structure "MlirOperation"
      let ptr = field t "ptr" (ptr void)
      let () = seal t
    end

    module OpPrintingFlags = struct
      type t

      let t : t structure typ = structure "MlirOpPrintingFlags"
      let () = seal t 
    end

    module Block = struct
      type t

      let t : t structure typ = structure "MlirBlock"
      let () = seal t
    end

    module Region = struct
      type t

      let t : t structure typ = structure "MlirRegion"
    end

    module Attribute = struct
      type t

      let t : t structure typ = structure "MlirAttribute"
      let ptr = field t "ptr" (ptr void)
      let () = seal t
    end

    type identifier

    let identifier : identifier structure typ = structure "MlirIdentifier"

    module Location = struct
      type t

      let t : t structure typ = structure "MlirLocation"
      let location_ptr = field t "ptr" (ptr void)
      let () = seal t
    end

    module Module = struct
      type t

      let t : t structure typ = structure "MlirModule"
      let () = seal t
    end

    module Type = struct
      type t

      let t : t structure typ = structure "MlirType"
      let () = seal t
    end

    module Value = struct
      type t

      let t : t structure typ = structure "MlirValue"
    end

    module NamedAttribute = struct
      type t

      let t : t structure typ = structure "MlirNamedAttribute"
      let name = field t "name" Support.StringRef.t
      let attribute = field t "attribute" Attribute.t
      let () = seal t
    end

    module OperationState = struct
      type t

      let t : t structure typ = structure "MlirOperationState"
      let name = field t "name" Support.StringRef.t
      let location = field t "location" Location.t
      let nResults = field t "nResults" intptr_t
      let results = field t "results" (ptr Type.t)
      let nOperands = field t "nOperands" intptr_t
      let operands = field t "operands" (ptr Value.t)
      let nRegions = field t "nRegions" intptr_t
      let regions = field t "regions" (ptr Region.t)
      let nSuccessors = field t "nSuccessors" intptr_t
      let successors = field t "successors" (ptr Block.t)
      let nAttributes = field t "nAttributes" intptr_t
      let attributes = field t "attributes" (ptr Attribute.t)
      let () = seal t
    end
  end
end
