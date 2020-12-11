module type Sig = sig
  type t

  val create : unit -> t
  val register : unit -> unit
end
