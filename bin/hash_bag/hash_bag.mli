module type Hashable = sig
  type t

  val hash : t -> int
end

module Make (M : Hashable) : sig
  type hash_map

  val find : hash_map -> M.t -> bool
  val add : hash_map -> M.t -> hash_map
  val remove : hash_map -> M.t -> hash_map
  val size : hash_map -> int
  val count : hash_map -> M.t -> int
  val join : hash_map -> hash_map -> hash_map
  val empty : unit -> hash_map
end
