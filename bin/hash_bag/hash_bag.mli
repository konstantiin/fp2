module type Hashable = sig
  type t

  val hash : t -> int
end

module type S' = sig
  type t
  type hash_map

  val find : t -> hash_map -> bool
  val add : t -> hash_map -> hash_map
  val remove : t -> hash_map -> hash_map
  val size : hash_map -> int
  val count : t -> hash_map -> int
  val join : hash_map -> hash_map -> hash_map
  val empty : unit -> hash_map
  val of_list : t list -> hash_map
  val to_list : hash_map -> t list
  val equal : hash_map -> hash_map -> bool
end

module type S = sig
  include S'

  val map :
    (module S' with type hash_map = 'ht and type t = 'tt) ->
    hash_map ->
    (t -> 'tt) ->
    'ht
end

module Make (M : Hashable) : S with type t = M.t
