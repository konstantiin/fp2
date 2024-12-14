open Containers

module type Hashable = sig
  type t

  val hash : t -> int
end

module Make (M : Hashable) = struct
  let backets_sz = 1000

  type hash_map = { backets : M.t list CCPersistentArray.t; size : int }

  let find hash_map element =
    let idx = M.hash element mod 1000 in
    match
      List.find_opt (Stdlib.( = ) element)
        (CCPersistentArray.get hash_map.backets idx)
    with
    | None -> false
    | Some _ -> true

  let add hash_map element =
    let idx = M.hash element mod 1000 in
    let lst = CCPersistentArray.get hash_map.backets idx in
    let new_backets =
      CCPersistentArray.set hash_map.backets idx (element :: lst)
    in
    { backets = new_backets; size = hash_map.size + 1 }

  let remove hash_map element =
    let idx = M.hash element mod 1000 in
    let lst = CCPersistentArray.get hash_map.backets idx in
    let new_backets =
      CCPersistentArray.set hash_map.backets idx
        (Utils.remove_from_list lst element)
    in
    { backets = new_backets; size = hash_map.size - 1 }

  let count hash_map element =
    let idx = M.hash element mod 1000 in
    let all =
      List.find_all (Stdlib.( = ) element)
        (CCPersistentArray.get hash_map.backets idx)
    in
    List.length all

  let empty () = { backets = CCPersistentArray.make backets_sz []; size = 0 }
  let size hash_map = hash_map.size

  let join hash_map1 hash_map2 =
    let b1 = CCPersistentArray.to_list hash_map1.backets in
    let b2 = CCPersistentArray.to_list hash_map2.backets in
    let zipped = List.combine b1 b2 in
    let new_backets =
      CCPersistentArray.of_list (List.map (fun (x, y) -> x @ y) zipped)
    in
    { backets = new_backets; size = hash_map1.size + hash_map1.size }
end
