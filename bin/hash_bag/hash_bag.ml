open Containers

let backets_sz = 1000

type 'a hash_map = {
  hash : 'a -> int;
  backets : 'a list CCPersistentArray.t;
  size : int;
}

let find hash_map element =
  let idx = hash_map.hash element mod 1000 in
  match
    List.find_opt (Stdlib.( = ) element)
      (CCPersistentArray.get hash_map.backets idx)
  with
  | None -> false
  | Some _ -> true

let add hash_map element =
  let idx = hash_map.hash element mod 1000 in
  let lst = CCPersistentArray.get hash_map.backets idx in
  let new_backets =
    CCPersistentArray.set hash_map.backets idx (element :: lst)
  in
  { hash = hash_map.hash; backets = new_backets; size = hash_map.size + 1 }

let remove hash_map element =
  let idx = hash_map.hash element mod 1000 in
  let lst = CCPersistentArray.get hash_map.backets idx in
  let new_backets =
    CCPersistentArray.set hash_map.backets idx
      (Utils.remove_from_list lst element)
  in
  { hash = hash_map.hash; backets = new_backets; size = hash_map.size - 1 }

let count hash_map element =
  let idx = hash_map.hash element mod 1000 in
  let all =
    List.find_all (Stdlib.( = ) element)
      (CCPersistentArray.get hash_map.backets idx)
  in
  List.length all

let init hf =
  { hash = hf; backets = CCPersistentArray.make backets_sz []; size = 0 }

let size hash_map = hash_map.size
