# Лабораторная работа №2. Лучинкин Константин
## Реализация hash-bag 

Код:
```ocaml
open Containers

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
  val filter : (t -> bool) -> hash_map -> hash_map
  val fold_left : ('acc -> t -> 'acc) -> 'acc -> hash_map -> 'acc
  val fold_right : (t -> 'acc -> 'acc) -> hash_map -> 'acc -> 'acc
end

module type S = sig
  include S'

  val map :
    (module S' with type hash_map = ' h' and type t = ' t') ->
    hash_map ->
    (t -> ' t') ->
    ' h'
end

module Make (M : Hashable) = struct
  type t = M.t
  type hash_map = t list CCPersistentArray.t * int

  let backets_sz = 1000

  let get_arr_idx el =
    let idx = M.hash el mod backets_sz in
    if idx >= 0 then idx else idx + backets_sz

  let find element = function
    | backets, _ -> (
        let idx = get_arr_idx element in
        match
          List.find_opt (Stdlib.( = ) element)
            (CCPersistentArray.get backets idx)
        with
        | None -> false
        | Some _ -> true)

  let add element = function
    | backets, sz ->
        let idx = get_arr_idx element in
        let lst = CCPersistentArray.get backets idx in
        let new_backets = CCPersistentArray.set backets idx (element :: lst) in
        (new_backets, sz + 1)

  let remove element = function
    | backets, sz ->
        let idx = get_arr_idx element in
        let lst = CCPersistentArray.get backets idx in
        let new_backets =
          CCPersistentArray.set backets idx (Utils.remove_from_list lst element)
        in
        (new_backets, sz - 1)

  let count element = function
    | backets, _ ->
        let idx = get_arr_idx element in
        let all =
          List.find_all (Stdlib.( = ) element)
            (CCPersistentArray.get backets idx)
        in
        List.length all

  let empty = fun _ -> (CCPersistentArray.make backets_sz [], 0)
  let size = function _, sz -> sz

  let join = function
    | backets1, sz1 -> (
        function
        | backets2, sz2 ->
            let b1 = CCPersistentArray.to_list backets1 in
            let b2 = CCPersistentArray.to_list backets2 in
            let zipped = List.combine b1 b2 in
            let new_backets =
              CCPersistentArray.of_list (List.map (fun (x, y) -> x @ y) zipped)
            in
            (new_backets, sz1 + sz2))

  let of_list lst =
    List.fold_left (fun curmap el -> curmap |> add el) (empty ()) lst

  let to_list = function
    | backets, _ ->
        List.flatten (CCPersistentArray.to_list backets)
        |> List.sort Stdlib.compare

  (* in this case converting to list and sorting is efficient enough for this case *)
  let equal = function
    | backets1, sz1 -> (
        function
        | backets2, sz2 ->
            sz1 = sz2
            && List.equal Stdlib.( = )
                 (to_list (backets1, sz1))
                 (to_list (backets2, sz2)))

  let map (type tt ht) (module H1 : S' with type t = tt and type hash_map = ht)
      (hm_from : hash_map) (f : t -> tt) =
    to_list hm_from |> List.map f |> H1.of_list

  let filter pred hash_map = to_list hash_map |> List.filter pred |> of_list
  let fold_left f init hash_map = to_list hash_map |> List.fold_left f init
  let fold_right f hash_map init = List.fold_right f (to_list hash_map) init
end

```