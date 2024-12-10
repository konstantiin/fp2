let rec remove_from_list_t lst el res =
  match lst with
  | [] -> res
  | h :: t ->
      if Stdlib.( = ) h el then remove_from_list_t t el res
      else remove_from_list_t t el (h :: res)

let remove_from_list lst el =
  let reversed = remove_from_list_t lst el [] in
  List.rev reversed
