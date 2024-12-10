type 'a hash_map

val find : 'a hash_map -> 'a -> bool
val add : 'a hash_map -> 'a -> 'a hash_map
val remove : 'a hash_map -> 'a -> 'a hash_map
val size : 'a hash_map -> int
val count : 'a hash_map -> 'a -> int
val init : ('a -> int) -> 'a hash_map
