open OUnit2
open Hash_bag

let str_hash str =
  let s = String.to_seq str in
  Seq.fold_left (fun acc c -> acc + Char.code c) 0 s

let str_bag = init str_hash
let b1 = add str_bag "11"
let b2 = add b1 "11"
let b3 = add b2 "33"
let rb3 = remove b3 "33"

let hash_bag_tests =
  "hash_bag tests"
  >::: [
         ("add + find №1" >:: fun _ -> assert_equal true (find b1 "11"));
         ("add + find №2" >:: fun _ -> assert_equal true (find b3 "11"));
         ("add + find №3" >:: fun _ -> assert_equal false (find b3 "22"));
         ("count №1" >:: fun _ -> assert_equal 2 (count b2 "11"));
         ("count №2" >:: fun _ -> assert_equal 2 (count b3 "11"));
         ("count №3" >:: fun _ -> assert_equal 0 (count b3 "22"));
         ("count №4" >:: fun _ -> assert_equal 1 (count b3 "33"));
         ("remove №1" >:: fun _ -> assert_equal false (find rb3 "33"));
         ("remove №2" >:: fun _ -> assert_equal 2 (count rb3 "11"));
       ]

let _ = run_test_tt_main hash_bag_tests
