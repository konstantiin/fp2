open OUnit2
open Hash_bag

module HashableStr = struct
  type t = string

  let hash str =
    let s = String.to_seq str in
    Seq.fold_left (fun acc c -> acc + Char.code c) 0 s
end

module StringHBag = Make (HashableStr)

let str_bag = StringHBag.empty ()
let b1 = StringHBag.add str_bag "11"
let b2 = StringHBag.add b1 "11"
let b3 = StringHBag.add b2 "33"
let rb3 = StringHBag.remove b3 "33"

let hash_bag_tests =
  "hash_bag tests"
  >::: [
         ( "add + find №1" >:: fun _ ->
           assert_equal true (StringHBag.find b1 "11") );
         ( "add + find №2" >:: fun _ ->
           assert_equal true (StringHBag.find b3 "11") );
         ( "add + find №3" >:: fun _ ->
           assert_equal false (StringHBag.find b3 "22") );
         ("count №1" >:: fun _ -> assert_equal 2 (StringHBag.count b2 "11"));
         ("count №2" >:: fun _ -> assert_equal 2 (StringHBag.count b3 "11"));
         ("count №3" >:: fun _ -> assert_equal 0 (StringHBag.count b3 "22"));
         ("count №4" >:: fun _ -> assert_equal 1 (StringHBag.count b3 "33"));
         ("remove №1" >:: fun _ -> assert_equal false (StringHBag.find rb3 "33"));
         ("remove №2" >:: fun _ -> assert_equal 2 (StringHBag.count rb3 "11"));
       ]

let _ = run_test_tt_main hash_bag_tests

module IntHashBag = Make (struct
  type t = int

  let hash = Fun.id
end)

let test =
  QCheck.Test.make ~count:1000 ~name:"hash_bag_neutral" QCheck.small_nat
    (fun x -> IntHashBag.(find (add (empty ()) x) x))

let _ = QCheck.Test.check_exn test
