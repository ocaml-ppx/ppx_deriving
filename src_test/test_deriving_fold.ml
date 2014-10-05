open OUnit2

type 'a btree = Node of 'a btree * 'a * 'a btree | Leaf
[@@deriving fold]

let test_btree ctxt =
  let btree  = (Node (Node (Leaf, 3, Leaf), 1, Node (Leaf, 2, Leaf))) in
  assert_equal ~printer:string_of_int 6 (fold_btree (+) 0 btree)

type 'a ty = 'a * int list
[@@deriving fold]

let suite = "Test deriving(fold)" >::: [
    "test_btree" >:: test_btree;
  ]
