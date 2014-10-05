open OUnit2

type 'a btree = Node of 'a btree * 'a * 'a btree | Leaf
[@@deriving map, show]

let test_btree ctxt =
  let btree  = (Node (Node (Leaf, 0, Leaf), 1, Node (Leaf, 2, Leaf))) in
  let btree' = map_btree (fun x -> x + 1) btree in
  assert_equal ~printer:(show_btree (fun fmt -> Format.fprintf fmt "%d"))
               (Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf)))
               btree'

type 'a ty = 'a * int list
[@@deriving map]

let suite = "Test deriving(map)" >::: [
    "test_btree" >:: test_btree;
  ]
