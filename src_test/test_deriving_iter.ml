open OUnit2

type 'a btree = Node of 'a btree * 'a * 'a btree | Leaf
[@@deriving iter]

let test_btree ctxt =
  let lst = ref [] in
  iter_btree (fun x -> lst := x :: !lst)
             (Node (Node (Leaf, 0, Leaf), 1, Node (Leaf, 2, Leaf)));
  assert_equal [2;1;0] !lst

type 'a ty = 'a * int list
[@@deriving iter]

let suite = "Test deriving(iter)" >::: [
    "test_btree" >:: test_btree;
  ]
