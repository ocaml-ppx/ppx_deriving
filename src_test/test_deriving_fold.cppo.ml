open OUnit2

type 'a btree = Node of 'a btree * 'a * 'a btree | Leaf
[@@deriving fold]

let test_btree ctxt =
  let btree  = (Node (Node (Leaf, 3, Leaf), 1, Node (Leaf, 2, Leaf))) in
  assert_equal ~printer:string_of_int 6 (fold_btree (+) 0 btree)

#if OCAML_VERSION >= (4, 03, 0)
type 'a btreer = Node of { lft: 'a btree; elt: 'a; rgt: 'a btree } | Leaf
[@@deriving fold]
#endif

type 'a ty = 'a * int list
[@@deriving fold]

let suite = "Test deriving(fold)" >::: [
    "test_btree" >:: test_btree;
  ]
