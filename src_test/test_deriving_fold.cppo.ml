open OUnit2

type 'a btree = Node of 'a btree * 'a * 'a btree | Leaf
[@@deriving fold]

let test_btree ctxt =
  let btree  = (Node (Node (Leaf, 3, Leaf), 1, Node (Leaf, 2, Leaf))) in
  assert_equal ~printer:string_of_int 6 (fold_btree (+) 0 btree)

type 'a reflist = 'a Pervasives.ref list
[@@deriving fold]

let test_reflist ctxt =
  let reflist  = [ ref 3 ; ref 2 ; ref 1 ] in
  assert_equal ~printer:string_of_int 6 (fold_reflist (+) 0 reflist)

#if OCAML_VERSION >= (4, 03, 0)
type 'a btreer = Node of { lft: 'a btree; elt: 'a; rgt: 'a btree } | Leaf
[@@deriving fold]
#endif

type 'a ty = 'a * int list
[@@deriving fold]

type ('a, 'b) res = ('a, 'b) Result.result [@@deriving fold]

let test_result ctxt =
  let f = fold_res (+) (-) in
  assert_equal ~printer:string_of_int 1 (f 0 (Result.Ok 1));
  assert_equal ~printer:string_of_int (-1) (f 0 (Result.Error 1))

let suite = "Test deriving(fold)" >::: [
  "test_btree" >:: test_btree;
  "test_result" >:: test_result;
  "test_reflist" >:: test_reflist;
]
