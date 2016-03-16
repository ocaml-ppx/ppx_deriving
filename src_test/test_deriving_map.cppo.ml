open OUnit2

module T : sig
  
  type 'a btree = Node of 'a btree * 'a * 'a btree | Leaf
  [@@deriving map, show]

#if OCAML_VERSION >= (4, 03, 0)
  type 'a btreer = Noder of { lft: 'a btree; elt: 'a; rgt: 'a btree } | Leafr
  [@@deriving map]
#endif

  type var0 = A0 of int [@@deriving map,show]

  type 'a var1 = A1 of 'a [@@deriving map,show]

  type 'a var2 = A2 of 'a | B2 of int [@@deriving map,show]

  type ('a,'b) var3 = A3 of 'a | B3 of bool | C3 of 'b * ('a,'b) var3 [@@deriving map,show]

  type record0 = { a0 : int } [@@deriving map,show]

  type 'a record1 = { a1 : 'a } [@@deriving map,show]

  type 'a record2 = { a2 : 'a; b2 : int } [@@deriving map,show]

  type ('a,'b) record3 = { a3 : 'a; b3 : bool; c3 : 'b } [@@deriving map,show]

end = struct

  type 'a btree = Node of 'a btree * 'a * 'a btree | Leaf
  [@@deriving map, show]

#if OCAML_VERSION >= (4, 03, 0)
  type 'a btreer = Noder of { lft: 'a btree; elt: 'a; rgt: 'a btree } | Leafr
  [@@deriving map]
#endif

  type 'a ty = 'a * int list
  [@@deriving map]

  (* variants and records with mixtures of poly/nonpoly fields *)

  type var0 = A0 of int [@@deriving map,show]

  type 'a var1 = A1 of 'a [@@deriving map,show]

  type 'a var2 = A2 of 'a | B2 of int [@@deriving map,show]

  type ('a,'b) var3 = A3 of 'a | B3 of bool | C3 of 'b * ('a,'b) var3 [@@deriving map,show]

  type record0 = { a0 : int } [@@deriving map,show]

  type 'a record1 = { a1 : 'a } [@@deriving map,show]

  type 'a record2 = { a2 : 'a; b2 : int } [@@deriving map,show]

  type ('a,'b) record3 = { a3 : 'a; b3 : bool; c3 : 'b } [@@deriving map,show]

end

open T

let fmt_chr fmt = Format.fprintf fmt "%c"
let fmt_flt fmt = Format.fprintf fmt "%f"
let fmt_int fmt = Format.fprintf fmt "%d"
let fmt_str fmt = Format.fprintf fmt "%s"

let test_btree ctxt =
  let btree  = (Node (Node (Leaf, 0, Leaf), 1, Node (Leaf, 2, Leaf))) in
  let btree' = map_btree (fun x -> x + 1) btree in
  assert_equal ~printer:(show_btree fmt_int)
               (Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf)))
               btree'

(* tests for #81 and #82 - allow non-poly fields in records and variants and
   provide more general type for map signature:
     ('a -> 'x) -> ... -> ('a,...) t -> ('x,...) t *)

let test_var0 ctxt =
  assert_equal ~printer:show_var0 (A0 10) (map_var0 (A0 10))

let test_var1 ctxt = 
  assert_equal ~printer:(show_var1 fmt_int) (A1 1) (map_var1 ((+)1) (A1 0));
  assert_equal ~printer:(show_var1 fmt_str) (A1 "2") (map_var1 string_of_int (A1 2))

let test_var2 ctxt = 
  assert_equal ~printer:(show_var2 fmt_int) (B2 7) (map_var2 ((+)1) (B2 7));
  assert_equal ~printer:(show_var2 fmt_int) (A2 5) (map_var2 ((+)1) (A2 4));
  assert_equal ~printer:(show_var2 fmt_int) (A2 5) (map_var2 int_of_float (A2 5.))

let test_var3 ctxt = 
  let show,map = show_var3 fmt_int fmt_str, map_var3 ((+)1) String.uppercase in
  assert_equal ~printer:show (A3 2) (map (A3 1));
  assert_equal ~printer:show (B3 false) (map (B3 false));
  assert_equal ~printer:show (C3("ABC", A3 3)) (map (C3("abc", A3 2)));
  assert_equal ~printer:show (C3("XYZ", B3 true)) (map (C3("xyz", B3 true)));
  let show,map = show_var3 fmt_int fmt_flt, map_var3 Char.code float_of_int in
  assert_equal ~printer:show (A3 97) (map (A3 'a'));
  assert_equal ~printer:show (B3 false) (map (B3 false));
  assert_equal ~printer:show (C3(4., A3 98)) (map (C3(4, A3 'b')));
  assert_equal ~printer:show (C3(5., B3 true)) (map (C3(5, B3 true)))

let test_record0 ctxt = 
  assert_equal ~printer:show_record0 {a0=10} (map_record0 {a0=10})

let test_record1 ctxt = 
  assert_equal ~printer:(show_record1 fmt_int) {a1=1} (map_record1 ((+)1) {a1=0});
  assert_equal ~printer:(show_record1 fmt_str) {a1="2"} (map_record1 string_of_int {a1=2})

let test_record2 ctxt = 
  assert_equal ~printer:(show_record2 fmt_int) {a2=5;b2=7} (map_record2 ((+)1) {a2=4;b2=7});
  assert_equal ~printer:(show_record2 fmt_int) {a2=5;b2=0} (map_record2 int_of_float {a2=5.;b2=0})

let test_record3 ctxt = 
  assert_equal ~printer:(show_record3 fmt_int fmt_str)
    {a3=5;b3=false;c3="ABC"} (map_record3 ((+)1) String.uppercase {a3=4;b3=false;c3="abc"});
  assert_equal ~printer:(show_record3 fmt_int fmt_flt)
    {a3=97;b3=false;c3=4.} (map_record3 Char.code float_of_int {a3='a';b3=false;c3=4})

let suite = "Test deriving(map)" >::: [
    "test_btree" >:: test_btree;
    "test_var0" >:: test_var0;
    "test_var1" >:: test_var1;
    "test_var2" >:: test_var2;
    "test_var3" >:: test_var3;
    "test_record0" >:: test_record0;
    "test_record1" >:: test_record1;
    "test_record2" >:: test_record2;
    "test_record3" >:: test_record3;
  ]


