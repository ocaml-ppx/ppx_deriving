open OUnit2

let fmt_chr fmt = Format.fprintf fmt "%c"
let fmt_flt fmt = Format.fprintf fmt "%f"
let fmt_int fmt = Format.fprintf fmt "%d"
let fmt_str fmt = Format.fprintf fmt "%s"

type 'a btree = Node of 'a btree * 'a * 'a btree | Leaf
[@@deriving map, show]

let test_btree ctxt =
  let btree  = (Node (Node (Leaf, 0, Leaf), 1, Node (Leaf, 2, Leaf))) in
  let btree' = map_btree (fun x -> x + 1) btree in
  assert_equal ~printer:(show_btree (fun fmt -> Format.fprintf fmt "%d"))
               (Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf)))
               btree'

#if OCAML_VERSION >= (4, 03, 0)
type 'a btreer = Node of { lft: 'a btree; elt: 'a; rgt: 'a btree } | Leaf
[@@deriving map]
#endif

type 'a ty = 'a * int list
[@@deriving map]

(* records *)

(* no poly field *)
type record0 = { a0 : int } [@@deriving map,show]
let test_record0 ctxt = 
  assert_equal ~printer:show_record0 
    {a0=0} (map_record0 {a0=0})

(* one poly field *)
type 'a record1 = { a1 : 'a } [@@deriving map,show]
let test_record1 ctxt = 
  assert_equal ~printer:(show_record1 fmt_int)
    {a1=1} (map_record1 ((+)1) {a1=0})

(* mixture of poly / non-poly fields *)
type 'a record2 = { a2 : 'a; b2 : int } [@@deriving map,show]
let test_record2 ctxt = 
  assert_equal ~printer:(show_record2 fmt_int)
    {a2=5;b2=7} (map_record2 ((+)1) {a2=4;b2=7})

type ('a,'b) record3 = { a3 : 'a; b3 : bool; c3 : 'b } [@@deriving map,show]
let test_record3 ctxt = 
  assert_equal ~printer:(show_record3 fmt_int fmt_str)
    {a3=5;b3=false;c3="ABC"} (map_record3 ((+)1) String.uppercase {a3=4;b3=false;c3="abc"})

(* change types *)
let test_record3_poly ctxt =
  let recd  = {a3='a';b3=true;c3=4} in
  let mapd = map_record3 Char.code float_of_int recd in
  let expt  = {a3=97;b3=true;c3=4.} in
  assert_bool
    (Printf.sprintf "(map %s = %s) <> %s"
      (show_record3 fmt_chr fmt_int recd)
      (show_record3 fmt_int fmt_flt mapd)
      (show_record3 fmt_int fmt_flt expt))
    (mapd = expt)

let suite = "Test deriving(map)" >::: [
    "test_btree" >:: test_btree;
    "test_record0" >:: test_record0;
    "test_record1" >:: test_record1;
    "test_record2" >:: test_record2;
    "test_record3" >:: test_record3;
    "test_record3_poly" >:: test_record3_poly;
  ]

