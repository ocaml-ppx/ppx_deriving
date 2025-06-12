(* ppx_deriving has several plugins, which we must name in our dune file. For
   example, ppx_deriving.show, ppx_deriving.enum etc. *)

(* ppx_deriving.show can create a printer for a type. *)
type tree =
  | Lf
  | Br of tree * int * tree [@@deriving show]
  (* Generates function show_tree *)

let tr = Br (Lf, 1, Br (Lf, 4, Lf))

let show () =
  print_endline (show_tree tr)

(* ppx_deriving.eq and ppx_deriving.ord can define custom equality. Here, some
   extra data in a record is ignored with regard to equality and ordering by
   giving custom functions for equality and comparison to one field. *) 
type r =
  {index : int;
   extra : int [@equal fun _ _ -> true] [@compare fun _ _ -> 0]}
   [@@deriving eq, ord]
   (* Generates functions equal_r and compare_r *)

let eq_ord () =
  Printf.printf "Equal: %b\n" (equal_r {index = 5; extra = 0} {index = 5; extra = 1});
  Printf.printf "Compare: %i\n" (compare_r {index = 5; extra = 0} {index = 6; extra = 1});
  Printf.printf "Compare: %i\n" (compare_r {index = 5; extra = 0} {index = 5; extra = 1})

(* ppx_deriving.enum gives an integer to each argument-less constructor of a datatype *)
type colour = Blue | Green | Red | Pink [@value 100] | Orange [@@deriving enum, show]

let enum () =
  Printf.printf "Integers: ";
  List.iter
    (fun x -> Printf.printf "%i " (colour_to_enum x))
    [Blue; Green; Red; Pink; Orange];
  Printf.printf "\n";
  Printf.printf "0 is %S\n" (show_colour (match colour_of_enum 2 with Some c -> c | _ -> assert false))

let () =
  match Sys.argv with
  | [|_; "show"|] -> show ()
  | [|_; "eq_ord"|] -> eq_ord ()
  | [|_; "enum"|] -> enum ()
  | _ -> Printf.eprintf "ppx_deriving example: unknown command line\n"
