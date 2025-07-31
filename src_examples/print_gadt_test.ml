type 'a expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | Pair : 'x expr * 'y expr -> ('x * 'y) expr
[@@deriving show]

let test_case =
  Pair (Pair (Int 10, Bool true), Pair (Int 42, Int 42))

let () =
  Format.printf "expr: %a@." (pp_expr (fun _ _ -> ())) test_case;
  ()

(*
tree: (Print_gadt_test.Pair (
         (Print_gadt_test.Pair ((Print_gadt_test.Int 10),
            (Print_gadt_test.Bool true))),
         (Print_gadt_test.Pair ((Print_gadt_test.Int 42),
            (Print_gadt_test.Int 42)))
         ))
*)
