open Ansifmt
open Ansifmt_demo

let expr =
  let open Expr in
  (var "x" + (value 3 - value 5)) * var "y"
;;

let () = IO.print_formatted expr ~using:(module Expr)
