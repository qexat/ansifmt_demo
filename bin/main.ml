open Ansifmt.Prelude
open Ansifmt_demo
open Expr

let expr = (var "x" + value 3) * var "y"
let () = print_formatted expr ~using:(module Expr)
