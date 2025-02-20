open Ansifmt

type t =
  | Binop of (Token_type.Operator.t * t * t)
  | Value of int
  | Variable of string

let ( + ) : t -> t -> t = fun left right -> Binop (Token_type.Operator.Plus, left, right)
let ( - ) : t -> t -> t = fun left right -> Binop (Token_type.Operator.Minus, left, right)

let ( * ) : t -> t -> t =
  fun left right -> Binop (Token_type.Operator.Asterisk, left, right)
;;

let ( / ) : t -> t -> t = fun left right -> Binop (Token_type.Operator.Slash, left, right)
let value (value : int) : t = Value value
let var (name : string) : t = Variable name

let is_atomic : t -> bool = function
  | Value _ | Variable _ -> true
  | _ -> false
;;

let rec to_element : t -> Formatting.Element.t =
  let open Formatting.Element in
  let apply_if condition func arg = if condition then func arg else arg in
  function
  | Binop (operator, left, right) ->
    [ apply_if (Fun.negate is_atomic left) parenthesized (to_element left)
    ; Token_type.Operator.to_element operator
    ; apply_if (Fun.negate is_atomic right) parenthesized (to_element right)
    ]
    |> cluster
    |> intercalated ~separating:[ Formatting.Token.space ]
  | Value value -> singleton (Formatting.Token_type.Literal_constant, Int.to_string value)
  | Variable name -> singleton (Formatting.Token_type.Identifier, name)
;;
