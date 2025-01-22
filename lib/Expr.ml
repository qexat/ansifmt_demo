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

let rec tokenize : t -> Formatting.Tree.t =
  let open Formatting.Tree in
  function
  | Binop (operator, left, right) ->
    block
      [ parenthesize_if (fun x -> not (is_atomic x)) tokenize left
      ; simple
          ((Formatting.Token.space :: Token_type.Operator.tokenize operator)
           @ [ Formatting.Token.space ])
      ; parenthesize_if (fun x -> not (is_atomic x)) tokenize right
      ]
  | Value value -> simple [ Formatting.Token_type.Literal_constant, Int.to_string value ]
  | Variable name -> simple [ Formatting.Token_type.Identifier, name ]
;;
