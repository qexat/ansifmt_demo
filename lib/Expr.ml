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

let rec tokenize : t -> Formatting.Token.t list = function
  | Binop (operator, left, right) ->
    tokenize left
    @ [ Formatting.Token.space ]
    @ Token_type.Operator.tokenize operator
    @ [ Formatting.Token.space ]
    @ tokenize right
  | Value value -> [ Formatting.Token_type.Literal_constant, Int.to_string value ]
  | Variable name -> [ Formatting.Token_type.Identifier, name ]
;;
