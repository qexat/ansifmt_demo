open Ansifmt

module Literal = struct
  type t =
    | Identifier
    | Integer

  let to_string : t -> string = function
    | Identifier -> "IDENTIFIER"
    | Integer -> "INTEGER"
  ;;

  let show = to_string
end

module Operator = struct
  type t =
    | Plus
    | Minus
    | Asterisk
    | Slash

  let to_string : t -> string = function
    | Plus -> "PLUS"
    | Minus -> "MINUS"
    | Asterisk -> "ASTERISK"
    | Slash -> "SLASH"
  ;;

  let show : t -> string = function
    | Plus -> "+"
    | Minus -> "-"
    | Asterisk -> "*"
    | Slash -> "/"
  ;;

  open Formatting

  let to_element (operator : t) : Element.t =
    Element.singleton (Token_type.Operator_expr, show operator)
  ;;
end

type t =
  | Literal of Literal.t
  | Operator of Operator.t
  (* Misc *)
  | Comment
  | Eof

let to_string : t -> string = function
  | Literal literal -> Literal.to_string literal
  | Operator operator -> Operator.to_string operator
  | Comment -> "COMMENT"
  | Eof -> "EOF"
;;
