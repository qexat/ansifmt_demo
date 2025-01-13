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

  let tokenize (operator : t) : Formatting.Token.t list =
    [ Formatting.Token_type.Operator_expr, show operator ]
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
