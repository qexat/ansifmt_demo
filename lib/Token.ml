type t =
  { ty : Token_type.t
  ; lexeme : string
  }

let to_string : t -> string = function
  | { ty; lexeme } ->
    Printf.sprintf "%s '%s'" (Token_type.to_string ty) (String.escaped lexeme)
;;
