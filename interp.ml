type id = string
type op = Plus | Minus | Times | Div ;;
type stm
  = Stmts of stm * stm
  | Assign of id * exp
  | Print of exp
and exp
  = ID of id
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | Div of exp * exp

exception No_such_symbol
let e0 = fun _ -> raise No_such_symbol
let update var vl env =
  fun v -> if v = var
    then vl
    else env v