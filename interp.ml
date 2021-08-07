type id = string

type op = Plus | Minus | Times | Div

type stm = Stmts of stm * stm | Assign of id * exp | Print of exp

and exp =
  | ID of id
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | Div of exp * exp

exception No_such_symbol

let e0 _ = raise No_such_symbol

let update var vl env v = if v = var then vl else env v

let rec trans_stmt ast env =
  match ast with
  | Stmts (s1, s2) ->
      let env' = trans_stmt sq env in
      trans_stms s2 env
  | Assign (var, e) ->
      let vl = trans_exp e env in
      update var vl env
  | Print e ->
      let vl = trans_exp e env in
      (print_int vl; print_string "\n"; env)
