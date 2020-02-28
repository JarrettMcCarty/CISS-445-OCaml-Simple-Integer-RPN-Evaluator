(*
Name: Jarrett McCarty
File: a06q02.ml
 *)

exception EmptyException;;
exception NotInttokException;;


type 'a list = Empty | Cons of 'a * 'a list;;

let init = Empty;;

let size list =
  let rec help list acc = match list with
      Empty -> acc
    | Cons (x, xs) -> help xs (acc + 1)
  in
  help list 0
;;

let is_empty list = match list with
    Empty -> true
  | _ -> false
;;

let head list = match list with
    Empty -> raise EmptyException
  | Cons (x, xs) -> x
;;

let tail list =
  let rec help list acc = match list with
    Empty -> acc
  | Cons (x, Empty) -> help Empty x
  | Cons (x, xs) -> help xs acc
  in
  help list (head list)
;;

let insert_head list n = Cons (n, list);;

let delete_head list = match list with
    Empty -> Empty
  | Cons (x, xs) -> xs
;;

let insert_tail list n =
  let rec help list acc = match list with
      Empty -> acc
    | Cons (x, xs) -> Cons (x, help xs acc)
  in
  help list (Cons (n, Empty))
;;

let delete_tail list =
  let rec help list acc = match list with
      Empty -> acc
    | Cons (x, Empty) -> acc
    | Cons (x, xs) -> Cons (x, help xs acc)
  in
  help list Empty
;;

let reverse list =
  let rec help list acc = match list with
      Empty -> acc
    | Cons (x, Empty) -> help Empty (insert_head acc x)
    | Cons (x, xs) -> help xs (insert_head acc x)
  in
  help list Empty
;;

type token = Inttok of int
| Plustok
| Minustok
| Multtok
| Divtok
;;

let get_int tok = match tok with
    Inttok i -> i
  | _ -> raise NotInttokException
;;

let parse str =
  let rec help i acc buffer =
    if i < 0 then
      if String.length buffer > 0
      then (insert_head acc (Inttok (int_of_string buffer)))
      else acc
    else if str.[i] = '+' then help (i - 1) (insert_head acc Plustok) ""
    else if str.[i] = '-' then help (i - 1) (insert_head acc Minustok) ""
    else if str.[i] = '*' then help (i - 1) (insert_head acc Multtok) ""
    else if str.[i] = '/' then help (i - 1) (insert_head acc Divtok) ""
    else if str.[i] = ' ' then
      if String.length buffer > 0
      then help (i - 1) (insert_head acc (Inttok (int_of_string buffer))) ""
      else help (i - 1) acc buffer
    else help (i - 1) acc (buffer ^ String.make 1 str.[i])
  in
  help (String.length str - 1) Empty ""
;;

let eval tok acc = match tok with
    Inttok i -> insert_head acc (Inttok i) 
  | Multtok ->  let r = head acc in
                let acc = delete_head acc in
                let l = head acc in
                let acc = delete_head acc in
                insert_head acc (Inttok (get_int (l) * get_int (r)));
  | Divtok -> let r = head acc in
              let acc = delete_head acc in
              let l = head acc in
              let acc = delete_head acc in
              insert_head acc (Inttok (get_int (l) / get_int (r)))
  | Plustok -> let r = head acc in
               let acc = delete_head acc in
               let l = head acc in
               let acc = delete_head acc in
               insert_head acc (Inttok (get_int (l) + get_int (r)))
  | Minustok -> let r = head acc in
                let acc = delete_head acc in
                let l = head acc in
                let acc = delete_head acc in
                insert_head acc (Inttok (get_int (l) - get_int (r)))
;;

let eval_rpn list =
  let rec help list acc = match list with
      Empty -> head acc
    | Cons (x, xs) -> help xs (eval x acc)
  in
  help list Empty
;;
