(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) *)
let rec last l =
  match l with
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last t;;

  (* or *)
  let rec last l = function
    | [] -> None
    | [x] -> Some x
    | _ :: t -> last t;;

(* 2. Find the last but one (last and penultimate) elements of a list. (easy) *)
let rec last_two = function
  | [] | [_] -> None
  | [h; t] -> Some (h,t)
  | h :: t -> last_two t;;

(* 3. Find the k'th element of a list. (easy) *)
let rec at id list =
  match list with
  | [] -> None
  | h :: t -> if id = 0 then Some h else at (id - 1) t;;

(* 4. Find the number of elements of a list. (easy) *)
let rec length = function
  | [] -> 0
  | [_] -> 1
  | _ :: t -> 1 + length t;;

  (* or with tail recursion *)

let rec __length c l =
  match l with
  | [] -> 0
  | [_] -> c + 1
  | _ :: t -> __length (c + 1) t;;
let length l = __length 0 l;;

(* 5. Reverse a list. (easy) *)
let rec rev = function
  | [] -> []
  | [x] -> [x]
  | h :: t -> rev t @ [h];;

  (* or with tail recursion *)
let rec __rev l rl =
  match l with
  | [] | [_] -> l @ rl
  | h :: t -> let new_rl = h :: rl in
    __rev t new_rl;;
let rev l = __rev l [];;

(* 6. Find out whether a list is a palindrome. (easy) *)
let is_palindrome l = l = rev l;;

(* 7. Flatten a nested list structure. (medium) *)
type 'a node =
  | One of 'a
  | Many of 'a node list;;

let rec flatten = function
  | [] -> []
  | One h :: t -> h :: flatten t
  | Many h :: t -> flatten h @ flatten t;;

  (* or with tail optimization *)
  let flatten l =
    let rec fltn l fl =
      match l with
      | [] -> fl
      | One h :: [] -> h :: fl
      | One h :: t ->
        let new_fl = h :: fl in
        fltn t new_fl
      | Many h :: [] -> fltn h fl
      | Many h :: t -> fltn t (fltn h fl) in (* here we can pass a product of partial application *)
    match l with
    | [] -> []
    | l -> fltn l [] |> rev;;

(* 8. Eliminate consecutive duplicates of list elements. (medium) *)
let rec compress = function
  | [] -> []
  | h :: [] -> [h]
  | h :: h1 :: t -> if h = h1 then compress(h :: t) else h :: compress(h1 :: t);;

(* or with tail optimization *)
let compress l =
  let rec comp l cl =
    match l with
    | [] -> cl
    | h :: [] -> h :: cl
    | h :: h1 :: t -> if h = h1 then comp (h :: t) cl else comp (h1 :: t) (h :: cl) in

  rev (comp l []);;

(* 9. Pack consecutive duplicates of list elements into sublists. (medium) *)

(* with tail optimization *)
let pack l =
  let rec _pack l group_item group rlist =
    match l with
    | [] -> group :: rlist
    | h :: t -> if h = group_item then
                  _pack t h (h :: group) rlist
                else
                  _pack t h [h] (group :: rlist) in
  match l with
  | [] -> []
  | h :: t -> _pack t h [h] [] |> rev;;

