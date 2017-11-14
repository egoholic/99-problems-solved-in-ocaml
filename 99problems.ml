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

(* 10. Run-length encoding of a list. (easy) *)

let encode l =
  let rec _encode l prev count rlist =
    match l with
    | [] -> rlist
    | h :: t -> if h = prev then
                  _encode t h (1 + count) rlist
                else
                  _encode t h 1 ((count, prev) :: rlist) in
  match l with
  | [] -> []
  | h :: t -> _encode t h 1 [] |> rev;;


(* 11. Modified run-length encoding. (easy) *)
type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let encode l =
  let rec _encode l prev count rlist =
    match l with
    | [] -> let rle = if count > 1 then Many(count, prev) else One prev in
            rle :: rlist
    | h :: t -> if h = prev then
                  _encode t h (1 + count) rlist
                else
                  let rle = if count > 1 then Many(count, prev) else One prev in
                  _encode t h 1 (rle :: rlist) in
  match l with
  | [] -> []
  | h :: t -> _encode t h 1 [] |> rev;;


(* 12. Decode a run-length encoded list. (medium) *)

let decode l =
  let rec list_from c x acc =
    if c > 0 then x :: (list_from (c - 1) x acc) else acc in
  let rec aux l acc =
    match l with
    | [] -> acc
    | h :: t -> match h with
                | One x -> aux t (x :: acc)
                | Many(c, x) -> aux t (list_from c x acc) in
  aux l [] |> rev;;

(* 14. Duplicate the elements of a list. (easy) *)

let rec duplicate = function
  | [] -> []
  | h :: t -> h :: h :: (duplicate t);;

  (* with tail optimization *)

let duplicate l =
  let rec aux l acc =
    match l with
    | [] -> acc
    | h :: t -> aux t (h :: h :: acc) in
  aux l [] |> rev;;

(* 15. Replicate the elements of a list a given number of times. (medium) *)

let rec replicate l c =
  let rec list_of e c acc =
    if c > 1 then e :: (list_of e (c - 1) acc) else e :: acc in
  match l with
  | [] -> []
  | h :: t -> (list_of h c (replicate t c));;

(* or with tail optimization *)

let replicate l c =
  let rec list_of e c acc =
    if c > 1 then list_of e (c - 1) (e :: acc) else e :: acc in
  let rec aux l acc =
    match l with
    | [] -> acc
    | h :: [] -> list_of h c acc
    | h :: t -> aux t (list_of h c acc) in
  aux l [] |> rev;;

(* 16. Drop every N'th element from a list. (medium) *)
let drop l ni =
  let rec aux l n =
    match l with
    | [] -> []
    | h :: t -> if n > 1 then h :: (aux t (n - 1)) else aux t ni in
  aux l ni;;


(* or with tail optimization *)

let drop l ni =
  let rec aux l n acc =
    match l with
    | [] -> acc
    | h :: t -> if n > 1 then aux t (n - 1) (h :: acc) else aux t ni acc in
  aux l ni [] |> rev;;

(* 17. Split a list into two parts; the length of the first part is given. (easy) *)
