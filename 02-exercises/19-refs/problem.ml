open! Base

(* It is sometimes useful to create a single mutable value. We can do this using
   a [ref]. We can create an [int ref] containing 0 as follows. *)
let x = ref 0

(* Then we can access the value in the ref using the [!] operator, and we can
   update it using the [:=] operator. So, we could increment our ref as
   follows. *)
let () =
  x := !x + 1

(* Write a function min_and_max which returns a tuple containing the minimum and
   maximum values in a non-empty list of positive integers. Your function should
   raise if the list is empty.

   You could do this using [List.fold], but for the purpose of this exercise,
   let's iterate over the list and explicitly maintain refs of the minimum and
   maximum values seen so far instead. *)
let min_and_max lst =
  let rec min_and_max_helper lst min_ref max_ref =
    match lst with
    | hd :: tl -> 
        if hd < !min_ref then min_ref := hd;
        if hd > !max_ref then max_ref := hd;
        min_and_max_helper tl min_ref max_ref
    | _ -> (!min_ref, !max_ref)
  in min_and_max_helper lst (ref 9999) (ref (-1 * 9999))

exception Empty_list
let min_and_max lst = 
  match lst with
  | [] -> raise Empty_list
  | hd :: tl ->
      let rec aux (min_val, max_val) = function
        | [] -> (min_val, max_val)  (* Return the final result when the list is empty *)
        | h :: t ->
            aux (min h min_val, max h max_val) t
      in
      aux (hd, hd) tl

(* By the way, can you guess how a [ref] is implemented under the hood? 

   (Hint: exercise 18.) *)

let%test "Testing min_and_max..." =
  [%compare.equal: int * int] (min_and_max [5;9;2;4;3]) (2,9) 
;;

let%test "Testing min_and_max..." =
  [%compare.equal: int*int] (min_and_max [11;15;7;34]) (7,34)
;;
