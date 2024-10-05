

(* Question 1 *)

let rec map2 f lst1 lst2 =
  match (lst1, lst2) with
  | ([], []) -> []  (* Base case: both lists are empty *)
  | (x::xs, y::ys) -> (f x y) :: (map2 f xs ys)  (* Recursively apply f to heads and tails *)
  | _ -> failwith "Lists are of different lengths"  (* If lists have different lengths *)
;;



(* Question 2 *)

let filter_even lst =
  List.filter (fun x -> x mod 2 = 0) lst;;



(* Question 3 *)

let compose_functions f g =
  fun x -> f (g x)

let composed = compose_functions (fun x -> x * 2) (fun y -> y + 3);; 



(* Question 4 *)

let rec reduce f acc lst =
  match lst with
  | [] -> acc
  | x::xs -> reduce f (f acc x) xs

