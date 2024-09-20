let rec map2 f lst1 lst2 =
  match (lst1, lst2) with
  | ([], []) -> []  (* Base case: both lists are empty *)
  | (x::xs, y::ys) -> (f x y) :: (map2 f xs ys)  (* Recursively apply f to heads and tails *)
  | _ -> failwith "Lists are of different lengths"  (* If lists have different lengths *)
;;


map2 (fun x y -> x + y) [1; 2; 3] [4; 5; 6];;

