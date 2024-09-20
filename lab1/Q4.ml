let rec reduce f acc lst =
  match lst with
  | [] -> acc
  | x::xs -> reduce f (f acc x) xs

let result = reduce (fun x y -> x + y) 0 [1; 2; 3; 4];;