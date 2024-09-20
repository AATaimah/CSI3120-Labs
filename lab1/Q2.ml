let filter_even lst =
  List.filter (fun x -> x mod 2 = 0) lst;;

let even_numbers = filter_even [1; 2; 3; 4; 5; 6];;