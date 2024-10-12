(* Step 1 Structure: Define the Location structure *)
type location = {
  name : string;
  x : float;
  y : float;
  priority : int;
}

(* Define the Vehicle structure *)
type vehicle = {
  id : int;
  capacity : int;
  route : location list;
}

(* Calculate the distance between two locations *)
let distance loc1 loc2 =
  let dx = loc2.x -. loc1.x in
  let dy = loc2.y -. loc1.y in
  sqrt (dx *. dx +. dy *. dy)

(* Custom function to split a list at a specified index *)
let rec split_at n lst =
  if n <= 0 then ([], lst)
  else match lst with
    | [] -> ([], [])
    | x :: xs ->
      let (l1, l2) = split_at (n - 1) xs in
      (x :: l1, l2)

(* Step 1: Input Delivery Locations *)
let rec input_locations n =
  if n <= 0 then []
  else
    let () = Printf.printf "Enter details for location %d:\n" (6 - n) in
    Printf.printf "Location name: ";
    let name = read_line () in
    Printf.printf "X coordinate: ";
    let x = float_of_string (read_line ()) in
    Printf.printf "Y coordinate: ";
    let y = float_of_string (read_line ()) in
    Printf.printf "Priority: ";
    let priority = int_of_string (read_line ()) in
    {name; x; y; priority} :: input_locations (n - 1)

(* Step 2: Input Vehicle Details *)
let rec input_vehicles n =
  if n <= 0 then []
  else
    let id = 3 - n in
    Printf.printf "Enter details for vehicle %d:\n" id;
    Printf.printf "Vehicle capacity: ";
    let capacity = int_of_string (read_line ()) in
    {id; capacity; route = []} :: input_vehicles (n - 1)

(* Step 3 P1: Sort Locations by Priority *)
let sort_locations_by_priority locations =
  List.sort (fun loc1 loc2 -> compare loc2.priority loc1.priority) locations

(* Step 3 P2: Assign Locations to Vehicles *)
let assign_locations_to_vehicles locations vehicles =
  let rec assign locs vehs acc =
    match vehs with
    | [] -> List.rev acc
    | v :: vs ->
      let assigned, remaining = split_at v.capacity locs in
      let new_vehicle = { v with route = assigned } in
      assign remaining vs (new_vehicle :: acc)
  in
  assign locations vehicles []

(* Step 3 P3: Route distance for Each Vehicle *)
let calculate_route_distance locations =
  let rec aux locs acc =
    match locs with
    | [] | [_] -> acc
    | a :: (b :: _ as rest) -> aux rest (acc +. distance a b)
  in
  aux locations 0.0

(* Step 4: Display the Results *)
let display_routes vehicles =
  List.iter (fun v ->
    Printf.printf "Vehicle %d route: %s\n" v.id
      (String.concat " -> " (List.map (fun loc -> loc.name) v.route));
    let dist = calculate_route_distance v.route in
    Printf.printf "Total distance: %.2f km\n" dist
  ) vehicles

(* Main Function to orchestrate all steps *)
let main () =
  (* Step 1: Get the number of delivery locations *)
  Printf.printf "Enter the number of delivery locations:\n";
  let num_locations = int_of_string (read_line ()) in
  let locations = input_locations num_locations in

  (* Step 2: Get the number of vehicles *)
  Printf.printf "Enter the number of vehicles:\n";
  let num_vehicles = int_of_string (read_line ()) in
  let vehicles = input_vehicles num_vehicles in

  (* Sort locations by priority *)
  let sorted_locations = sort_locations_by_priority locations in

  (* Assign locations to vehicles based on capacity *)
  let assigned_vehicles = assign_locations_to_vehicles sorted_locations vehicles in

  (* Display the routes and total distances *)
  display_routes assigned_vehicles

(* Run the main function *)
let () = main ()
