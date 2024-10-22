(* Step 1: Sudoku Inputs *)
let sudoku1 = [|
  [|1; 0; 0; 4|];
  [|0; 0; 3; 0|];
  [|3; 0; 0; 1|];
  [|0; 2; 0; 0|];
|]

let sudoku2 = [|
  [|0; 2; 0; 4|];
  [|0; 0; 1; 0|];
  [|0; 1; 0; 0|];
  [|4; 0; 0; 0|];
|]

let sudoku3 = [|
  [|0; 0; 0; 2|];
  [|0; 3; 4; 0|];
  [|0; 0; 0; 0|];
  [|2; 0; 0; 0|];
|]

(* Helper function to get a column from a grid *)
let get_column grid col_idx =
  Array.map (fun row -> row.(col_idx)) grid

(* Helper function to get a 2x2 subgrid from a grid *)
let get_subgrid grid row col =
  let subgrid_row_start = (row / 2) * 2 in
  let subgrid_col_start = (col / 2) * 2 in
  [
    grid.(subgrid_row_start).(subgrid_col_start);
    grid.(subgrid_row_start).(subgrid_col_start + 1);
    grid.(subgrid_row_start + 1).(subgrid_col_start);
    grid.(subgrid_row_start + 1).(subgrid_col_start + 1)
  ]

(* Step 2: Check if a number can be placed at a given position *)
let is_valid grid num row col =
  (* Row check: Ensure the number does not already exist in the row *)
  if Array.exists ((=) num) grid.(row) then false
  (* Column check: Ensure the number does not exist in the column *)
  else if Array.exists ((=) num) (get_column grid col) then false
  (* Subgrid check: Ensure the number does not exist in the 2x2 subgrid *)
  else if List.exists ((=) num) (get_subgrid grid row col) then false
  else true

(* Step 3: Verify if the initial grid is valid *)
let is_valid_grid grid =
  let rec check_all_cells row col =
    if row = 4 then true
    else if col = 4 then check_all_cells (row + 1) 0
    else
      let value = grid.(row).(col) in
      if value <> 0 && not (
        (* Temporarily remove the value from the grid to check if it's valid *)
        let grid_copy = Array.map Array.copy grid in
        grid_copy.(row).(col) <- 0;
        is_valid grid_copy value row col
      )
      then false
      else check_all_cells row (col + 1)
  in
  check_all_cells 0 0

(* Step 4: Implement the backtracking algorithm *)
let rec solve grid =
  (* Find the first empty cell (0) *)
  let rec find_empty row col =
    if row = 4 then None
    else if col = 4 then find_empty (row + 1) 0
    else if grid.(row).(col) = 0 then Some (row, col)
    else find_empty row (col + 1)
  in
  match find_empty 0 0 with
  | None -> true  (* No empty cells, solved! *)
  | Some (row, col) ->
    (* Try numbers from 1 to 4 *)
    let rec try_number n =
      if n > 4 then false
      else if is_valid grid n row col then
        (* Modify the grid in place *)
        let () = grid.(row).(col) <- n in
        if solve grid then true
        else
          let () = grid.(row).(col) <- 0 in  (* Reset the cell if it doesn't work *)
          try_number (n + 1)
      else try_number (n + 1)
    in
    try_number 1

(* Step 5; Display the 4x4 Sudoku Grid *)
let print_grid grid =
  Array.iter (fun row ->
    Array.iter (fun cell -> Printf.printf "%d " cell) row;
    Printf.printf "\n"
  ) grid

(* Step 6: Handle edge cases *)
let solve_sudoku grid =
  if not (is_valid_grid grid) then
    Printf.printf "Invalid input grid\n"
  else if not (solve grid) then
    Printf.printf "No solution exists\n"
  else
    print_grid grid

(* Running the program *)
let () =
  Printf.printf "Solving Sudoku 1:\n";
  solve_sudoku sudoku1;
  Printf.printf "Solving Sudoku 2:\n";
  solve_sudoku sudoku2;
  Printf.printf "Solving Sudoku 3:\n";
  solve_sudoku sudoku3;