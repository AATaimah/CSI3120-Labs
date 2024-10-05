(* STEP 1 Define a type called 'job' to represent each job with its start time, duration, and priority *)
type job = {
  start_time: int;  (* Start time in minutes from midnight *)
  duration: int;    (* Duration in minutes *)
  priority: int;    (* Priority of the job *)
}

(* STEP 2 Function to convert hours and minutes into total minutes from midnight *)
let time_to_minutes hours minutes =
  hours * 60 + minutes

(* REVERSE OF STEP 2 Function to convert total minutes back into hours and minutes *)
let minutes_to_time total_minutes =
  let hours = total_minutes / 60 in  (* Extract hours from total minutes *)
  let minutes = total_minutes mod 60 in  (* Extract remaining minutes *)
  (hours, minutes)

(* HELPER Function to read an integer input from the user *)
let read_int prompt =
  print_string prompt;  (* Print prompt for the user *)
  let line = read_line () in  (* Read input from user *)
  int_of_string (String.trim line)  (* Convert input to integer *)

(* STEP 3 HELPER Function to read the details of a single job from the user *)
let read_job n =
  Printf.printf "For job %d, please enter the following details:\n" n;

  (* Get start time inputs *)
  let hours = read_int "- Start Time (hours): " in
  let minutes = read_int "- Start Time (minutes): " in

  (* Get start time in minutes *)
  let start_time = time_to_minutes hours minutes in

  (* Get duration input *)  
  let duration = read_int "- Duration (minutes): " in

  (* Get priority input *)  
  let priority = read_int "- Priority: " in

  (* Return one job *)
  { start_time; duration; priority } 

(* STEP 3 Function to read multiple jobs by calling the helper *)
let rec read_jobs num_jobs =
  let rec aux n acc =
    if n > num_jobs then
      (* Return accumulated jobs in reverse order *)
      List.rev acc 
    else
      let job = read_job n in

      (* Accumulate jobs and continue recursion *)
      aux (n + 1) (job :: acc) 
  in
  (* Start with empty list and read all jobs *)
  aux 1 []

(* STEP 4 STRATEGY 1: Schedule jobs without overlaps *)
let schedule_jobs jobs =
  let sorted_jobs = List.sort (fun a b -> compare a.start_time b.start_time) jobs in

  (* Auxiliary function to recursively schedule jobs *)
  let rec aux scheduled remaining =
    match remaining with
    | [] -> List.rev scheduled  (* Reverse scheduled jobs when done *)
    | job :: rest ->
      match scheduled with
      | [] -> aux [job] rest  (* Schedule the first job *)
      | last_job :: _ ->
        let end_time_last = last_job.start_time + last_job.duration in
        if job.start_time >= end_time_last then
          (* Schedule non-overlapping job *)
          aux (job :: scheduled) rest
        else
          (* Skip overlapping job *)
          aux scheduled rest
  in
  (* Start scheduling with an empty list *)
  aux [] sorted_jobs

(* STEP 4 STRATEGY 2: Schedule jobs with highest priority *)
let schedule_jobs_max_priority jobs =
  let sorted_jobs = List.sort (fun a b -> compare b.priority a.priority) jobs in
  let rec aux scheduled remaining =
    match remaining with
    | [] -> List.rev scheduled  (* Return scheduled jobs when done *)
    | job :: rest ->
      (* Check if job overlaps with any already scheduled jobs *)
      let overlaps_with_scheduled =
        List.exists (fun scheduled_job ->
          let end_time_scheduled = scheduled_job.start_time + scheduled_job.duration in
          let end_time_job = job.start_time + job.duration in
          not (end_time_job <= scheduled_job.start_time || job.start_time >= end_time_scheduled)
        ) scheduled
      in
      if not overlaps_with_scheduled then
        (* Schedule job if no overlap *)
        aux (job :: scheduled) rest  
      else
        (* Skip job if overlapping *)
        aux scheduled rest  
  in
  (* Start scheduling with an empty list *)
  aux [] sorted_jobs  

(* STEP 4 STRATEGY 3: Schedule jobs to minimize idle time *)
let schedule_jobs_min_idle jobs =
  let sorted_jobs = List.sort (fun a b -> compare a.start_time b.start_time) jobs in
  let rec aux scheduled current_end_time remaining_jobs =
    if remaining_jobs = [] then
      (* Return scheduled jobs when done *)
      List.rev scheduled 
    else
      (* Find jobs that can start after the current end time *)
      let possible_jobs = List.filter (fun job -> job.start_time >= current_end_time) remaining_jobs in
      if possible_jobs = [] then
        let next_job = List.hd remaining_jobs in
        aux (next_job :: scheduled) (next_job.start_time + next_job.duration) (List.tl remaining_jobs)
      else
        let next_job = List.hd possible_jobs in
        let new_remaining_jobs = List.filter (fun job -> job <> next_job) remaining_jobs in
        aux (next_job :: scheduled) (next_job.start_time + next_job.duration) new_remaining_jobs
  in
  (* Start scheduling with current end time = 0 *)
  aux [] 0 sorted_jobs  

(* STEP 5 Function to print the scheduled jobs in a friendly format *)
let print_schedule scheduled_jobs strategy_name =
  Printf.printf "Scheduled Jobs (%s):\n" strategy_name;
  List.iter (fun job ->
    let (hours, minutes) = minutes_to_time job.start_time in
    Printf.printf "Job scheduled: Start Time = %d:%02d, Duration = %d minutes, Priority = %d\n"
      hours minutes job.duration job.priority
  ) scheduled_jobs

(* STEP 6 Main function *)
let () =
  let num_jobs = read_int "How many jobs do you want to schedule? " in
  let jobs = read_jobs num_jobs in
  Printf.printf "Choose a scheduling strategy (1 for No Overlaps, 2 for Max Priority, 3 for Minimize Idle Time): ";
  let strategy_choice = read_int "" in
  let scheduled_jobs, strategy_name =
    match strategy_choice with
    | 1 ->
      (schedule_jobs jobs, "No Overlaps")
    | 2 ->
      (schedule_jobs_max_priority jobs, "Max Priority")
    | 3 ->
      (schedule_jobs_min_idle jobs, "Minimize Idle Time")
    | _ ->
      Printf.printf "Invalid choice. Defaulting to No Overlaps strategy.\n";
      (schedule_jobs jobs, "No Overlaps")
  in
  print_schedule scheduled_jobs strategy_name
