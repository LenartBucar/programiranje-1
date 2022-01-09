type available = { loc : int * int; possible : int option list }
exception No_Options

type state = { problem : Model.problem; current_grid : int option Model.grid; options : available list}

(* VVVV DIGEST VVVV *)

let digest_state (state : state) : string =
  (* Return a unique digest for a state based on the grid, used for debugging *)
  (Array.fold_left (fun acc s -> acc ^ s) "" (Array.map (fun x ->
    Array.fold_left (fun acc s -> acc ^ s) "" (Array.map (function
	  | None -> " "
	  | Some t -> string_of_int t
	) x)
  ) state.current_grid)) |> Digest.string |> Digest.to_hex

(* ^^^^ DIGEST ^^^^ *)

let print_available (available : available) : unit =
  (* Prints all options for a given position *)
  Printf.printf "(%d,%d) - " (fst available.loc) (snd available.loc);
  List.iter (fun x -> Printf.printf "%d " (Option.get x)) available.possible;
  Printf.printf "\n"

let print_constraint (constr : Model.constr) : unit =
  (* Prints a given constraint *)
  Printf.printf "%s - %d - " constr.constr_type constr.value;
  List.iter (fun x -> Printf.printf "(%d,%d)" (fst x) (snd x)) constr.cells;
  Printf.printf "\n"

let print_state (state : state) : unit =
  (* Prints the given state *)
  Printf.printf "%s\n" (digest_state state);
  Model.print_grid
    (function None -> " " | Some digit -> string_of_int digit)
    state.current_grid;
  (* List.iter print_constraint state.problem.constraints; *)
  List.iter print_available state.options

type response = Solved of Model.solution | Unsolved of state | Fail of state

let triangular (n : int) : int = (n * (n+1)) / 2

let find_pos (el : 'a) (lst : 'a list) : int = 
    (* returns the position of the first occurance of the element, starting with 1 *)
	let rec aux n el = function
	  | [] -> -1
	  | h::t when h = el -> n
	  | _::t -> aux (n+1) el t
	in
	aux 1 el lst

let take (n : int) (lst : 'a list) : ('a list) = 
  (* Returns first n elements of the list. Raises Failure if the list is too short *)
  let rec take' n lst acc =
    match n with 
	| 0 -> List.rev acc
    | _ -> take' (n-1) (List.tl lst) (List.hd lst :: acc)
  in take' n lst []

let get_digits (grid : 'a option Model.grid) (constr : Model.constr) : 'a list =
  (* Get the digits that are already fixed in a constraint *)
  Model.unoption (List.map (fun (x, y) -> grid.(x).(y)) constr.cells)
  
let get_constr_options (state : state) (constr : Model.constr) : int list list =
  (* Get a list of options for each unfixed location for a constraint *)
  List.map 
    (fun o -> Model.unoption o.possible) 
	(List.filter (fun c -> (List.mem c.loc constr.cells) && state.current_grid.(fst c.loc).(snd c.loc) = None) state.options)

let all_digits = List.init 9 (fun x -> Some (x+1))

let max a b = if a > b then a else b
let min a b = if a < b then a else b
let sum = List.fold_left (+) 0


(* VVVV Get units of options VVVV *)

let get_opt_row (options : available list) (row_ind : int) = 
  List.filter (fun x -> fst (x.loc) = row_ind) options (* Returns n-th row of options (only unfixed positions) *)

let opt_rows (options : available list) = List.init 9 (get_opt_row options)  (* Returns a list of rows of options *)

let get_opt_column (options : available list) (col_ind : int) = 
  List.filter (fun x -> snd (x.loc) = col_ind) options (* Returns n-th column of options (only unfixed positions) *)

let opt_columns (options : available list) = List.init 9 (get_opt_column options)  (* Returns a list of columns of options *)

let get_opt_box (options : available list) (box_ind : int) =  (* Returns n-th box of options (only unfixed positions) *)
  List.filter (fun x -> 
    (fst (x.loc)) / 3 = box_ind / 3 &&
	(snd (x.loc)) / 3 = box_ind mod 3
  ) options

let opt_boxes (options : available list) = List.init 9 (get_opt_box options)  (* Returns a list of boxes of options *)

(* ^^^^ Get units of options ^^^^ *)

let remove_candidates (remove : int option list) (available : available) : available =
  (* Removes candidates from a location *)
  let rec rem r = function
    | [] -> []
	| h::t -> if List.mem h r then rem r t else h :: (rem r t)
  in
  {available with possible = rem remove available.possible}

(* VVVV n-tuple check VVVV *)

let find_tuple (options : available list) (opt : available) =
  (* Find whether a certain spot is a member of a n-tuple *)
  let same, diff = List.partition (fun x -> List.for_all (fun m -> List.mem m opt.possible) x.possible) options in
  if List.length same = List.length opt.possible then
    let diff = List.map (remove_candidates opt.possible) diff in
  same @ diff
  else
  options

let filter_tuples (options : available list) : available list =
  (* Filter all tuples out of the appropriate cells *)
  List.fold_left find_tuple options options

let clean_tuples (state : state) : state =
  (* Cleans tuples from the whole state *)
  {state with options = List.fold_left (fun opt func -> List.concat_map filter_tuples (func opt)) state.options [opt_rows; opt_columns; opt_boxes]}

(* ^^^^ n-tuple check ^^^^ *)

(* VVVV pointing pairs check VVVV *)

(* The following section checks for and removes pointing pairs, but as it slows down the code execution on average, it is not used *)

(* ---------------- UNUSED VVVV 
let find_pointing_pair_box_r digit opts = (* if X in a box can only be in one row, remove from elsewhere in row *)
  let contains, not_contains = List.partition (fun x -> List.mem digit x.possible) opts in
  if contains = [] then None else
  let rows = List.map (fun x -> fst x.loc) contains in
  let align_row = List.for_all ((=) (List.hd rows)) rows in
  if not align_row then None else
  Some ((List.hd rows), List.map (fun x -> snd x.loc) contains)  (* return row to clean, and columns of exceptions *)
  
let clean_pps_row digit data options =
  match data with
  | None -> options
  | Some (row, exceptions) ->
  let clean, leave = List.partition (fun opt -> (fst opt.loc) = row && not (List.mem (snd opt.loc) exceptions) ) options in
  let cleaned = List.map (remove_candidates [digit]) clean in
  cleaned @ leave
  
let find_pointing_pair_box_c digit opts = (* if X in a box can only be in one col, remove from elsewhere in col *)
  let contains, not_contains = List.partition (fun x -> List.mem digit x.possible) opts in
  if contains = [] then None else
  let cols = List.map (fun x -> snd x.loc) contains in
  let align_col = List.for_all ((=) (List.hd cols)) cols in
  if not align_col then None else
  Some ((List.hd cols), List.map (fun x -> fst x.loc) contains)  (* return col to clean, and rows of exceptions *)
  
let clean_pps_col digit data options =
  match data with
  | None -> options
  | Some (col, exceptions) ->
  let clean, leave = List.partition (fun opt -> (snd opt.loc) = col && not (List.mem (fst opt.loc) exceptions) ) options in
  let cleaned = List.map (remove_candidates [digit]) clean in
  cleaned @ leave
  
let clean_pps state =
  let options = state.options in
  let options = 
    List.fold_left
      (fun opts digit ->
        List.fold_left 
          (fun opt box -> clean_pps_row digit (find_pointing_pair_box_r digit box) opt) 
	      opts 
	      (opt_boxes opts)
      )
	  options
	  (generate_digits 9)
  in
  let options = 
    List.fold_left
      (fun opts digit ->
        List.fold_left 
          (fun opt box -> clean_pps_col digit (find_pointing_pair_box_c digit box) opt) 
          opts 
	      (opt_columns opts)
      )
	  options
	  (generate_digits 9)
  in
  {state with options = options}
---------------- UNUSED ^^^^ *)

(* ^^^^ pointing pairs check ^^^^ *)

(* VVVV hidden singles check VVVV *)

let find_hidden_single (digit : int option) (opts : available list) : available list =
  (* finds whether opts contains a hidden single *)
  let contains, not_contains = List.partition (fun x -> List.mem digit x.possible) opts in
  if List.length contains <> 1 then opts else
  let[@warning "-8"] [contains] = contains in
  [remove_candidates (List.filter ((<>) digit) all_digits) contains] @ not_contains

let find_hidden_singles_unit unit_fun (state : state) (digit : int option) : state =
  (* Finds hidden singles in a specific type of units (rows/colums/boxes) *)
  {state with options = List.concat_map (find_hidden_single digit) (unit_fun state.options)}
  
let find_hidden_singles (state : state) : state =
  (* Finds all hidden singles *)
  List.fold_left 
    (fun st func -> 
	  List.fold_left 
	    (find_hidden_singles_unit 
		  func 
		)
		st 
		all_digits
	)
	state 
	[opt_rows; opt_boxes; opt_columns]
	
	(*
    List.fold_left (fun st func -> List.fold_left (find_hidden_singles_unit func ) st (List.init 9 (fun x -> Some (x+1)))) state [opt_rows; opt_boxes; opt_columns]
	*)

(* ^^^^ hidden singles check ^^^^ *)

let precompute_sum = function (* all possible digits for a cage of a given length with a given sum *)
  | (2, 3) -> [1;2]
  | (2, 4) -> [1;3]
  | (2, 5) -> [1;2;3;4]
  | (2, 6) -> [1;2;4;5]
  | (2, 7) -> [1;2;3;4;5;6]
  | (2, 8) -> [1;2;3;5;6;7]
  | (2, 9) -> [1;2;3;4;5;6;7;8]
  | (2, 10) -> [1;2;3;4;6;7;8;9]
  | (2, 11) -> [2;3;4;5;6;7;8;9]
  | (2, 12) -> [3;4;5;7;8;9]
  | (2, 13) -> [4;5;6;7;8;9]
  | (2, 14) -> [5;6;8;9]
  | (2, 15) -> [6;7;8;9]
  | (2, 16) -> [7;9]
  | (2, 17) -> [8;9]
  | (3, 6) -> [1;2;3]
  | (3, 7) -> [1;2;4]
  | (3, 8) -> [1;2;3;4;5]
  | (3, 22) -> [5;6;7;8;9]
  | (3, 23) -> [6;8;9]
  | (3, 24) -> [7;8;9]
  | (4, 10) -> [1;2;3;4]
  | (4, 11) -> [1;2;3;5]
  | (4, 12) -> [1;2;3;4;5;6]
  | (4, 28) -> [4;5;6;7;8;9]
  | (4, 29) -> [5;7;8;9]
  | (4, 30) -> [6;7;8;9]
  | (5, 15) -> [1;2;3;4;5]
  | (5, 16) -> [1;2;3;4;6]
  | (5, 34) -> [4;6;7;8;9]
  | (5, 35) -> [5;6;7;8;9]
  | _ -> [1;2;3;4;5;6;7;8;9]

(* VVVV Innie / Outie check VVVV *)

(* The following section checks for and removes pokies (innies/outies), but as it slows down the code execution on average, it is not used *)

(* ---------------- UNUSED VVVV
let get_cages_row (constraints : Model.constr list) row =
  List.filter ( fun (x : Model.constr) ->
    (List.mem row (List.map fst x.cells)) && x.constr_type = "K"
  ) constraints

let get_uncaged_row grid cages row =
  let caged = List.concat_map (fun (c : Model.constr) -> c.cells) cages in
  Model.unoption (List.init 9 (fun col ->
    if List.mem (row,col) caged then None else
	  match grid.(row).(col) with
	  | Some x -> Some (col, x)
	  | None -> None
  ))
  
let get_pokies_row grid cages uncaged row =
  let caged = List.concat_map (fun (c : Model.constr) -> c.cells) cages in
  if List.length uncaged + List.length caged = 10 &&
     List.length (List.filter (fun x -> fst x <> row) caged) = 1 (* exacty one caged cell outside of the row *)
	 then (* outie *)
    let [outie] = List.filter (fun c -> fst c <> row) caged in
	let value = List.fold_left (fun a b -> a + snd b) 0 uncaged + List.fold_left (+) 0 (List.map (fun (x : Model.constr) -> x.value) cages) in
	Some (outie, Some (value - 45))
  else if List.length uncaged + List.length caged = 8 then (* innie *) 
    if List.exists (fun x -> fst x <> row) caged then None else
    let filled = (List.map fst uncaged) @ (List.map snd caged) in
	(* Printf.printf "Filled: %d\n" (List.length filled);
	List.iter (Printf.printf "%d-") filled;
	Printf.printf " in row %d\n" row; *)
	let [innie] = List.filter (fun c -> not (List.mem c filled)) (List.init 9 (fun x -> x)) in
	let value = List.fold_left (fun a b -> a + snd b) 0 uncaged + List.fold_left (+) 0 (List.map (fun (x : Model.constr) -> x.value) cages) in
	Some ((row, innie), Some (45 - value))
  else None
  
let fix_pokies_row state row =
  let cages = get_cages_row state.problem.constraints row in
  let uncaged = get_uncaged_row state.current_grid cages row in
  match get_pokies_row state.current_grid cages uncaged row with
  | Some (coords, value) -> {state with options = List.map (fun opt -> if opt.loc <> coords then opt else
      remove_candidates (
	    List.filter 
	    (fun x -> x <> value) 
		(List.init 9 (fun t -> Some (t+1)))
	  ) 
	  opt
    ) state.options}
  | None -> state

let fix_pokies_rows state =
  List.fold_left fix_pokies_row state (List.init 9 (fun x -> x))
  
(* -------------------------------- *)
  
let get_cages_col (constraints : Model.constr list) col =
  List.filter ( fun (x : Model.constr) ->
    (List.mem col (List.map snd x.cells)) && x.constr_type = "K"
  ) constraints

let get_uncaged_col grid cages col =
  let caged = List.concat_map (fun (c : Model.constr) -> c.cells) cages in
  Model.unoption (List.init 9 (fun row ->
    if List.mem (row,col) caged then None else
	  match grid.(row).(col) with
	  | Some x -> Some (row, x)
	  | None -> None
  ))
  
let get_pokies_col grid cages uncaged col =
  let caged = List.concat_map (fun (c : Model.constr) -> c.cells) cages in
  if List.length uncaged + List.length caged = 10 &&
     List.length (List.filter (fun x -> snd x <> col) caged) = 1 (* exacty one caged cell outside of the column *)
	 then (* outie *)
    let [outie] = List.filter (fun c -> snd c <> col) caged in
	let value = List.fold_left (fun a b -> a + snd b) 0 uncaged + List.fold_left (+) 0 (List.map (fun (x : Model.constr) -> x.value) cages) in
	Some (outie, Some (value - 45))
  else if List.length uncaged + List.length caged = 8 then (* innie *) 
    if List.exists (fun x -> snd x <> col) caged then None else
    let filled = (List.map fst uncaged) @ (List.map fst caged) in
	let [innie] = List.filter (fun c -> not (List.mem c filled)) (List.init 9 (fun x -> x)) in
	let value = List.fold_left (fun a b -> a + snd b) 0 uncaged + List.fold_left (+) 0 (List.map (fun (x : Model.constr) -> x.value) cages) in
	Some ((innie, col), Some (45 - value))
  else None
  
let fix_pokies_col state col =
  let cages = get_cages_col state.problem.constraints col in
  let uncaged = get_uncaged_col state.current_grid cages col in
  match get_pokies_col state.current_grid cages uncaged col with
  | Some (coords, value) -> {state with options = List.map (fun opt -> if opt.loc <> coords then opt else
      remove_candidates (
	    List.filter 
	    (fun x -> x <> value) 
		(List.init 9 (fun t -> Some (t+1)))
	  ) 
	  opt
    ) state.options}
  | None -> state

let fix_pokies_cols state =
  List.fold_left fix_pokies_col state (List.init 9 (fun x -> x))


let fix_pokies state = fix_pokies_cols (fix_pokies_rows state)
---------------- UNUSED ^^^^ *)
(* ^^^^ Innie / Outie check ^^^^ *)

(* VVVV Arrow check VVVV *)

let check_arrow_head state (arrow : Model.constr) (digit : int option) : bool =
  (* Check if a digit is possible to be placed on the head of the arrow *)
  let[@warning "-8"] head::tail = arrow.cells in
  let options = get_constr_options state {arrow with cells = tail} in
  let fixed = get_digits state.current_grid {arrow with cells = tail} in
  let fixed_sum = sum fixed in
  let min_opt = List.map (fun lst -> List.fold_left min (List.hd lst) lst) options in
  let max_opt = List.map (fun lst -> List.fold_left max (List.hd lst) lst) options in
  let min_sum = sum min_opt in
  let max_sum = sum max_opt in
  fixed_sum + min_sum <= Option.get digit && Option.get digit <= fixed_sum + max_sum
  
let check_arrow_tail state (arrow : Model.constr) (loc : int * int) (digit : int option) : bool =
  (* Check if a digit is possible to be placed on the tail of the arrow *)
  let[@warning "-8"] head::tail = arrow.cells in
  let fixed = get_digits state.current_grid {arrow with cells = tail} in
  let fixed_sum = sum fixed in
  let h, possible = match state.current_grid.(fst head).(snd head) with
  | Some x -> x, List.mem (Option.get digit) (precompute_sum ((List.length tail - List.length fixed), x - fixed_sum))
  | None -> let[@warning "-8"] [opts] = (List.filter (fun o -> o.loc = head) state.options) in
			let opts = Model.unoption opts.possible in 
			(List.fold_left max (List.hd opts) opts), true
  in
  possible && h >= Option.get digit + fixed_sum + (List.length tail - List.length fixed) - 1


let check_arrow (state : state) (arrow : Model.constr) : bool =
  (* Check if an arrow is solved correctly *)
  if arrow.constr_type <> "A" then true else
  let[@warning "-8"] head::tail = arrow.cells in
  let tail_digits = get_digits state.current_grid {arrow with cells = tail} in
  let tail_sum = (sum tail_digits) in
  match state.current_grid.(fst head).(snd head) with
  | None -> tail_sum <= 9
  | Some x -> if List.length tail = List.length tail_digits then tail_sum = x else tail_sum < x
  

(* ^^^^ Arrow check ^^^^ *)

(* VVVV Thermo check VVVV *)

let rec get_extreme (delta : int) = function
  (* Gets the most extreme digit in a certain direction that can be placed on a thermo *)
  | (Some a)::(Some b)::t -> get_extreme delta ((Some b)::t)
  | (Some a)::None::t -> get_extreme delta ((Some (a + delta))::t)
  | None::t -> get_extreme delta ((Some (if delta = (-1) then 9 else 1))::t)
  | (Some x)::[] -> x
  | [] -> failwith "Empty thermo?!?"

(* ^^^^ Thermo check ^^^^ *)

(* VVVV Constraint filtering VVVV *)

let is_possible_cage (cx : int) (cy : int) (state : state) (constr : Model.constr) (digit : int option) : bool =
  (* Check if a digit can be placed on a certain location within a cage *)
  if not (List.mem (cx, cy) constr.cells) then true else
  let num = (List.length constr.cells) - 1 in
  let already_filled = get_digits state.current_grid constr in
  let filled_sum = sum already_filled in
  if List.length already_filled = num then 
    Option.get digit = constr.value - filled_sum
  else
  let num = num - List.length already_filled and
      value = constr.value - filled_sum 
  in
  let below_max = (value - triangular num) >= Option.get digit in
  let already_in_cage = List.mem (Option.get digit) already_filled in
  let options = precompute_sum (num+1, value) in
  not already_in_cage && below_max && List.mem (Option.get digit) options
  
let[@warning "-8"] is_possible_arrow (cx : int) (cy : int) (state : state) (constr : Model.constr) (digit : int option) : bool =
  (* Check if a digit can be placed on a certain location within an arrow *)
  if not (List.mem (cx, cy) constr.cells) then true else
  let head::tail = constr.cells in
  if head = (cx, cy) then 
  check_arrow_head state constr digit else
  check_arrow_tail state constr (cx, cy) digit
  
let is_possible_thermo (cx : int) (cy : int) (state : state) (constr : Model.constr) (digit : int option) : bool =
  (* Check if a digit can be placed on a certain location within a thermo *)
  if not (List.mem (cx, cy) constr.cells) then true else
  let pos = find_pos (cx, cy) constr.cells in
  let digits = List.map (fun (x, y) -> state.current_grid.(x).(y)) constr.cells in
  let below = take pos digits and
      above = take (List.length digits - pos + 1) (List.rev digits) in
  let low = get_extreme 1 below in
  let high = get_extreme (-1) above in
  low <= Option.get digit && Option.get digit <= high

(* ------------------------ *)

let is_possible_constr (cx : int) (cy : int) (state : state) (constr : Model.constr) (digit : int option) =
  (* Check if a digit can be placed on a certain location within a constraint *)
  match constr.constr_type with
  | "K" -> is_possible_cage cx cy state constr digit
  | "A" -> is_possible_arrow cx cy state constr digit
  | "T" -> is_possible_thermo cx cy state constr digit
  | _ -> failwith "Unknown constraint"

(* ^^^^ Constraint filtering ^^^^ *)

let is_possible_digit (cx : int) (cy : int) (state : state) (constraints : Model.constr list) (digit : int option) : bool = 
  (* Check if a digit can be placed on a certain location *)
  if Array.mem digit (Model.get_row state.current_grid cx) ||
     Array.mem digit (Model.get_column state.current_grid cy) ||
	 Array.mem digit (Model.get_box state.current_grid (cx/3 * 3 + cy/3)) ||
	 not (List.for_all (fun x -> is_possible_constr cx cy state x digit) constraints) 
	 then false
	 else true

let get_options (grid : int option Model.grid) (constraints : Model.constr list) : available list =
  (* Prepares options for all empty cells *)
  Model.unoption (List.flatten (List.init 9 (fun x -> 
	List.init 9 (fun y -> 
	  match grid.(x).(y) with
	  | Some x -> None
	  | None -> Some {loc = (x, y); possible = all_digits}
	)
  )))

let initialize_state (problem : Model.problem) : state =
  { current_grid = Model.copy_grid problem.initial_grid; options = get_options problem.initial_grid problem.constraints; problem }

let validate_state (state : state) : response =
  let unsolved =
    Array.exists (Array.exists Option.is_none) state.current_grid
  in
  if not (List.for_all (check_arrow state) state.problem.constraints) then Fail state else
  if unsolved then Unsolved state
  else
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = Model.map_grid Option.get state.current_grid in
    if Model.is_valid_solution state.problem solution then Solved solution
    else Fail state
	
let get_possible (state : state) (cx : int) (cy : int) : int option list =
  (* Get a list of options for a position *)
  let rec aux = function
	| [] -> []
	| av::rem -> (match (av.loc) with
			  | l when l = (cx, cy) -> av.possible
			  | _ -> aux rem
			)
  in
  aux state.options

let step_cell (state : state) (cx : int) (cy : int) cell =
  (* Advances the cell: if it has only one option it fixes it, if there are no more options, raises No_Options *) 
  match state.current_grid.(cx).(cy) with
  | Some x -> ()
  | None -> match get_possible state cx cy with
			| [] -> raise No_Options
			| [x] -> state.current_grid.(cx).(cy) <- x
			| _ -> ()


let filter_state (state : state) : state =
  (* Removes all options that are not valid anymore *)
  let filter_option state (available : available) =
    match state.current_grid.(fst available.loc).(snd available.loc) with
    | None -> Some {available with possible = List.filter (
	      is_possible_digit (fst available.loc) (snd available.loc) state state.problem.constraints
	    ) available.possible
      } 
    | Some x -> None
  in
  let new_state = {state with options = Model.unoption (List.map (filter_option state) state.options)} in
  new_state


let[@warning "-8"] branch_state (state : state) : (state * state) option =
  let options = List.sort (fun opt1 opt2 -> List.compare_lengths opt1.possible opt2.possible) state.options in
  let chng :: options = options in
  let {loc = loc; possible = possible} = chng in
  match possible with
    | [] -> None
	| hyp_true :: hyp_false -> Some (
	{state with current_grid = Model.copy_grid state.current_grid; options = {loc = loc; possible = [hyp_true]} :: options}, 
	{state with current_grid = Model.copy_grid state.current_grid; options = {loc = loc; possible = hyp_false} :: options}
	)


let compare_grid (state : state) : unit = 
  (* If all entered digits in a grid match, prints out the state. *)
  let solution = [|
    [| 4;6;7;8;1;3;5;9;2 |];
    [| 3;8;5;7;2;9;1;6;4 |];
    [| 1;2;9;4;5;6;7;3;8 |];
    [| 7;1;6;5;3;8;2;4;9 |];
    [| 8;9;4;2;6;1;3;5;7 |];
    [| 2;5;3;9;4;7;8;1;6 |];
    [| 6;4;8;3;7;5;9;2;1 |];
    [| 5;7;2;1;9;4;6;8;3 |];
    [| 9;3;1;6;8;2;4;7;5 |];
    |] in
  if Array.for_all2 (fun sol cur ->
    Array.for_all2 (fun s c ->
	  match c with
	  | None -> true
	  | Some x -> s = x
	) sol cur
  ) solution state.current_grid then print_state state


(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  let pass = 
    try
	  Some (Model.mapi_grid (step_cell state) state.current_grid)
    with
      No_Options -> (* compare_grid state; *) None  (* If a correct grid is rejected, print out the state *)
  in
  if pass = None then None else

  (* let state = filter_state state in
  let state = find_hidden_singles state in
  let state = clean_tuples state in *)
  let state = state 
    |> filter_state 
	|> find_hidden_singles 
	|> clean_tuples 
	(* |> clean_pps *) (* SLOWS DOWN ON AVERAGE *)
	(* |> fix_pokies *) (* SLOWS DOWN ON AVERAGE *)
  in
  (* TODO: na tej točki je stanje smiselno počistiti in zožiti možne rešitve *)
  match validate_state state with
  | Solved solution ->
      (* če smo našli rešitev, končamo *)
      Some solution
  | Fail fail ->
      (* prav tako končamo, če smo odkrili, da rešitev ni *)
      None
  | Unsolved state' ->
      (* če še nismo končali, raziščemo stanje, v katerem smo končali *)
      explore_state state'

and explore_state (state : state) =
  (* pri raziskovanju najprej pogledamo, ali lahko trenutno stanje razvejimo *)
  match branch_state state with
  | None ->
      (* če stanja ne moremo razvejiti, ga ne moremo raziskati *)
      None
  | Some (st1, st2) -> (
      (* če stanje lahko razvejimo na dve možnosti, poizkusimo prvo *)
      match solve_state st1 with
      | Some solution ->
          (* če prva možnost vodi do rešitve, do nje vodi tudi prvotno stanje *)
          Some solution
      | None ->
          (* če prva možnost ne vodi do rešitve, raziščemo še drugo možnost *)
          solve_state st2 )

let solve_problem (problem : Model.problem) =
  problem |> initialize_state |> solve_state
