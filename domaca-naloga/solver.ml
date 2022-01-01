type available = { loc : int * int; possible : int option list }
exception No_Options

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
type state = { problem : Model.problem; current_grid : int option Model.grid; options : available list}

let print_available available =
  Printf.printf "(%d,%d) - " (fst available.loc) (snd available.loc);
  List.iter (fun x -> Printf.printf "%d " (Option.get x)) available.possible;
  Printf.printf "\n"

let print_constraint (constr : Model.constr) =
  Printf.printf "%s - %d - " constr.constr_type constr.value;
  List.iter (fun x -> Printf.printf "(%d,%d)" (fst x) (snd x)) constr.cells;
  Printf.printf "\n"

let print_state (state : state) : unit =
  Model.print_grid
    (function None -> " " | Some digit -> string_of_int digit)
    state.current_grid;
  (* List.iter print_constraint state.problem.constraints; *)
  List.iter print_available state.options
  
let print_fail_state (state : state) : unit =
  Model.print_grid
    (function None -> " " | Some digit -> string_of_int digit)
    state.current_grid;
  (* List.iter (fun x -> if List.length x.possible = 0 then Printf.printf "(%d,%d)\n" (fst x.loc) (snd x.loc)) state.options *)
  List.iter print_available state.options

type response = Solved of Model.solution | Unsolved of state | Fail of state

let triangular n = (n * (n+1)) / 2

let rec find_pos n el = function
  | [] -> -1
  | h::t when h = el -> n
  | _::t -> find_pos (n+1) el t

let get_digits grid (constr : Model.constr) =
  Model.unoption (List.map (fun (x, y) -> grid.(x).(y)) constr.cells)
  
let get_constr_options state (constr : Model.constr) =
  List.map 
    (fun o -> Model.unoption o.possible) 
	(List.filter (fun c -> (List.mem c.loc constr.cells) && state.current_grid.(fst c.loc).(snd c.loc) = None) state.options)

(* VVVV Get units of options VVVV *)

let get_opt_row (options : available list) (row_ind : int) = 
  List.filter (fun x -> fst (x.loc) = row_ind) options (* Returns n-th row of options (only unfixed positions) *)

let opt_rows options = List.init 9 (get_opt_row options)  (* Returns a list of rows *)

let get_opt_column (options : available list) (col_ind : int) = 
  List.filter (fun x -> snd (x.loc) = col_ind) options (* Returns n-th column of options (only unfixed positions) *)

let opt_columns options = List.init 9 (get_opt_column options)  (* Returns a list of columns *)

let get_opt_box (options : available list) (box_ind : int) =  (* Returns n-th box *)
  List.filter (fun x -> 
    (fst (x.loc)) / 3 = box_ind / 3 &&
	(snd (x.loc)) / 3 = box_ind mod 3
  ) options

let opt_boxes options = List.init 9 (get_opt_box options)  (* Returns a list of boxes *)

(* ^^^^ Get units of options ^^^^ *)

let remove_candidates remove available =
  let rec rem r = function
    | [] -> []
	| h::t -> if List.mem h r then rem r t else h :: (rem r t)
  in
  {available with possible = rem remove available.possible}

(* VVVV n-tuple check VVVV *)

let find_tuple (options : available list) opt =
  let same, diff = List.partition (fun x -> x.possible = opt.possible) options in
  if List.length same = List.length opt.possible then
    let diff = List.map (remove_candidates opt.possible) diff in
    same @ diff
  else
  options

let filter_tuples (options : available list) =
  List.fold_left find_tuple options options

let clean_tuples (options : available list) =
  let options = List.concat_map filter_tuples (opt_rows options) in
  let options = List.concat_map filter_tuples (opt_columns options) in
  let options = List.concat_map filter_tuples (opt_boxes options) in
  options

(* ^^^^ n-tuple check ^^^^ *)

(* VVVV pointing pairs check VVVV *)


(* 

if X in a row can only be in one box, remove from elsewhere in box
if X in a col can only be in one box, remove from elsewhere in box
if X in a box can only be in one row, remove from elsewhere in row
if X in a box can only be in one col, remove from elsewhere in col

*)

(* ^^^^ pointing pairs check ^^^^ *)

(* VVVV hidden singles check VVVV *)

let find_hidden_single digit opts =
  let contains, not_contains = List.partition (fun x -> List.mem digit x.possible) opts in
  if List.length contains <> 1 then opts else
  let[@warning "-8"] [contains] = contains in
  [remove_candidates (List.filter ((<>) digit) (List.init 9 (fun x -> Some (x+1)))) contains] @ not_contains

let find_hidden_singles_unit unit_fun state digit =
  {state with options = List.concat_map (find_hidden_single digit) (unit_fun state.options)}
  
let find_hidden_singles state =
  List.fold_left 
    (fun st func -> 
	  List.fold_left 
	    (find_hidden_singles_unit 
		  func 
		)
		st 
		(List.init 9 (fun x -> Some (x+1)))
	)
	state 
	[opt_rows; opt_boxes; opt_columns]
	
	(*
    List.fold_left (fun st func -> List.fold_left (find_hidden_singles_unit func ) st (List.init 9 (fun x -> Some (x+1)))) state [opt_rows; opt_boxes; opt_columns]
	*)

(* ^^^^ hidden singles check ^^^^ *)

(* VVVV Innie / Outie check VVVV *)

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

(* ^^^^ Innie / Outie check ^^^^ *)

(* VVVV Arrow check VVVV *)

let check_arrow_head state (arrow : Model.constr) digit =
  let[@warning "-8"] head::tail = arrow.cells in
  let options = get_constr_options state {arrow with cells = tail} in
  let fixed = get_digits state.current_grid {arrow with cells = tail} in
  let fixed_sum = List.fold_left (+) 0 fixed in
  let min_opt = List.map (fun lst -> List.fold_left (fun a b -> if a < b then a else b) (List.hd lst) lst) options in
  let max_opt = List.map (fun lst -> List.fold_left (fun a b -> if a > b then a else b) (List.hd lst) lst) options in
  let min_sum = List.fold_left (+) 0 min_opt in
  let max_sum = List.fold_left (+) 0 max_opt in
  fixed_sum + min_sum <= Option.get digit && Option.get digit <= fixed_sum + max_sum
  (* fixed_sum + (List.length tail - List.length fixed) <= Option.get digit *)
  
let check_arrow_tail state (arrow : Model.constr) loc digit =
  let[@warning "-8"] head::tail = arrow.cells in
  let h = match state.current_grid.(fst head).(snd head) with
  | Some x -> x
  | None -> let[@warning "-8"] [opts] = (List.filter (fun o -> o.loc = head) state.options) in
			let opts = Model.unoption opts.possible in 
			List.fold_left (fun a b -> if a > b then a else b) (List.hd opts) opts
  in
  h >= Option.get digit + List.length tail - 1


let check_arrow state (arrow : Model.constr) =
  if arrow.constr_type <> "A" then true else
  let[@warning "-8"] head::tail = arrow.cells in
  let tail_digits = get_digits state.current_grid {arrow with cells = tail} in
  let tail_sum = (List.fold_left (+) 0 tail_digits) in
  match state.current_grid.(fst head).(snd head) with
  | None -> tail_sum <= 9
  | Some x -> if List.length tail = List.length tail_digits then tail_sum = x else tail_sum < x
  

(* ^^^^ Arrow check ^^^^ *)

(* VVVV Constraint filtering VVVV *)

let precompute_cage = function (* all possible digits for a cage of a given length with a given sum *)
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

let is_possible_cage cx cy state (constr : Model.constr) digit = (* make cages work *)
  if not (List.mem (cx, cy) constr.cells) then true else
  let num = (List.length constr.cells) - 1 in
  (* Printf.printf "Cell (%d,%d) - cage size %d, cage value %d. Max digit: %d\n" cx cy (num + 1) constr.value (constr.value - triangular num); *)
  let below_max = (constr.value - triangular num) >= Option.get digit in
  let already_filled = get_digits state.current_grid constr in
  if List.length already_filled = num then 
    (Option.get digit = constr.value - (List.fold_left (+) 0 already_filled))
  else
  let already_in_cage = List.mem (Option.get digit) already_filled in
  let options = precompute_cage (num+1, constr.value) in
  List.mem (Option.get digit) options && not already_in_cage && below_max
  
let[@warning "-8"] is_possible_arrow cx cy state (constr : Model.constr) digit =
  if not (List.mem (cx, cy) constr.cells) then true else
  let head::tail = constr.cells in
  if head = (cx, cy) then check_arrow_head state constr digit else
  check_arrow_tail state constr (cx, cy) digit
  
let is_possible_thermo cx cy state (constr : Model.constr) digit =
  if not (List.mem (cx, cy) constr.cells) then true else
  let pos = find_pos 1 (cx, cy) constr.cells in
  pos <= Option.get digit && Option.get digit <= (9 - (List.length constr.cells - pos))

let is_possible_constr cx cy state (constr : Model.constr) digit =
  match constr.constr_type with
  | "K" -> is_possible_cage cx cy state constr digit
  | "A" -> is_possible_arrow cx cy state constr digit
  | "T" -> is_possible_thermo cx cy state constr digit
  | _ -> failwith "Unknown constraint"

(* ^^^^ Get units of options ^^^^ *)

let is_possible_digit cx cy state (constraints : Model.constr list) digit = (* TODO: restrict constraints *)
  if Array.mem digit (Model.get_row state.current_grid cx) ||
     Array.mem digit (Model.get_column state.current_grid cy) ||
	 Array.mem digit (Model.get_box state.current_grid (cx/3 * 3 + cy/3)) ||
	 not (List.for_all (fun x -> is_possible_constr cx cy state x digit) constraints) then 
	 false else
	 true


(* let rec filter_options cx cy grid (constraints : Model.constr list) = function (* reduces options available for a given cell *)
  | [] -> []
  | x::t -> if is_possible_digit cx cy grid constraints x then x :: (filter_options cx cy grid constraints t) else filter_options cx cy grid constraints t *)

let get_options grid (constraints : Model.constr list) =
  Model.unoption (List.flatten (List.init 9 (fun x -> 
	List.init 9 (fun y -> 
	  match grid.(x).(y) with
	  | Some x -> None
	  | None -> Some {loc = (x, y); possible = (List.init 9 (fun x-> Some (x+1)))}
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
	
let get_possible state cx cy =
  let rec aux = function
	| [] -> []
	| av::rem -> (match (av.loc) with
			  | l when l = (cx, cy) -> av.possible
			  | _ -> aux rem
			)
  in
  aux state.options

let step_cell state cx cy cell = 
  match state.current_grid.(cx).(cy) with
  | Some x -> ()
  | None -> match get_possible state cx cy with
			| [] -> raise No_Options
			| [x] -> state.current_grid.(cx).(cy) <- x;
			| _ -> () (* TODO: MULTIPLE OPTIONS *)


let filter_state state =
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


let[@warning "-8"] branch_state (state : state) : (state * state) option =  (* TODO when can we not branch the state?*)
  let options = List.sort (fun opt1 opt2 -> List.compare_lengths opt1.possible opt2.possible) state.options in
  let chng :: options = options in
  let {loc = loc; possible = possible} = chng in
  match possible with
    | [] -> None
	| hyp_true :: hyp_false -> Some (
	{state with current_grid = Model.copy_grid state.current_grid; options = {loc = loc; possible = [hyp_true]} :: options}, 
	{state with current_grid = Model.copy_grid state.current_grid; options = {loc = loc; possible = hyp_false} :: options}
	)


let compare_grid state = 
  let solution = [|
    [| 2;6;9;3;4;1;8;7;5 |];
    [| 5;7;1;6;2;8;3;9;4 |];
    [| 4;8;3;9;7;5;2;6;1 |];
    [| 1;3;6;4;8;7;9;5;2 |];
    [| 9;4;5;2;1;6;7;3;8 |];
    [| 8;2;7;5;9;3;4;1;6 |];
    [| 3;1;4;8;6;9;5;2;7 |];
    [| 6;9;2;7;5;4;1;8;3 |];
    [| 7;5;8;1;3;2;6;4;9 |];
    |] in
  if Array.for_all2 (fun sol cur ->
    Array.for_all2 (fun s c ->
	  match c with
	  | None -> true
	  | Some x -> s = x
	) sol cur
  ) solution state.current_grid then print_fail_state state


(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =
  (* print_state state; *)
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  let pass = 
    try
	  Some (Model.mapi_grid (step_cell state) state.current_grid)
    with
      No_Options -> compare_grid state; None
  in
  if pass = None then None else

  let state = filter_state state in
  let state = find_hidden_singles state in
  let state = {state with options = clean_tuples state.options} in
  (* let state = fix_pokies state in *)
  (* print_state state; *)
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
