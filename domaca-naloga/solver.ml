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
  List.iter print_constraint state.problem.constraints;
  List.iter print_available state.options
  
let print_fail_state (state : state) : unit =
  Model.print_grid
    (function None -> " " | Some digit -> string_of_int digit)
    state.current_grid;
  List.iter (fun x -> if List.length x.possible = 0 then Printf.printf "(%d,%d)\n" (fst x.loc) (snd x.loc)) state.options

type response = Solved of Model.solution | Unsolved of state | Fail of state

let triangular n = (n * (n+1)) / 2

let rec find_pos n el = function
  | [] -> -1
  | h::t when h = el -> n
  | _::t -> find_pos (n+1) el t

let get_digits grid (constr : Model.constr) =
  Model.unoption (List.map (fun (x, y) -> grid.(x).(y)) constr.cells)




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

let remove_candidates remove available =
  let rec rem r = function
    | [] -> []
	| h::t -> if List.mem h r then rem r t else h :: (rem r t)
  in
  {available with possible = rem remove available.possible}

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
  | _ -> [1;2;3;4;5;6;7;8;9]

let is_possible_cage cx cy grid (constr : Model.constr) digit = (* make cages work *)
  if not (List.mem (cx, cy) constr.cells) then true else
  let num = (List.length constr.cells) - 1 in
  (* Printf.printf "Cell (%d,%d) - cage size %d, cage value %d. Max digit: %d\n" cx cy (num + 1) constr.value (constr.value - triangular num); *)
  let below_max = (constr.value - triangular num) >= Option.get digit in
  let already_filled = get_digits grid constr in
  if List.length already_filled = num then 
    (Option.get digit = constr.value - (List.fold_left (+) 0 already_filled))
  else
  let already_in_cage = List.mem (Option.get digit) already_filled in
  let options = precompute_cage (num+1, constr.value) in
  List.mem (Option.get digit) options && not already_in_cage && below_max
  

let[@warning "-8"] is_possible_arrow cx cy grid (constr : Model.constr) digit =
  if not (List.mem (cx, cy) constr.cells) then true else
  let head::tail = constr.cells in
  if head = (cx, cy) then Option.get digit >= List.length tail else
  Option.get digit <= (10 - (List.length tail))
  
  
let is_possible_thermo cx cy grid (constr : Model.constr) digit =
  (* Printf.printf "Checking thermo constraint on (%d,%d) - digit %d\n" cx cy (Option.get digit); *)
  if not (List.mem (cx, cy) constr.cells) then true else
  let pos = find_pos 1 (cx, cy) constr.cells in
  pos <= Option.get digit && Option.get digit <= (9 - (List.length constr.cells - pos))

let is_possible_constr cx cy grid (constr : Model.constr) digit =
  match constr.constr_type with
  | "K" -> is_possible_cage cx cy grid constr digit
  | "A" -> is_possible_arrow cx cy grid constr digit
  | "T" -> is_possible_thermo cx cy grid constr digit
  | _ -> failwith "Unknown constraint"

let is_possible_digit cx cy grid (constraints : Model.constr list) digit = (* TODO: restrict constraints *)
  if Array.mem digit (Model.get_row grid cx) ||
     Array.mem digit (Model.get_column grid cy) ||
	 Array.mem digit (Model.get_box grid (cx/3 * 3 + cy/3)) ||
	 not (List.for_all (fun x -> is_possible_constr cx cy grid x digit) constraints) then false else true


let rec filter_options cx cy grid (constraints : Model.constr list) = function (* reduces options available for a given cell *)
  | [] -> []
  | x::t -> if is_possible_digit cx cy grid constraints x then x :: (filter_options cx cy grid constraints t) else filter_options cx cy grid constraints t

let get_options grid (constraints : Model.constr list) =
  Model.unoption (List.flatten (List.init 9 (fun x -> 
	List.init 9 (fun y -> 
	  match grid.(x).(y) with
	  | Some x -> None
	  | None -> Some {loc = (x, y); possible = filter_options x y grid constraints (List.init 9 (fun x-> Some (x+1)))}
	)
  )))

let initialize_state (problem : Model.problem) : state =
  { current_grid = Model.copy_grid problem.initial_grid; options = get_options problem.initial_grid problem.constraints; problem }

let validate_state (state : state) : response =
  let unsolved =
    Array.exists (Array.exists Option.is_none) state.current_grid
  in
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
			| [x] -> state.current_grid.(cx).(cy) <- x
			| _ -> () (* TODO: MULTIPLE OPTIONS *)


let filter_state state =
	let filter_option grid (available : available) =
	  if grid.(fst available.loc).(snd available.loc) = None then Some {available with possible = List.filter (is_possible_digit (fst available.loc) (snd available.loc) grid state.problem.constraints) available.possible} else None
	in
	{state with options = Model.unoption (List.map (filter_option state.current_grid) state.options)}


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
  (* print_state state;*)
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  let pass = 
    try
	  Some (Model.mapi_grid (step_cell state) state.current_grid)
    with
      No_Options -> compare_grid state; None
  in
  if pass = None then None else

  let state = filter_state state in
  let state = {state with options = clean_tuples state.options} in
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
