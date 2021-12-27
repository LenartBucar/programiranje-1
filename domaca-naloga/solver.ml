type available = { loc : int * int; possible : int option list }
exception No_Options

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
type state = { problem : Model.problem; current_grid : int option Model.grid; options : available list}

let print_available available =
  Printf.printf "(%d,%d) - " (fst available.loc) (snd available.loc);
  List.iter (fun x -> Printf.printf "%d " (Option.get x)) available.possible;
  Printf.printf "\n"

let print_state (state : state) : unit =
  Model.print_grid
    (function None -> " " | Some digit -> string_of_int digit)
    state.current_grid;
  (* List.iter print_available state.options *)

type response = Solved of Model.solution | Unsolved of state | Fail of state


let is_possible_digit cx cy grid digit =
  if Array.mem digit (Model.get_row grid cx) ||
     Array.mem digit (Model.get_column grid cy) ||
	 Array.mem digit (Model.get_box grid (cx/3 * 3 + cy/3)) then false else true

let rec filter_options cx cy grid = function (* reduces options available for a given cell *)
  | [] -> []
  | x::t -> if is_possible_digit cx cy grid x then x :: (filter_options cx cy grid t) else filter_options cx cy grid t

let get_options grid =
  Model.unoption (List.flatten (List.init 9 (fun x -> 
	List.init 9 (fun y -> 
	  match grid.(x).(y) with
	  | Some x -> None
	  | None -> Some {loc = (x, y); possible = filter_options x y grid (List.init 9 (fun x-> Some (x+1)))}
	)
  )))

let initialize_state (problem : Model.problem) : state =
  { current_grid = Model.copy_grid problem.initial_grid; options = get_options problem.initial_grid; problem }

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
	  if grid.(fst available.loc).(snd available.loc) = None then Some {available with possible = List.filter (is_possible_digit (fst available.loc) (snd available.loc) grid) available.possible} else None
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

(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  let pass = 
    try
	  Some (Model.mapi_grid (step_cell state) state.current_grid)
    with
      No_Options -> None
  in
  if pass = None then None else

  let state = filter_state state in
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
