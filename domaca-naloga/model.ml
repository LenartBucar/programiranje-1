(* Pomožni tip, ki predstavlja mrežo *)

type 'a grid = 'a Array.t Array.t

(* Funkcije za prikaz mreže.
   Te definiramo najprej, da si lahko z njimi pomagamo pri iskanju napak. *)

(* Razbije seznam [lst] v seznam seznamov dolžine [size] *)
let chunkify size lst =
  let rec aux chunk chunks n lst =
    match (n, lst) with
    | _, [] when chunk = [] -> List.rev chunks
    | _, [] -> List.rev (List.rev chunk :: chunks)
    | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
    | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
  in
  aux [] [] size lst

let string_of_list string_of_element sep lst =
  lst |> List.map string_of_element |> String.concat sep

let string_of_nested_list string_of_element inner_sep outer_sep =
  string_of_list (string_of_list string_of_element inner_sep) outer_sep

let string_of_row string_of_cell row =
  let string_of_cells =
    row |> Array.to_list |> chunkify 3
    |> string_of_nested_list string_of_cell "" "│"
  in
  "┃" ^ string_of_cells ^ "┃\n"

let int_of_char c =
  Char.code c - Char.code '0'

let print_grid string_of_cell grid =
  let ln = "───" in
  let big = "━━━" in
  let divider = "┠" ^ ln ^ "┼" ^ ln ^ "┼" ^ ln ^ "┨\n" in
  let row_blocks =
    grid |> Array.to_list |> chunkify 3
    |> string_of_nested_list (string_of_row string_of_cell) "" divider
  in
  Printf.printf "┏%s┯%s┯%s┓\n" big big big;
  Printf.printf "%s" row_blocks;
  Printf.printf "┗%s┷%s┷%s┛\n" big big big

(* Funkcije za dostopanje do elementov mreže *)

let get_row (grid : 'a grid) (row_ind : int) = grid.(row_ind)  (* Returns n-th row *)

let rows grid = List.init 9 (get_row grid)  (* Returns a list of rows *)

let get_column (grid : 'a grid) (col_ind : int) =  (* Returns n-th column *)
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind))

let columns grid = List.init 9 (get_column grid)  (* Returns a list of columns *)

let get_box (grid : 'a grid) (box_ind : int) =  (* Returns n-th box *)
  let row pos_ind = (box_ind / 3) * 3 + pos_ind / 3 in
  let col pos_ind = (box_ind mod 3) * 3 + pos_ind mod 3 in
  Array.init 9 (fun p -> grid.(row p).(col p))

let boxes grid = List.init 9 (get_box grid)  (* Returns a list of boxes *)

(* Funkcije za ustvarjanje novih mrež *)

let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid = 
  Array.init 9 (fun row -> get_row grid row |> Array.map f)
  
let mapi_grid (f : int -> int -> 'a -> 'b) (grid : 'a grid) : 'b grid = 
  Array.init 9 (fun row -> get_row grid row |> Array.mapi (f row))

let copy_grid (grid : 'a grid) : 'a grid = map_grid (fun x -> x) grid

let foldi_grid (f : int -> int -> 'a -> 'acc -> 'acc) (grid : 'a grid)
    (acc : 'acc) : 'acc =
  let acc, _ =
    Array.fold_left
      (fun (acc, row_ind) row ->
        let acc, _ =
          Array.fold_left
            (fun (acc, col_ind) cell ->
              (f row_ind col_ind cell acc, col_ind + 1))
            (acc, 0) row
        in
        (acc, row_ind + 1))
      (acc, 0) grid
  in
  acc

let row_of_string cell_of_char str =
  List.init (String.length str) (String.get str) |> List.filter_map cell_of_char

let grid_of_string cell_of_char str =
  let grid =
    str |> String.split_on_char '\n'
    |> List.map (row_of_string cell_of_char)
    |> List.filter (function [] -> false | _ -> true)
    |> List.map Array.of_list |> Array.of_list
  in
  if Array.length grid <> 9 then failwith "Nepravilno število vrstic";
  if Array.exists (fun x -> x <> 9) (Array.map Array.length grid) then
    failwith "Nepravilno število stolpcev";
  grid

(* Model za vhodne probleme *)

type constr = {constr_type : string; value : int; cells : (int * int) list}

type problem = { initial_grid : int option grid; constraints : constr list }

let string_of_cell = function
  | Some c -> string_of_int c
  | _ -> " "

let rec split_list l = function
  | 0 -> [], l
  | n -> match l with
		 | h :: t -> let f, s = split_list t (n-1) in h::f, s
		 | [] -> [], []

let split_inp str = 
  let grid, cons = split_list (String.split_on_char '\n' str) 13 in
  let grid = String.concat "\n" grid in
  match cons with
  | [] -> grid, []
  | h :: t -> grid, t
  

let print_problem problem : unit = print_grid string_of_cell problem.initial_grid

let make_cells cells =
  let cells = String.split_on_char ';' cells in
  List.map (fun x -> int_of_char x.[1], int_of_char x.[3]) cells

(* VVVV CONSTRAINT PARSERS VVVV *)

let[@warning "-8"] make_cage c =
  let [v; cells] = (String.split_on_char ' ' c) in
  let v = int_of_string v in
  let cells = make_cells cells in
  {constr_type = "K"; value = v; cells = cells}
  
let make_arrow c =
  let s = make_cells (String.sub c 0 5) in
  let cells = make_cells (String.sub c 9 (String.length c - 9)) in
  {constr_type = "A"; value = (-1); cells = s@cells}
  
let make_thermo c =
  let cells = make_cells c in
  {constr_type = "T"; value = (-1); cells = cells}

(* ^^^^ CONSTRAINT PARSERS ^^^^ *)

let parse_constraint constr =
  let t = constr.[0] and
	  c = String.sub constr 3 (String.length constr - 3)
  in
  match t with
  | 'K' -> make_cage c
  | 'A' -> make_arrow c
  | 'T' -> make_thermo c
  | _ -> failwith "Unknown constraint"

let parse_constraints constraints =
  List.map parse_constraint (List.filter (fun x -> x.[0] <> '#') constraints)

let problem_of_string str =
  let cell_of_char = function
    | ' ' -> Some None
    | c when '1' <= c && c <= '9' -> Some (Some (Char.code c - Char.code '0'))
    | _ -> None
  in
  let grid_str, constraint_str = split_inp str in
  { initial_grid = grid_of_string cell_of_char grid_str; constraints = parse_constraints constraint_str }

(* Model za izhodne rešitve *)

let check_unique arr =
  let solved = Array.init 9 (fun x -> x+1) in
  Array.for_all (fun x -> Array.mem x solved) arr

type solution = int grid

let print_solution solution = print_grid string_of_int solution

let rec duplicates = function
  | [] -> false
  | hd::tl -> List.mem hd tl || duplicates tl

let validate_cage soln constr = 
  let values = List.map (fun (x, y) -> soln.(x).(y)) constr.cells in
  (* Printf.printf "Validating cage with value %d\n" constr.value; *)
  (* List.iter (Printf.printf "%d-") values; *)
  (* Printf.printf "\n"; *)
  let sum = (List.fold_left (+) 0 values) in
  (* Printf.printf "Sum of digits is %d\n" sum; *)
  let dupes = duplicates values in
  (sum = constr.value) && not dupes
  
  
let[@warning "-8"] validate_arrow soln constr = 
  let head::tail = List.map (fun (x, y) -> soln.(x).(y)) constr.cells in
  head = (List.fold_left (+) 0 tail)
  
let validate_thermo soln constr = 
  let rec validate (soln : int grid) = function
    | h::s::t -> soln.(fst h).(snd h) < soln.(fst s).(snd s) && validate soln (s::t)
	| _ -> true
  in
  validate soln constr.cells
  
let validate_constraint soln constr =
  match constr.constr_type with
  | "K" -> validate_cage soln constr
  | "A" -> validate_arrow soln constr
  | "T" -> validate_thermo soln constr
  | _ -> failwith "Unknown constraint"

let is_valid_solution problem solution = (* TODO: Validate constraints *)
  List.for_all check_unique (rows solution) &&
  List.for_all check_unique (columns solution) &&
  List.for_all check_unique (boxes solution) &&
  List.for_all (validate_constraint solution) problem.constraints
  
  
let rec unoption (data : 'a option list) : 'a list =
  match data with
  | [] -> []
  | h::t -> (match h with
			 | Some x -> x :: unoption t
			 | None -> unoption t
			)