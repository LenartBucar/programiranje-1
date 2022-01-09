let read_problem filename =
  let channel = open_in_bin filename in
  let str = really_input_string channel (in_channel_length channel) in
  close_in channel;
  Model.problem_of_string str

let find_solution problem =
  let before = Sys.time () in
  let solution = Solver.solve_problem problem in
  let after = Sys.time () in
  let elapsed_time = after -. before in
  (solution, elapsed_time)

let display_solution = function
  | Some solution ->
      Printf.printf "Končna rešitev:\n";
      Model.print_solution solution
  | None -> Printf.printf "Rešitev ne obstaja.\n"

let find_and_display_solution (problem : Model.problem) =
  Printf.printf "Rešujem:\n";
  Model.print_problem problem;
  Printf.printf "\n%!";
  let response, elapsed_time = find_solution problem in
  display_solution response;
  Printf.printf "Čas reševanja: %f s.\n%!" elapsed_time

let find_solutions (problems : Model.problem list) =
  let before = Sys.time () in
  let _ = List.iteri 
			(fun i x -> 
				match (Solver.solve_problem x) with
				| Some _ -> ()
				| None -> Printf.printf "%d\n" i; Model.print_problem x;
			)
			problems in
  let after = Sys.time () in
  let elapsed_time = after -. before in
  Printf.printf "Čas reševanja: %f s.\n%!" elapsed_time

let () =
  (* Če se program sesuje, nam to izpiše klicni sklad. *)
  Printexc.record_backtrace true;
  (* Tabela sistemskih argumentov vsebuje ime klicanega programa ter argumente, ki mu sledijo *)
  Sys.argv
  (* Tabelo pretvorimo v seznam *)
  |> Array.to_list
  (* Odstranimo prvi element (ime klicanega programa), da dobimo seznam imen datotek *)
  |> List.tl
  (* Iz vsake datoteke preberemo problem *)
  |> List.map read_problem
  (* Probleme zaporedoma rešimo *)
  |> List.iter find_and_display_solution (* Solve each sudoku individually *)
  (* |> find_solutions *) (* Solve and time all sudokus at once *)

(* Če domačo nalogo rešujete prek spletnega vmesnika, ki ne podpira branja datotek,
   lahko delovanje preizkušate prek spodnjega programa. *)

(* let () = "
┏━━━┯━━━┯━━━┓
┃483│921│657┃
┃967│3 5│821┃
┃251│876│493┃
┠───┼───┼───┨
┃548│132│976┃
┃729│ 64│ 38┃
┃136│798│ 45┃
┠───┼───┼───┨
┃372│689│514┃
┃814│253│769┃
┃695│417│382┃
┗━━━┷━━━┷━━━┛"
  |> Model.problem_of_string
  |> find_and_display_solution *)
