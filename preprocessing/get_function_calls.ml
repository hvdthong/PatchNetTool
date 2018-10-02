(*
 * This file is part of PatchNet, licensed under the terms of the GPL v2.
 * See copyright.txt in the PatchNet source code for more information.
 * The PatchNet source code can be obtained at
 * https://github.com/hvdthong/PatchNetTool
 *)

(* This is rather a hack.  It works on existing newres.out files.  The
dictionary information is hard coded.  A function call is assumed to be an
identifier immediately followed by an open parenthesis.  It should not be
preceded by a . or ->.  Function names that turn out to be generic are left
with their current code. *)

(* This is run on the output (.out file) of getinfo *)

let identifier = 72
let generic = 11
let open_paren = 81
let dot = 93
let arrow = 92

let process_output_to_list2 = fun command ->
  let chan = Unix.open_process_in command in
  let res = ref ([] : string list) in
  let rec process_otl_aux () =
    let e = input_line chan in
    res := e::!res;
    process_otl_aux() in
  try process_otl_aux ()
  with End_of_file ->
    let stat = Unix.close_process_in chan in (List.rev !res,stat)
let cmd_to_list command =
  let (l,_) = process_output_to_list2 command in l
let process_output_to_list = cmd_to_list
let cmd_to_list_and_status = process_output_to_list2

let get_data l =
  let rec loop after_dot = function
      [c1;c2;c3]::[d1;d2;d3]::rest
      when not after_dot && not (c1 = dot || c1 = arrow) ->
	if c1 = identifier && not (c2 = generic) && d1 = open_paren
	then c2 :: d1 :: loop false rest
	else c1 :: loop false ([d1;d2;d3]::rest)
    | [c1;c2;c3]::rest ->
	if c1 = dot || c1 = arrow
	then c1 :: loop true rest
	else c1 :: loop false rest
    | [] -> []
    | _ -> failwith "three values should always be present" in
  loop false l

let parse i o =
  let thecommit = ref "" in
  let rec loop _ =
    let l = input_line i in
    (match Str.split (Str.regexp ":") l with
      "commit"::rest -> thecommit := l; Printf.fprintf o "%s\n" l
    | ("label"|"author"|"committer"|"commit_date"|"author_date")::
      rest -> Printf.fprintf o "%s\n" l
    | [("commit message"|"simplified commit message")] ->
	Printf.fprintf o "%s\n" l;
	let l = input_line i in
	Printf.fprintf o "%s\n" l
    | [] -> Printf.fprintf o "\n"
    | ["commit code"] ->
	Printf.fprintf o "%s\n" l;
	let rec iloop _ =
	  let l = input_line i in
	  match Str.split (Str.regexp ": ") l with
	    [hunk;sign;category;line] ->
	      Printf.fprintf o "%s: %s: %s: %s\n" hunk sign category
		(String.concat ","
		   (List.map string_of_int
		      (get_data
			 (List.map
			    (function entry ->
			      List.map int_of_string
				(Str.split (Str.regexp "x") entry))
			    (Str.split (Str.regexp ",") line)))));
	      iloop()
	  | ["file";_] -> Printf.fprintf o "%s\n" l; iloop()
	  | [] -> Printf.fprintf o "\n"
	  | _ -> failwith ("bad line: "^l) in
	iloop()
    | [x] -> Printf.fprintf o "%s\n" x
    | _ -> failwith "bad line2");
    loop() in
  try loop() with End_of_file -> ()

let gfc file ofile =
  let i = open_in file in
  let o = open_out ofile in
  parse i o;
  close_in i;
  close_out o

