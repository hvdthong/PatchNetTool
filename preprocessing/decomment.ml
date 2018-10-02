(*
 * This file is part of PatchNet, licensed under the terms of the GPL v2.
 * See copyright.txt in the PatchNet source code for more information.
 * The PatchNet source code can be obtained at
 * https://github.com/hvdthong/PatchNetTool
 *)

(* Hoping for no occurrences of /*/ *)
(* empties strings, if possible *)
(* not supporting //, shouldn't occur in the kernel *)

exception Unbalanced

let drop_ifdefs = ref true

let destring l =
  let pieces = Str.full_split (Str.regexp "\"") l in
  let rec loop = function
      [] -> ""
    | Str.Delim _ :: Str.Text s :: Str.Delim _ :: rest ->
	"\""^(String.make (String.length s) ' ')^"\""^(loop rest)
    | Str.Text s :: rest -> s ^ loop rest
    | _ -> raise Unbalanced in
  try loop pieces with Unbalanced -> l (* hope for the best *)

let deifdef l0 =
  let l = String.concat "" (Str.split (Str.regexp "[ \t]") l0) in
  let ifdef =
    Str.string_match (Str.regexp "#if") l 0 ||
    Str.string_match (Str.regexp "#else") l 0 ||
    Str.string_match (Str.regexp "#endif") l 0 ||
    Str.string_match (Str.regexp "#error") l 0 in
  if ifdef
  then String.make (String.length l0) ' '
  else l0

let do_decomment i o =
  let rec loop = function
      [] -> (outloop,"")
    | Str.Text s::rest ->
	let (continue,rest) = loop rest in
	(continue, s ^ rest)
    | Str.Delim "/*"::Str.Text s::Str.Delim "*/"::rest ->
	let (continue,rest) = loop rest in
	(continue,"  "^(String.make (String.length s) ' ')^"  "^rest)
    | Str.Delim "/*"::Str.Delim "*/"::rest ->
	let (continue,rest) = loop rest in
	(continue,"    "^rest)
    | [Str.Delim "/*";Str.Text s] -> (inloop,"")
    | [Str.Delim "/*"] -> (inloop,"")
    | (Str.Delim s)::rest ->
	(* bad string, but don't want to crash, so move on *)
	let (continue,rest) = loop rest in
	(continue,s^rest)
  and outloop _ =
    let l = input_line i in
    let l = destring l in
    let l = if !drop_ifdefs then deifdef l else l in
    let pieces = Str.full_split (Str.regexp "/\\*\\|\\*/") l in
    let (continue,res) = loop pieces in
    (*let res = if String.trim res = "" then "" else res in*)
    Printf.fprintf o "%s\n" res;
    continue()
  and inloop _ =
    let l = input_line i in
    let l = destring l in
    let l = if !drop_ifdefs then deifdef l else l in
    let pieces = Str.full_split (Str.regexp "/\\*\\|\\*/") l in
    let front = function
	[Str.Text s] -> (inloop,"")
      | Str.Delim "*/"::rest ->
	  let (continue,res) = loop rest in
	  (continue,"  "^res)
      | Str.Text s::Str.Delim "*/"::rest ->
	  let (continue,res) = loop rest in
	  (continue,(String.make (String.length s) ' ')^"  "^res)
      | [] -> (inloop,"")
    | (Str.Text s|Str.Delim s)::rest ->
	(* bad string, but don't want to crash, so move on *)
	let (continue,rest) = loop rest in
	(continue,s^rest) in
    let (continue,res) = front pieces in
    (*let res = if String.trim res = "" then "" else res in*)
    Printf.fprintf o "%s\n" res;
    continue() in
  try outloop() with End_of_file -> ()

let decomment file dest =
  let i = open_in file in
  let o = open_out dest in
  do_decomment i o;
  close_in i;
  close_out o
