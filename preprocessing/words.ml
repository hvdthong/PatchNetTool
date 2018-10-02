(*
 * This file is part of PatchNet, licensed under the terms of the GPL v2.
 * See copyright.txt in the PatchNet source code for more information.
 * The PatchNet source code can be obtained at
 * https://github.com/hvdthong/PatchNetTool
 *)

module C = Lcommon

let drop_committer = ref true
let drop_tag_lines = ref true

let unspace s =
  String.concat " " (Str.split (Str.regexp "[ \t]+") s)

let unpunct s =
  let s = String.concat "" (Str.split (Str.regexp_string "(") s) in
  let s = String.concat "" (Str.split (Str.regexp_string ")") s) in
  let s = String.concat "" (Str.split (Str.regexp_string "[") s) in
  let s = String.concat "" (Str.split (Str.regexp_string "]") s) in
  let s = String.concat "" (Str.split (Str.regexp "[\"`']") s) in
  if s = ""
  then []
  else
    let len = String.length s in
    let last = String.get s (len - 1) in
    let s =
      if List.mem last ['.';',';':';';';'!']
      then String.sub s 0 (len-1)
      else s in
    let s =
      try let _ = Str.search_forward (Str.regexp "ffff") s 0 in "__address__"
      with Not_found ->
	try let _ = Str.search_forward (Str.regexp "0000") s 0 in "__number__"
	with Not_found ->
	try
	  let n = Str.search_forward (Str.regexp "+0x") s 0 in
	  let _ = Str.search_forward (Str.regexp "/0x") s n in
	  Printf.sprintf "%s+__offset__"
	    (List.hd (Str.split (Str.regexp_string "+") s))
	with Not_found ->
	  if Str.string_match (Str.regexp "[0-9]*\\.[0-9]+$") s 0
	  then "__float__"
	  else if Str.string_match (Str.regexp "[0-9]+$") s 0
	  then "__number__"
	  else s in
    if s = "" then [] else [s]

let hexnum s =
  let len = String.length s in
  if List.mem len [2;4;8;16;32;64]
  then
    try
      let _ =
	int_of_string
	  (String.concat "" (Str.split (Str.regexp "[a-fA-F]+") s)) in true
    with _ -> false
  else false

let get_words committer lines =
  let rcommitter = Str.regexp committer in
  let res =
    List.map
      (function l ->
	let l =
	  if !drop_committer
	  then
	    match Str.split_delim rcommitter l with
	      _::_::_ -> None
	    | _ -> Some l
	  else Some l in
	match l with
	  None -> []
	| Some l ->
	    match Str.full_split (Str.regexp "[:@<>]") l with
	      (Str.Text before)::(Str.Delim ":")::(Str.Text name)::
	      (Str.Delim "<")::
	      (Str.Text _)::(Str.Delim "@")::(Str.Text domain)::
	      (Str.Delim ">")::_ ->
		let bef = String.lowercase_ascii(C.trim before) in
		let ender =
		  List.hd (List.rev (Str.split (Str.regexp "-") bef)) in
		if !drop_tag_lines && (bef = "cc" || ender = "by")
		then []
		else
		  let nm = unspace name in
		  let nm = if nm = "" then [] else [nm] in
		  (Str.split (Str.regexp "[ \t]+") before) @
		  nm @ [domain]
	    | (Str.Text before)::(Str.Delim ":")::_
	      when
		!drop_tag_lines &&
		List.mem (C.trim(String.lowercase_ascii before))
		  ["fixes";"cc"] -> []
	    | es ->
		let w = Str.split (Str.regexp "[ \t]+") l in
		let first = match w with [] -> "" | x::_ -> x in
		let last = match List.rev w with [] -> "" | x::_ -> x in
		let is_bug = first = "BUG" || first = "BUG:" in
		(* the goal of the following is to drop lines related to
		   oopses *)
		let isnonwd =
		  String.length first > 1 &&
		  String.uppercase_ascii first = first in
		let endslash =
		  match Str.split (Str.regexp_string "/") last with
		    _::_::_ -> true
		  | _ -> false in
		if is_bug || (not isnonwd && not endslash)
		then
		  let w = List.concat (List.map unpunct w) in
		  List.map String.lowercase_ascii
		    (List.filter
		       (function x ->
			 Str.string_match
			   (Str.regexp "[a-zA-Z_][-a-zA-Z0-9_]*$")
			   x 0 && not (hexnum x))
		       w)
		else [])
      lines in
  List.concat res

let run_get_words infos =
  (* get commit messages *)
  let commits =
    List.fold_left
      (fun prev ((commit,stable,_,_,committer,files),_) ->
        (commit,committer)::prev)
      [] infos in
  let cwd = Sys.getcwd() in
  Sys.chdir !C.linux;
  let prefix = "/dev/shm/getinfo3" in
  let logs =
    Parmap.parmap ~ncores:(!C.cores) ~chunksize:C.chunksize
      ~init:(fun id -> Parmap.redirect ~path:prefix ~id)
      (fun (commit,committer) ->
        Printf.eprintf "step3: working on %s\n" commit;
        let message =
          C.cmd_to_list
            (Printf.sprintf "git log -n 1 --pretty=format:\"%%B\" %s"
               commit) in
        (commit,get_words committer message,message))
      (Parmap.L commits) in
  Printf.eprintf "done with step 3: %d\n" (List.length logs);
  Sys.chdir cwd;
  (* checking the words with python *)
  let mywords = Hashtbl.create 101 in
  List.iter
    (fun (commit,words,msg) ->
      List.iter
        (fun x ->
          if not (Hashtbl.mem mywords x) then Hashtbl.add mywords x "")
        words)
    logs;
  let fl = Filename.temp_file "wordlist" "txt" in
  let fl2 = Filename.temp_file "wordlist" "txt" in
  let o = open_out fl in
  Hashtbl.iter (fun k _ -> Printf.fprintf o "%s\n" k) mywords;
  close_out o;
  let _ =
    Sys.command
      (Printf.sprintf "python3 nlp.py %s > %s; /bin/rm %s" fl fl2 fl) in
  let i = open_in fl2 in
  let rec loop _ =
    let l1 = input_line i in
    let l2 = input_line i in
    Hashtbl.replace mywords l1 l2;
    loop() in
  (try loop() with End_of_file -> close_in i);
  let _ = Sys.command ("/bin/rm "^fl2) in
  (* adding the words to the dictionary *)
  List.iter
    (function (commit,words,msg) ->
      List.iter
	(function w ->
	  Lexer_c.add_index (Lexer_c.Info w) (Some commit);
	  let w' = Hashtbl.find mywords w in
	  if not (w' = "" || w' = w)
	  then Lexer_c.add_index (Lexer_c.Info w') (Some commit))
	words)
    logs;
  (* retrieving the words that occur in enough commits *)
  let logs =
    List.fold_left (* cannot be parmapped!!! *)
      (fun prev (commit,words,msg) ->
	let owords =
	  List.rev
	    (List.fold_left
	       (fun prev w ->
		 match Lexer_c.from_text_index w with
		   Some index -> index::prev
		 | None -> prev)
	       [] words) in
	let nlwords =
	  List.rev
	    (List.fold_left
	       (fun prev w ->
		 let w' = Hashtbl.find mywords w in
		 if w' = ""
		 then prev
		 else
		   match Lexer_c.from_text_index w' with
		     Some index -> index::prev
		   | None -> prev)
	       [] words) in
        (commit, owords, nlwords, msg) :: prev)
      [] logs in
  Printf.eprintf "done with step 4: %d\n" (List.length logs);
  logs
