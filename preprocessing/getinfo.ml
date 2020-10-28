(*
 * This file is part of PatchNet, licensed under the terms of the GPL v2.
 * See copyright.txt in the PatchNet source code for more information.
 * The PatchNet source code can be obtained at
 * https://github.com/hvdthong/PatchNetTool
 *)

module C = Lcommon
module CD = Collect_diff

let balance = ref false

(* if true, then the commit log message is included in the output *)
let words = ref true

let protect_strings s =
  (* Drop escaped string quotes - they aren't words, so we don't care about
     them *)
  let s = String.concat "" (Str.split (Str.regexp_string "\\\"") s) in
  (* likewise, drop ' ' *)
  let s = String.concat "" (Str.split (Str.regexp_string "' '") s) in
  let pieces = Str.split_delim (Str.regexp "\"") s in
  (* make strings into one word *)
  let protect s =
    (* hope for the best... *)
    let s = String.concat "SSS" (Str.split (Str.regexp " ") s) in
    let s = String.concat "TTT" (Str.split (Str.regexp "\t") s) in
    s in
  let rec loop = function
      before::inside::rest -> before ^ "\"" ^ protect inside ^ "\"" ^ loop rest
    | [s] -> s
    | [] -> "" in
  loop pieces

let cfile file =
  Filename.check_suffix file ".c" || Filename.check_suffix file ".h"

let do_balance l =
  Random.self_init();
  let tbl = Hashtbl.create 101 in
  List.iter
    (function (((commit,label,ad,cd,_,_),results) as x) ->
      C.hashadd tbl label x)
    l;
  let minsz =
    Hashtbl.fold
      (fun k v mn ->
	match mn with
	  None -> Some(List.length !v)
	| Some n -> Some(min n (List.length !v)))
      tbl None in
  let rec reduce l = function
      0 -> l
    | n ->
	let chosen = Random.int (List.length l) in
	let rec iloop l n =
	  match l with
	    [] -> l
	  | x::xs ->
	      if n = 0
	      then xs
	      else x :: (iloop xs (n-1)) in
	reduce (iloop l chosen) (n-1) in
  match minsz with
    None -> failwith "no data?"
  | Some minsz ->
      Hashtbl.fold
	(fun k v r ->
	  List.fold_left (fun p c ->  c :: p) r
	    (reduce !v (List.length !v - minsz)))
	tbl []

let rec print_between f s = function
    [] -> ()
  | [x] -> f x
  | x::xs -> f x; s(); print_between f s xs

let print_output res txt infos logtbl =
  let o = open_out res in
  let u = match txt with Some txt -> Some(open_out txt) | None -> None in
  List.iter
    (function ((commit,label,ad,cd,_,_),results) ->
      (match u with
	Some u ->
	  Printf.fprintf u "\n----------\ncommit: %s\n" commit;
	  Printf.fprintf u "label: %s\n" label;
	  Printf.fprintf u "author_date: %s\n" ad;
	  Printf.fprintf u "commit_date: %s\n" cd
      | None -> ());
      Printf.fprintf o "commit: %s\n" commit;
      Printf.fprintf o "label: %s\n" label;
      Printf.fprintf o "author_date: %s\n" ad;
      Printf.fprintf o "commit_date: %s\n" cd;
      (match logtbl with
	Some logtbl ->
	  let (message,nlmessage,msgu) = Hashtbl.find logtbl commit in
	  Printf.fprintf o "\ncommit message:\n";
	  print_between
	    (function x -> Printf.fprintf o "%d" x)
	    (function _ -> Printf.fprintf o ",")
	    message;
	  Printf.fprintf o "\n";
	  Printf.fprintf o "\nsimplified commit message:\n";
	  print_between
	    (function x -> Printf.fprintf o "%d" x)
	    (function _ -> Printf.fprintf o ",")
	    nlmessage;
	  Printf.fprintf o "\n";
	  (match u with
	    Some u ->
	      Printf.fprintf u "commit message:\n";
	      List.iter (function l -> Printf.fprintf u "%s\n" l) msgu
	  | None -> ())
      | _ -> ());
      Printf.fprintf o "\ncommit code:\n";
      (match u with
	Some u -> Printf.fprintf u "\ncommit code:\n"
      | None -> ());
      List.iter
	(function (file,lines) ->
	  Printf.fprintf o "file: %s\n" file;
	  (match u with
	    Some u -> Printf.fprintf u "file: %s\n" file
	  | None -> ());
	  List.iter
	    (function (hnk,sign,sln,scol,eln,ecol,ty,coded,str) ->
	      Printf.fprintf o "%d: %s: %s: " hnk sign ty;
	      (match u with
		Some u -> Printf.fprintf u "%d: %s: %s: " hnk sign ty
	      | None -> ());
	      print_between
		(function x -> Printf.fprintf o "%s" x)
		(function _ -> Printf.fprintf o ",")
		coded;
	      Printf.fprintf o "\n";
	      (match u with
		Some u -> Printf.fprintf u "%s\n" str
	      | None -> ()))
	    lines)
	results;
      Printf.fprintf o "\n")
    infos;
  (match u with Some u -> close_out u | None -> ());
  close_out o

let commit_list = ref ""
let output_prefix = ref ""

let options =
  ["-o", Arg.Set_string output_prefix,
    "  prefix of the output file";
    "--commit-list", Arg.Set_string commit_list,
    "  list of labelled commits";
    "--commit_list", Arg.Set_string commit_list,
    "  list of labelled commits (alternative argument)";
    "--git", Arg.Set_string Lcommon.linux,
    "  location of git tree";
    "--nolog", Arg.Clear words, "  exclude commit log message";
    "--balance", Arg.Set balance,
    "  same number of commits with true and false labls";
    "--keep-ifdefs", Arg.Clear Decomment.drop_ifdefs,
    "  keep ifdefs, for backward compatability";
    "--tmpdir", Arg.Set_string C.tmpdir,
    "  temporary directory";
    "-j", Arg.Set_int C.cores, "  number of cores";
  ]
let anonymous s = failwith "no anonymous arguments"
let usage = ""

let _ =
  Arg.parse (Arg.align options) anonymous usage;
  (if !output_prefix = "" then failwith "-o <output file prefix> required");
  CD.before := Printf.sprintf "%s/linux-before" !C.tmpdir;
  CD.after := Printf.sprintf "%s/linux-after" !C.tmpdir;
  let _ = Sys.command (Printf.sprintf "mkdir -p %s/tmp" !C.tmpdir) in
  let infos = Patch.get_commits !commit_list in
  let infos =
    List.fold_left
      (fun prev
	  (commit,label,author_email,author_date,
	   committer_name,committer_email,
	   commit_date,subject,from,files) ->
	     (commit,label,author_date,commit_date,
	      committer_email,files) :: prev)
      [] infos in
  (* get diffs and words *)
  CD.before := C.before_linux;
  CD.after := C.after_linux;
  CD.quiet := true;
  let prefix = "/dev/shm/getinfo1" in
  let infos =
    if !C.cores = 1
    then
      List.rev
	(List.fold_left
	   (fun rest ((commit,_,_,_,_,files) as x) ->
	     Printf.eprintf "starting %s\n" commit; flush stderr;
	     try
	       match CD.getone (commit,files) with
		 ([],_) -> rest
	       | cur -> (x,cur) :: rest
	     with CD.Failed -> rest)
	   [] infos)
    else
      Parmap.parfold ~ncores:(!C.cores) ~chunksize:C.chunksize
	~init:(fun id -> CD.me := id; Parmap.redirect ~path:prefix ~id)
	~finalize:(fun () -> flush_all ())
	(fun ((commit,_,_,_,_,files) as x) rest ->
	  Printf.eprintf "starting %s\n" commit; flush stderr;
	  try
	    match CD.getone (commit,files) with
	      ([],_) -> rest
	    | cur -> (x,cur) :: rest
	  with CD.Failed -> rest)
	(Parmap.L infos) [] (@) in
  Printf.eprintf "after parfold\n"; flush stderr;
  Lexer_c.init();
  List.iter (* cannot be parmapped!!! *)
    (function ((commit,_,_,_,_,_),(_,words)) ->
      List.iter
	(function x -> Lexer_c.add_index (Lexer_c.Info x) (Some commit))
	words)
     infos;
  Printf.eprintf "after word processing %d\n" (List.length infos);
  let prefix = "/dev/shm/getinfo2" in
  let infos =
    Parmap.parmap ~ncores:(!C.cores) ~chunksize:C.chunksize
      ~init:(fun id -> Parmap.redirect ~path:prefix ~id)
      (fun (((commit,_,_,_,_,_) as x),(results,_)) ->
	Lexer_c.current_commit := commit;
	let res =
	  (x,
	   List.map
	     (function (file,results) ->
	       (file,
		List.map
		  (function (hnk,sign,sln,scol,eln,ecol,str) ->
		    let (ty,str) =
		      match Str.bounded_split (Str.regexp ": ") str 2 with
			["ECC";str] -> ("ECC",str)
		      | ["EHC";str] -> ("EHC",str)
		      | _ -> ("Normal",str) in
		    let lexbuf = Lexing.from_string str in
		    let rec loop _ =
		      let tok =
			try Some(Lexer_c.token lexbuf)
			with Failure s ->
			  Printf.eprintf "Problem with %s: %s\n" commit s;
			  None in
		      match tok with
			Some ["-1";"-1"] -> []
		      | Some tok -> tok @ loop()
		      | None -> loop() in
		    (hnk,sign,sln,scol,eln,ecol,ty,loop(),str))
		  results))
	     results) in
	flush stdout; flush stderr; res)
      (Parmap.L infos) in
  let infos =
    if !balance
    then do_balance infos
    else infos in
  Printf.eprintf "after parmap\n"; flush stderr;
  let logtbl = 
    if !words
    then
      begin
	let logtbl = Hashtbl.create 101 in
	let logs = Words.run_get_words infos in
	List.iter
	  (function (commit,words,nlwords,msg) ->
	    Hashtbl.add logtbl commit (words,nlwords,msg))
	  logs;
	Some logtbl
      end
    else None in
  let tmp = !output_prefix^".tmp" in
  print_output tmp None (*(Some "newtxt.out")*) infos logtbl;
  let o = open_out (!output_prefix^".dict") in
  Lexer_c.print_dictionary o;
  close_out o;
  Get_function_calls.gfc tmp (!output_prefix^".out")

