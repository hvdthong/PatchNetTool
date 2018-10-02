(*
 * This file is part of PatchNet, licensed under the terms of the GPL v2.
 * See copyright.txt in the PatchNet source code for more information.
 * The PatchNet source code can be obtained at
 * https://github.com/hvdthong/PatchNetTool
 *)

(* Given a list of commits, find the author, authordate, committer of
the commits that contain at least one .c or .h file *)

module C = Lcommon

let marker = ">=============="
let marker2 = ">==============++++++++++++++"

let rec parse_commit_data commits linux =
  let process (l,label) acc =
    match l with
      commit::author_name::author_email::author_date::
      committer_name::committer_email::commit_date::subject::rest ->
	let author_email =
	  if author_email = "" then author_name else author_email in
	let committer_email = 
	  if committer_email = "" then committer_name else committer_email in
	let author_email = String.lowercase_ascii author_email in
	let committer_email = String.lowercase_ascii committer_email in
	let entry x =
	  (commit,label,author_email,author_date,committer_name,
	   committer_email,commit_date,subject,None,x) in
	let third (a,b,c) = c in
	let rec iloop acc = function
	    x::xs ->
	      if x = marker
	      then (acc,xs)
	      else
		(match Str.split (Str.regexp "[ \t]+") x with
		  [ins;del;fl] ->
		    iloop
		      ((C.safe_int_of_string 10 ins,
			C.safe_int_of_string 11 del,
			fl) :: acc) xs
		| _ -> iloop acc xs)
	  | _ -> (acc,[]) in
	let (pieces,rest) = iloop [] rest in
	let files = List.map third pieces in
	(entry files) :: acc
    | _ ->
	List.iter (fun x -> Printf.eprintf "%s\n" x) l;
	failwith "incomplete data" in
  Parmap.parfold ~ncores:(!C.cores) ~chunksize:C.chunksize
    process (Parmap.L commits) [] (@)

let get_commits commit_file =
  let commits = C.cmd_to_list ("cat "^commit_file) in
  let pretty =
    Printf.sprintf
      "--pretty=format:\"%%H%%n%%an%%n%%ae%%n%%at%%n%%cn%%n%%ce%%n%%ct%%n%%s\"" in
  let fixed_args = "--numstat --diff-filter=M --no-merges" in
  let infos =
    List.map
      (function c ->
	match Str.split (Str.regexp ": ") c with
	  [commit;label] ->
	    let commit = String.trim commit in
	    let label = String.trim label in
	    (if not (List.mem label ["true";"false"])
	    then failwith ("bad label: "^label));
	    (C.cmd_to_list
	       (Printf.sprintf "cd %s; git log -n 1 %s %s %s -- \"*.[ch]\""
		  !C.linux pretty fixed_args commit),
	     label)
	| _ ->  failwith "commit file lines should have the format commit: label")
      commits in
  let res = parse_commit_data infos !C.linux in
  Printf.eprintf "Patches: %d\n" (List.length res);
  res

