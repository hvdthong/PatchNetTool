(*
 * This file is part of PatchNet, licensed under the terms of the GPL v2.
 * See copyright.txt in the PatchNet source code for more information.
 * The PatchNet source code can be obtained at
 * https://github.com/hvdthong/PatchNetTool
 *)

module C = Lcommon

let before = ref "/dev/shm/linux-before"
let after = ref "/dev/shm/linux-after"
let decommented = "___decommented.c"
let me = ref 0
let quiet = ref false
let dwdiff = ref false

type change =
    Minus of int * int * int * int | Plus of int * int * int * int

type ctx = Min | Pls | Ctx

let extra_trim s =
  let len = String.length s in
  let is_space c = c = '\\' || C.is_space c in
  let i = ref 0 in
  while !i < len && try is_space (String.get s !i) with _ -> false do
    incr i
  done;
  let j = ref (len - 1) in
  while !j >= !i && try is_space (String.get s !j) with _ -> false do
    decr j
  done;
  if !i = 0 && !j = len - 1 then
    s
  else if !j >= !i then
    String.sub s !i (!j - !i + 1)
  else
    ""

let get_start_and_end s ts =
  let starter =
    match Str.bounded_split_delim (Str.regexp "[^ \t]") s 2 with
      [s] -> 0
    | [] -> 0
    | [space;s] -> String.length space
    | _ -> failwith "wrong length" in
  (starter,String.length ts + starter)

(*
let parse_line_diff lines =
  let rec loop valid hnk oline nline acc = function
      x::xs ->
	(match Str.split (Str.regexp " ") x with
	  "@@"::oline::nline::"@@"::_ ->
	    let oline =
	      C.safe_int_of_string 1
		(List.hd (Str.split (Str.regexp "[-,]") oline)) in
	    let nline =
	      C.safe_int_of_string 2
		(List.hd (Str.split (Str.regexp "[\\+,]") nline)) in
	    loop true (hnk+1) oline nline acc xs
	| _ ->
	    if valid
	    then
	      (let rest = String.sub x 1 (String.length x - 1) in
	      let trest = C.trim rest in
	      match String.get x 0 with
		'-' ->
		  if trest = ""
		  then loop true hnk (oline+1) nline acc xs
		  else
		    begin
		    let (scol,ecol) = get_start_and_end rest trest in
		    let entry = Minus(hnk,oline,scol,ecol) in
		    loop true hnk (oline+1) nline (entry::acc) xs
		    end
	      | '+' ->
		  if trest = ""
		  then loop true hnk oline (nline+1) acc xs
		  else
		    let (scol,ecol) = get_start_and_end rest trest in
		    let entry = Plus(hnk,nline,scol,ecol) in
		    loop true hnk oline (nline+1) (entry::acc) xs
	      | _ -> failwith ("unknown sign: "^x))
	    else loop false hnk oline nline acc xs)
    | [] -> acc in
  loop false 0 0 0 [] lines
*)

let parse_line_diff lines =
  let rec get_lines char ln acc = function
      x::xs ->
	if String.get x 0 = char
	then
	  let rest = String.sub x 1 (String.length x - 1) in
          let trest = extra_trim rest in
	  if trest = ""
	  then get_lines char (ln+1) acc xs
	  else
	    let (scol,ecol) = get_start_and_end rest trest in
	    get_lines char (ln+1) ((rest,ln,scol,ecol)::acc) xs
	else (List.rev acc,x::xs)
    | [] -> (List.rev acc,[]) in
  let rec loop hnk = function
      x::xs ->
	(match Str.split (Str.regexp " ") x with
	  "@@"::oline::nline::"@@"::_ ->
	    let oline =
	      C.safe_int_of_string 1
		(List.hd (Str.split (Str.regexp "[-,]") oline)) in
	    let nline =
	      C.safe_int_of_string 2
		(List.hd (Str.split (Str.regexp "[\\+,]") nline)) in
	    let (mlines,xs) = get_lines '-' oline [] xs in
	    let (plines,xs) = get_lines '+' nline [] xs in
	    (hnk,(mlines,plines)) :: loop (hnk+1) xs
	| _ -> loop hnk xs)
    | [] -> [] in
  let hnks = loop 1 lines in
  List.rev (* was reversed, so preserve that *)
    (List.concat
       (List.map
	  (function (hnk,(m,p)) ->
	    (List.map (function (_,ln,scol,ecol) -> Minus(hnk,ln,scol,ecol)) m) @
	    (List.map (function (_,ln,scol,ecol) -> Plus(hnk,ln,scol,ecol)) p))
	  hnks))

let parse_diff lines =
  let left = Str.regexp "\\[-\\|{\\+" in
  let right = Str.regexp "-\\]\\|\\+}" in
  let rec cloop l =
    match Str.bounded_full_split left l 2 with
      [Str.Text s1; Str.Delim s2; Str.Text s3] ->
	(Str.Text s1) :: (Str.Delim s2) :: mploop s3
    | [Str.Text s1] -> [Str.Text s1]
    | [Str.Delim s2; Str.Text s3] -> (Str.Delim s2) :: mploop s3
    | [Str.Delim s2] -> [Str.Delim s2]
    | pieces ->
	(Printf.eprintf "pieces 1 line %s\n" l;
	List.iter
	  (function
	      Str.Text s -> Printf.eprintf "text1: %s\n" s
	    | Str.Delim s -> Printf.eprintf "delim1: %s\n" s)
	  pieces;
	failwith "unexpected pieces1")
  and mploop l =
    match Str.bounded_full_split right l 2 with
      [Str.Text s1; Str.Delim s2; Str.Text s3] ->
	(Str.Text s1) :: (Str.Delim s2) :: cloop s3
    | [Str.Delim s2; Str.Text s3] -> (Str.Delim s2) :: cloop s3
    | [Str.Text s2; Str.Delim s3] ->
	[Str.Text s2; Str.Delim s3]
    | [Str.Delim s3] -> [Str.Delim s3]
    | [Str.Text s2] -> [Str.Text s2]
    | [] -> []
    | pieces ->
	(List.iter
	  (function
	      Str.Text s -> Printf.eprintf "text2: %s\n" s
	    | Str.Delim s -> Printf.eprintf "delim2: %s\n" s)
	  pieces;
	failwith "unexpected pieces2") in
  let minus s (hnk,ln,sc,ec) =
    if C.trim s = "" then [] else [Minus(hnk,ln,sc,ec)] in
  let plus s (hnk,ln,sc,ec) =
    if C.trim s = "" then [] else [Plus(hnk,ln,sc,ec)] in
  let get_ender s offset =
    String.length (C.trim s) + offset in
  let get_starter s offset =
    let prefix =
      match Str.bounded_split_delim (Str.regexp "[^ \t]") s 2 with
	[s] -> 0
      | [] -> failwith "empty string"
      | [space;s] -> String.length space
      | _ -> failwith "wrong length" in
    prefix + offset in
  let rec loop hnk status prvoline prvooffset prvnline prvnoffset acc =function
      ("--","--","--")::xs -> loop (hnk+1) status 0 0 0 0 acc xs
    | (oline,nline,str)::xs ->
	let oline = C.safe_int_of_string 1 oline in
	let nline = C.safe_int_of_string 2 nline in
	let pieces =
	  match status with
	    Min | Pls -> mploop str
	  | Ctx -> cloop str in
	let rec iloop status moffset poffset = function
	    [] -> (status,[],moffset,poffset) (* blank line? *)
	  | [Str.Text s] ->
	      (match status with
		Min ->
		  let moffset = get_starter s moffset in
		  (status,minus s (hnk,oline,moffset,get_ender s moffset),
		   moffset+String.length s,poffset)
	      | Pls ->
		  let poffset = get_starter s poffset in
		  (status,plus s (hnk,nline,poffset,get_ender s poffset),
		   moffset,poffset+String.length s)
	      | Ctx ->
		  (status,[], (* can happen if line is blank? *)
		   moffset+String.length s,poffset+String.length s))
	  | Str.Text s::Str.Delim "-]"::rest
	  | Str.Delim "[-"::Str.Text s::Str.Delim "-]"::rest ->
	      let woffset = get_starter s moffset in
	      let wender = get_ender s woffset in
	      let (stat,rest,moffset2,poffset2) =
		iloop Ctx (moffset+String.length s) poffset rest in
	      (stat,(minus s (hnk,oline,woffset,wender)) @ rest,
	       moffset2,poffset2)
	  | Str.Text s::Str.Delim "+}"::rest
	  | Str.Delim "{+"::Str.Text s::Str.Delim "+}"::rest ->
	      let woffset = get_starter s poffset in
	      let wender = get_ender s woffset in
	      let (stat,rest,moffset2,poffset2) =
		iloop Ctx moffset (poffset+String.length s) rest in
	      (stat,(plus s (hnk,nline,woffset,wender)) @ rest,
	       moffset2,poffset2)
	  | Str.Delim "-]"::rest | Str.Delim "+}"::rest ->
	      iloop Ctx 0 0 rest
	  | Str.Text s::rest ->
	      let len = String.length s in
	      iloop Ctx (moffset + len) (poffset + len) rest
	  | [Str.Delim "[-";Str.Text s] ->
	      let woffset = get_starter s moffset in
	      (Min,minus s (hnk,oline,woffset,get_ender s woffset),
	       moffset+String.length s,poffset)
	  | [Str.Delim "{+";Str.Text s] ->
	      let woffset = get_starter s poffset in
	      (Pls,plus s (hnk,nline,woffset,get_ender s woffset),
	       moffset,poffset+String.length s)
	  | _ -> failwith "bad pieces" in
	let moffset = if oline = prvoline then prvooffset else 0 in
	let poffset = if nline = prvnline then prvnoffset else 0 in
	let (status,infos,ooffset,noffset) =
	  iloop status moffset poffset pieces in
	loop hnk status oline ooffset nline noffset (infos@acc) xs
    | [] -> acc in
  loop 0 Ctx 0 0 0 0 [] lines
    
exception Failed
    
let runone commit tag info file =
  let diffinfo = (!C.tmpdir^"/collect_diff") in
  let diffinfo =
    if !C.cores > 1 then Printf.sprintf "%s%d" diffinfo !me else diffinfo in
  let htbl = Hashtbl.create 101 in
  let o = open_out diffinfo in
  List.iter
    (function (hnk,line,scol,ecol) ->
      (if not (Hashtbl.mem htbl line) then Hashtbl.add htbl line hnk);
      Printf.fprintf o "%d:%d:%d\n" line scol ecol)
    info;
  close_out o;
  let cocci_opts =
    (* --disable-macros is to avoid expanding macros because of line # pbs *)
    "--very-quiet --include-headers-for-types --no-loops --timeout 360" in
  let t1 = Unix.gettimeofday () in
  let cmd =
    Printf.sprintf
      "spatch --debug parse_changes.cocci %s -D data=%s -D tag=%s -D commit=%s -D thefile=%s %s"
      cocci_opts diffinfo tag commit file file in
  let results = C.cmd_to_list cmd in
  Printf.eprintf "cmd %s\n" cmd;
  Printf.eprintf "results: %d\n" (List.length results);
  let t2 = Unix.gettimeofday () in
  Printf.eprintf "spatch time: %0.2f\n" (t2 -. t1); flush stderr;
  if List.mem "Failed" results
  then
    begin
      Printf.eprintf "Failed\n"; flush stderr;
      (if !quiet
      then Printf.printf "Failed\n"
      else
	List.iter (function l -> Printf.printf "%s%s\n" tag l) results);
      raise Failed
    end
  else
    begin
      let res =
      List.map
	(function l ->
	  match Str.bounded_split (Str.regexp "[\\.:-]") l 5 with
	    [sln;scol;eln;ecol;str] ->
	      let str = String.sub str 1 (String.length str - 1) in
	      let sln = C.safe_int_of_string 3 sln in
	      let eln = C.safe_int_of_string 4 eln in
	      let hnk =
		let rec loop sln =
		  if sln > eln
		  then
		    begin
		      Printf.eprintf "Failed: sln %d > eln %d\n" sln eln;
		      raise Failed
		    end
		  else
		    try Hashtbl.find htbl sln
		    with Not_found -> loop (sln+1) in
		loop sln in
	      (hnk,tag,sln,C.safe_int_of_string 5 scol,
	       eln,C.safe_int_of_string 6 ecol,Decomment.destring str)
	  | _ -> failwith ("bad string: "^l))
	results in
      Printf.eprintf "Succeeded %d\n" (List.length results); flush stderr;
      res
    end

let cleanup_dwdiff l =
  let rec read_to_end f right acc = function
      [] -> failwith "not possible"
    | [x] -> (List.rev acc,x,[])
    | ((_,_,str) as x)::((y::_) as rest) ->
	if f y && List.length (Str.split right str) < 2
	then read_to_end f right (x::acc) rest
	else (List.rev acc,x,rest) in
  let reorganize (oline1,nline1,str1) rest left right fn =
    match Str.split_delim (Str.regexp_string left) str1 with
      [bef1;aft1] ->
	let re_right = Str.regexp_string right in
	let (middle,(oline3,nline3,str3),after) =
	  read_to_end fn re_right [] rest in
	(match Str.bounded_split_delim re_right str3 2 with
	  [bef3;aft3] ->
	    if bef1 = bef3
	    then
	      let new_str1 = left^bef1^aft1 in
	      let new_str3 = right^bef3^aft3 in
	      Some ((oline1,nline1,new_str1)::middle,
		    (oline3,nline3,new_str3)::after)
	    else None
	| _ -> None)
    | _ -> None in
  let rec loop = function
      [] -> []
    | [x] -> [x]
    | ((oline1,nline1,str1) as x)::(((oline2,nline2,str2)::_) as rest) ->
	if oline1 = oline2
	then
	  match reorganize x rest "{+" "+}" (fun (o,_,_) -> o = oline1) with
	    None -> x :: loop rest
	  | Some(first,rest) -> first @ loop rest
	else if nline1 = nline2
	then
	  match reorganize x rest "[-" "-]" (fun (_,n,_) -> n = nline1) with
	    None -> x :: loop rest
	  | Some(first,rest) -> first @ loop rest
	else x :: loop rest in
  loop l

let union l1 l2 =
  List.fold_left (fun p x -> if List.mem x p then p else x :: p) l2 l1

let seen_words = Hashtbl.create 101

let getone (commit,files) =
  let separator = "!\"#%&()+*,-./:;<=>?@[\\]^{|}~ \\t" in
  (if not !quiet then Printf.printf "COMMIT: %s\n\n" commit);
  let btmp = Printf.sprintf "%s/tmp/before_%d.c" !C.tmpdir !me in
  let atmp = Printf.sprintf "%s/tmp/after_%d.c" !C.tmpdir !me in
  let dbtmp = Printf.sprintf "%s/tmp/before_decommented_%d.c" !C.tmpdir !me in
  let datmp = Printf.sprintf "%s/tmp/after_decommented_%d.c" !C.tmpdir !me in
  List.fold_left
    (fun prev file ->
      let _ =
	Sys.command
	  (Printf.sprintf "cd %s; git show %s^:%s > %s"
	     !C.linux commit file btmp) in
      let _ =
	Sys.command
	  (Printf.sprintf "cd %s; git show %s:%s > %s"
	     !C.linux commit file atmp) in
      Decomment.decomment btmp dbtmp;
      Decomment.decomment atmp datmp;
      (if not !quiet then Printf.printf "FILE: %s\n" file);
      let diff =
	if !dwdiff
	then
	  C.cmd_to_list
	    (Printf.sprintf "dwdiff %s %s -L1 -C0 -A best --delimiters='%s'"
	       dbtmp datmp separator)
	else
	  (*let opt = "--ignore-blank-lines --ignore-space-change -U0" in*)
	  let opt = "-U0" in
	  C.cmd_to_list (Printf.sprintf "git diff %s %s %s" opt dbtmp datmp) in
      if diff = []
      then prev
      else
	begin
	  (if not !quiet
	  then
	    begin
	      List.iter (function l -> Printf.printf "%s\n" l) diff;
	      Printf.printf "\n";
	      flush stdout;
	      flush stderr
	    end);
	  let info =
	    if !dwdiff
	    then
	      begin
		let diff =
		  List.map
		    (function l ->
		      match Str.bounded_split (Str.regexp "[: ]") l 3 with
			["--"] -> ("--","--","--")
		      | [oline;nline] -> (oline,nline,"")
		      | [oline;nline;str] -> (oline,nline,str)
		      | _ -> failwith "bad line")
		    diff in
		let diff = cleanup_dwdiff diff in
		(if not !quiet
		then
		  begin
		    Printf.printf "CLEANED:\n";
		    List.iter
		      (function (o,n,str) ->
			Printf.printf "%s:%s %s\n" o n str)
		      diff;
		    Printf.printf "\n";
		    flush stdout;
		    flush stderr
		  end);
		parse_diff diff
	      end
	    else parse_line_diff diff in
	  let (minus_info,plus_info) =
	    List.fold_left
	      (fun (mi,pi) ->
		function
		    Minus(hnk,line,scol,ecol) -> ((hnk,line,scol,ecol)::mi,pi)
		  | Plus(hnk,line,scol,ecol) -> (mi,(hnk,line,scol,ecol)::pi))
	      ([],[]) info in
	  Printf.eprintf "starting minus results\n"; flush stderr;
	  let minus_results =
	    if minus_info = []
	    then []
	    else runone commit "-" minus_info btmp in
	  Printf.eprintf "starting plus results\n"; flush stderr;
	  let plus_results =
	    if plus_info = []
	    then []
	    else runone commit "+" plus_info atmp in
	  let results = List.sort compare (minus_results@plus_results) in
	  (if not !quiet
	  then
	    begin
	      List.iter
		(function (hnk,sign,sln,scol,eln,ecol,str) ->
		  Printf.printf "%d.%d-%d.%d:%s%s\n" sln scol eln ecol
		    sign str)
		results;
	      Printf.printf "\n"
	    end);
	  if results = []
	  then prev
	  else
	    ((file,results)::(fst prev),
	     if !quiet
	     then
	       List.fold_left
		 (fun prev (hnk,sign,sln,scol,eln,ecol,str) ->
		   List.fold_left
		     (fun prev ->
		       function
			   Str.Text _ -> prev
			 | Str.Delim wd ->
			     if Hashtbl.mem seen_words wd
			     then prev
			     else
			       begin
				 Hashtbl.add seen_words wd ();
				 wd :: prev
			       end)
		     prev
		     (Str.full_split (Str.regexp "[a-zA-Z_][a-zA-Z0-9_]*")
			str))
		 (snd prev) results
	     else [])
	end)
    ([],[]) files
