@initialize:ocaml@
commit << virtual.commit;
data << virtual.data;
tag << virtual.tag; // - or +
thefile << virtual.thefile; // - or +
@@

let relevant_lines = Hashtbl.create 101
let variables = Hashtbl.create 101
let local_functions = ref []
let inits = Hashtbl.create 101

let escape x = String.concat "\\n" (Str.split (Str.regexp "\n") x)

let hashadd tbl key pos =
  let cell =
    try Hashtbl.find tbl key
    with Not_found ->
      let cell = ref [] in
      Hashtbl.add tbl key cell;
      cell in
  cell := pos :: !cell

let hashaddv tbl key (i,t) =
  let t = String.concat "__0__" (Str.split (Str.regexp " ") t) in
  let cell =
    try Hashtbl.find tbl key
    with Not_found ->
      let cell = ref [] in
      Hashtbl.add tbl key cell;
      cell in
  cell := (i,t) :: !cell

let iota starter ender =
  let rec loop i =
    if i > ender
    then []
    else i :: loop (i+1) in
  loop starter

let safe_int_of_string key s =
  try int_of_string s
  with x -> (Printf.eprintf "%d: ios failure on %s\n" key s; raise x)

let lines = Common.cmd_to_list (Printf.sprintf "cat %s" data)
let _ =
  List.iter
    (function line ->
      match Str.split (Str.regexp (Printf.sprintf "[:%s]" tag)) line with
	[line;scol;ecol] ->
	  let line = safe_int_of_string 21 line in
	  let scol = safe_int_of_string 22 scol in
	  let ecol = safe_int_of_string 23 ecol in
	  hashadd relevant_lines line (scol,ecol,ref [])
      | _ -> failwith "bad line")
    lines

let merge scol ecol found =
  let found = List.sort compare found in
  let rec loop = function
      [] -> false
    | [(s,e)] ->
	s = scol && e = ecol
    | (s1,e1)::(s2,e2)::rest ->
	if s1 <= s2 && s2 <= e1 && e1 <= e2
	then loop ((s1,e2)::rest)
	else if s1 = scol && (e1 = s2 || e1 + 1 = s2)
	then loop ((s1,e2)::rest)
	else false in
  loop found

(* update found whenever coverage is obtained, ie the entire line is matched *)
let domerge scol ecol found =
  let ok = merge scol ecol !found in
  (if ok then found := [(scol,ecol)]);
  ok

let already_covered scol ecol found =
  let found = List.sort compare !found in
  let rec loop = function
      [] -> false
    | [(s,e)] ->
	s <= scol && e >= ecol
    | (s1,e1)::(s2,e2)::rest ->
	if s1 <= scol && e1 >= ecol
	then true
	else
	if e1 + 1 = s2
	then loop ((s1,e2)::rest)
	else false in
  loop found

let inrange p =
  p.file = List.hd(Coccilib.files()) &&
  p.line >= p.current_element_line && p.line <= p.current_element_line_end &&
  p.line_end >= p.current_element_line && p.line_end <= p.current_element_line_end

(* see if the position is within a line that contains changes and that line
has not already been chosen (not ok) *)
let relevant p =
  let p = List.hd p in
  inrange p &&
  try
    (if not (p.line = p.line_end)
    then
      failwith
	(Printf.sprintf
	   "%s: %s: multiline arg to relevant: %d.%d-%d.%d"
	   commit p.file p.line p.col p.line_end p.col_end));
    let info = !(Hashtbl.find relevant_lines p.line) in
    let starter = p.col in
    let ender = p.col_end in
    List.exists
      (function (s,e,found) ->
	let ok = domerge s e found in (* true if whole line covered *)
	let used = ok || already_covered starter ender found in
	not ok && not used && s <= starter && ender <= e)
      info
  with Not_found -> false

let subrelevant (_,p,all) =
  let p = List.hd p in
  inrange p &&
  let lines = iota p.line p.line_end in
  let elements =
    List.concat
      (List.map
	 (function line ->
	   try
	     List.map (fun x -> (line,x))
	       (!(Hashtbl.find relevant_lines line))
	   with Not_found -> [])
	 lines) in
  let starter = (p.line,p.col) in
  let ender = (p.line_end,p.col_end) in
  List.exists
    (function (l,(s,e,found)) ->
      let ok = domerge s e found in
      let used =
	ok ||
	(if fst starter = l && fst ender = l
	then already_covered (snd starter) (snd ender) found
	else if fst starter = l
	then already_covered (snd starter) e found
	else if fst ender = l
	then already_covered s (snd ender) found
	else ok) in
      (all || (not ok && not used)) && (max starter (l,s)) < (min (l,e) ender))
    elements

let funsubrelevant (id,pos,all) =
  let p = List.hd pos in
  if List.mem p.current_element !local_functions
  then
    let p =
      {p with
	line = p.current_element_line;
	line_end = p.current_element_line_end;} in
    subrelevant (id,[p],all)
  else false

let fullyrelevant (_,p,all) =
  let p = List.hd p in
  inrange p &&
  let lines = iota p.line p.line_end in
  let elements =
    List.concat
      (List.map
	 (function line ->
	   try
	     List.map (fun x -> (line,x))
	       (!(Hashtbl.find relevant_lines line))
	   with Not_found -> [])
	 lines) in
  let ok =
    List.filter
      (function l ->
	try let _ = List.assoc l elements in true with _ -> false)
      lines in
  (*List.length ok >= (List.length lines) / 3 &&*)
  List.length ok = (List.length lines) &&
  (let starter = (p.line,p.col) in
  let ender = (p.line_end,p.col_end) in
  List.exists
    (function (l,(s,e,found)) ->
      let ok = domerge s e found in
      let used =
	ok ||
	(if fst starter = l && fst ender = l
	then already_covered (snd starter) (snd ender) found
	else if fst starter = l
	then already_covered (snd starter) e found
	else if fst ender = l
	then already_covered s (snd ender) found
	else ok) in
      (all || (not ok && not used)) && (max starter (l,s)) < (min (l,e) ender))
    elements)

let dfullyrelevant (n,p,all) =
  let res = fullyrelevant (n,p,all) in
  Printf.eprintf "fullyrelevant %d -> %b\n" n res;
  res

let dsubrelevant (n,p,all) =
  let p = List.hd p in
  flush stdout;
  Printf.eprintf
    "%d: file: %s line %d.%d-%d.%d\n" n
    p.file p.line p.col p.line_end p.col_end;
  flush stderr;
  let lines = iota p.line p.line_end in
  let elements =
    List.concat
      (List.map
	 (function line ->
	   try
	     List.map (fun x -> (line,x))
	       (!(Hashtbl.find relevant_lines line))
	   with Not_found -> [])
	 lines) in
  let starter = (p.line,p.col) in
  let ender = (p.line_end,p.col_end) in
  Printf.eprintf "options: %d\n" (List.length elements); flush stderr;
  List.exists
    (function (l,(s,e,found)) ->
      Printf.eprintf "%d: s %d e %d found %d\n" l s e (List.length !found);
      let ok = domerge s e found in
      let used =
	ok ||
	(if fst starter = l && fst ender = l
	then already_covered (snd starter) (snd ender) found
	else if fst starter = l
	then already_covered (snd starter) e found
	else if fst ender = l
	then already_covered s (snd ender) found
	else ok) in
      List.iter (fun (s,e) -> Printf.eprintf "  %d %d\n" s e) !found;
      Printf.eprintf "  trying %d.%d - %d.%d: %b %b %b\n" l s l e all (not ok)
	(not used);
      Printf.eprintf "  max %d.%d %d.%d < min %d.%d %d.%d: %b\n"
	(fst starter) (snd starter) l s (fst ender) (snd ender) l e
	((max starter (l,s)) < (min (l,e) ender));
      (all || (not ok && not used)) && (max starter (l,s)) < (min (l,e) ender))
    elements

let drelevant p =
  let p = List.hd p in
  flush stdout;
  Printf.eprintf "line %d: %d-%d\n" p.line p.col p.col_end;
  flush stderr;
  try
    (if not (p.line = p.line_end)
    then
      failwith
	(Printf.sprintf
	   "%s: %s: multiline arg to relevant: %d.%d-%d.%d"
	   commit p.file p.line p.col p.line_end p.col_end));
    let info = !(Hashtbl.find relevant_lines p.line) in
    let starter = p.col in
    let ender = p.col_end in
    Printf.eprintf "options: %s\n"
      (String.concat " "
	 (List.map (fun (s,e,_) -> Printf.sprintf "%d-%d" s e) info));
    let res =
      List.exists
	(function (s,e,found) ->
	  let ok = domerge s e found in
	  let used = already_covered starter ender found in
	  not ok && not used && s <= starter && ender <= e)
	info in
    Printf.eprintf "ok? %b\n" res;
    res
  with Not_found -> false

(* we hope for only one statement per line, or at least only one with
 changes *)
let validate line scol ecol =
  try
    let entry = !(Hashtbl.find relevant_lines line) in
    match ecol with
      None ->
	List.iter
	  (function (scol1,ecol1,found) ->
	    (if scol <= scol1
	    then found := (scol1,ecol1) :: !found
	    else if scol <= ecol1
	    then found := (scol,ecol1) :: !found);
	    (*Printf.eprintf "ending found %s\n"
	      (String.concat " "
		 (List.map (function (s,e) -> Printf.sprintf "%d-%d" s e)
		    !found))*))
	  entry
    | Some ecol ->
	List.iter
	  (function (scol1,ecol1,found) ->
	    (if scol <= scol1 && ecol >= ecol1
	    then found := (scol1,ecol1) :: !found
	    else if scol <= scol1 && ecol >= scol1
	    then found := (scol1,ecol) :: !found
	    else if scol <= ecol1 && ecol >= ecol1
	    then found := (scol,ecol1) :: !found
	    else if scol1 <= scol && ecol <= ecol1
	    then found := (scol,ecol) :: !found);
	    (*Printf.eprintf "ending found %s\n"
	      (String.concat " "
		 (List.map (function (s,e) -> Printf.sprintf "%d-%d" s e)
		    !found))*))
	  entry
  with Not_found -> ()

let validate_positions2 n p1 p2 =
  Printf.eprintf "validating %d: %d.%d - %d.%d\n" n
    (List.hd p1).line (List.hd p1).col
    (List.hd p2).line_end (List.hd p2).col_end;
  let p = List.hd p1 in
  let line = p.line in
  let col = p.col in
  let p = List.hd p2 in
  let line_end = p.line_end in
  let col_end = p.col_end in
  if line = line_end
  then validate line col (Some col_end)
  else
    begin
      validate line col None;
      validate line_end 0 (Some col_end);
      let middle_lines = iota (line+1) (line_end-1) in
      List.iter (function line -> validate line 0 None) middle_lines
    end

let validate_positions n p = validate_positions2 n p p

let infile p1 p2 =
  (List.hd p1).file = thefile && (List.hd p2).file = thefile

let validate_last_position n p =
  let p = List.hd p in
  let p2 = [{p with line = p.line_end; col = p.col_end - 1}] in
  validate_positions n p2

let update_term s ps =
  let pieces = Str.split (Str.regexp "\\b") s in
  let pieces =
    List.map
      (function x ->
	if List.mem x !local_functions
	then "__local__0__function__"
	else x)
      pieces in
  let env = try !(Hashtbl.find variables ps) with Not_found -> [] in
  let ienv = try !(Hashtbl.find inits ps) with Not_found -> [] in
  if env = [] && ienv = []
  then s
  else
    let line =
      List.map
	(function x ->
	  let ty = try List.assoc x env with Not_found -> x in
	  try "__from__0__" ^ (List.assoc x ienv)
	  with Not_found -> 
	    if String.get ty (String.length ty - 1) = '*'
	    then String.concat "__star__" (Str.split_delim (Str.regexp_string "*") ty)
	    else x)
	pieces in
    String.concat "" line

let isptr ty = String.get ty (String.length ty - 1) = '*'

let hasbrace s =
  try let _ = Str.search_forward (Str.regexp_string "{") s 0 in true
  with Not_found -> false

let add_function f =
  (if not(List.mem f !local_functions)
  then local_functions := f :: !local_functions);
  false

@@
identifier f : script:ocaml() { add_function f };
@@

f(...) { ... }

@re disable neg_if@
expression e;
expression list es;
iterator it;
position ps;
position pi : script:ocaml() { subrelevant(0,pi,true) };
statement S1,S2,S;
@@

(
(
if (e@pi) S1 else S2
|
while (e@pi) S1
|
it (es@pi) S1
|
for (e@pi;...;...) S1
|
for (...;e@pi;...) S1
|
for (...;...;e@pi) S1
|
return e@pi;
|
e@pi;
)
&
S@ps
)

@ini2@
type t; // : script:ocaml() { not(isptr(t)) };
idexpression t i;
identifier fn;
position re.ps;
position pa : script:ocaml() { funsubrelevant(6,pa,true) };
statement re.S;
@@

i@pa =@S@ps fn(...)

@script:ocaml@
fn << ini2.fn;
i << ini2.i;
ps << re.ps;
@@

hashaddv inits ps (i,fn)

@preini@
type t; // : script:ocaml() { not(isptr(t)) };
idexpression t i;
position re.ps,p != ini2.pa;
statement re.S;
@@

i@p@S@ps

@ini exists@
idexpression preini.t preini.i;
identifier fn;
position preini.p;
expression e;
@@

i = fn(...)
... when != i = e
i@p

@script:ocaml@
fn << ini.fn;
i << preini.i;
ps << re.ps;
@@

hashaddv inits ps (i,fn)

@tre@
type T;
idexpression T i;
position re.ps;
statement S;
@@

i@S@ps

@script:ocaml@
t << tre.T;
i << tre.i;
ps << re.ps;
@@

hashaddv variables ps (i,t)

@if_two_branches disable drop_else, neg_if@
position ps,pe,pl,p1,p2,p3;
expression e;
position pi : script:ocaml() { subrelevant(1,pi,false) };
statement S,S1,S2;
@@

(
(
if (e@pi) \( {@pe@p1 ... } \& S1@pl \) else \( {@p2 ... } \& S2@p3 \)
|
if (e@pi) \( {@pe@p1 ... } \& S1@pl \) else@p2 S2
|
if (e@pi)@pe S1 else@pl \( {@p2 ... } \& S2@p3 \)
|
if (e@pi)@pe S1 else@pl S2
)
&
S@ps
)

@script:ocaml@
e << if_two_branches.e;
ps << if_two_branches.ps;
pe << if_two_branches.pe;
pl << if_two_branches.pl;
p1 << if_two_branches.p1 = [];
p2 << if_two_branches.p2 = [];
p3 << if_two_branches.p3 = [];
@@

if infile ps pe
then
  begin
    validate_positions2 1 ps pe;
    (match (p1,p2) with
      ([],[]) -> validate_positions 2 pl (* no braces, else only *)
    | (_,[]) -> failwith "not possible"
    | ([],_) -> validate_positions2 3 pl p2 (* braces only in else case *)
    | (_,_) -> (* braces in then branch *)
	let plhd = List.hd pl in
	let pl = [{plhd with line = plhd.line_end; col = plhd.col_end - 1}] in
	validate_positions2 4 pl p2);
    (if not (p3 = []) then validate_last_position 5 p3);
    let e = update_term e ps in
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: if (%s)"
	    (List.hd ps).line (List.hd ps).col
	    (List.hd pe).line_end (List.hd pe).col_end e))
  end

@if_one_branch@
position ps,pe,p2;
expression e;
position pi : script:ocaml() { subrelevant(2,pi,false) };
statement S,S1;
@@

(
(
if (e@pi) \( {@pe ... } \& S1@p2 \)
|
if (e@pi)@pe S1
)
&
S@ps
)

@ecc exists@
position if_one_branch.pi;
expression e;
expression e1 != 0;
identifier l;
@@

if (e@pi) { ... \(goto l; \| return e1;\) }

@script:ocaml depends on ecc@
e << if_one_branch.e;
ps << if_one_branch.ps;
pe << if_one_branch.pe;
p2 << if_one_branch.p2 = [];
@@

if infile ps pe
then
  begin
    validate_positions2 6 ps pe;
    (if not (p2 = []) then validate_last_position 7 p2);
    let e = update_term e ps in
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: ECC: if (%s)"
	    (List.hd ps).line (List.hd ps).col
	    (List.hd pe).line_end (List.hd pe).col_end e))
  end

@script:ocaml depends on !ecc@
e << if_one_branch.e;
ps << if_one_branch.ps;
pe << if_one_branch.pe;
p2 << if_one_branch.p2 = [];
@@

if infile ps pe
then
  begin
    validate_positions2 8 ps pe;
    (if not (p2 = []) then validate_last_position 9 p2);
    let e = update_term e ps in
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: if (%s)"
	    (List.hd ps).line (List.hd ps).col
	    (List.hd pe).line_end (List.hd pe).col_end e))
  end

@while_one_branch@
position ps,pe,p2;
expression e;
position pi : script:ocaml() { subrelevant(3,pi,false) };
statement S,S1;
@@

(
(
while (e@pi) \( {@pe ... } \& S1@p2 \)
|
while (e@pi)@pe S1
)
&
S@ps
)

@script:ocaml@
e << while_one_branch.e;
ps << while_one_branch.ps;
pe << while_one_branch.pe;
p2 << while_one_branch.p2 = [];
@@

if infile ps pe
then
  begin
    validate_positions2 10 ps pe;
    (if not (p2 = [])
    then validate_last_position 11 p2);
    let e = update_term e ps in
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: while (%s)"
	    (List.hd ps).line (List.hd ps).col
	    (List.hd pe).line_end (List.hd pe).col_end e))
  end

@switch_one_branch@
position ps,pe;
expression e,e1;
position pi : script:ocaml() { subrelevant(3,pi,false) };
statement S;
@@

(
switch (e@pi) {@pe case e1: ... }
&
S@ps
)

@script:ocaml@
e << switch_one_branch.e;
ps << switch_one_branch.ps;
pe << switch_one_branch.pe;
@@

if infile ps pe
then
  begin
    validate_positions2 101 ps pe;
    validate_last_position 102 ps;
    let e = update_term e ps in
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: switch (%s)"
	    (List.hd ps).line (List.hd ps).col
	    (List.hd pe).line_end (List.hd pe).col_end e))
  end

@switch_case@
position p1,p3;
expression e;
position p2 : script:ocaml() { subrelevant(3,p2,false) };
@@

switch (...) { case@p1 e@p2:@p3 ... }

@script:ocaml@
e << switch_case.e;
p1 << switch_case.p1;
p3 << switch_case.p3;
@@

if infile p1 p3
then
  begin
    validate_positions2 100 p1 p3;
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: case %s :"
	    (List.hd p1).line (List.hd p1).col
	    (List.hd p3).line_end (List.hd p3).col_end e))
  end

@it_one_branch@
iterator it;
position ps,pe,p2;
expression list es;
position pi : script:ocaml() { subrelevant(4,pi,false) };
statement S,S1;
@@

(
(
it (es@pi) \( {@pe ... } \& S1@p2 \)
|
it (es@pi)@pe S1
)
&
S@ps
)

@script:ocaml@
it << it_one_branch.it;
es << it_one_branch.es;
ps << it_one_branch.ps;
pe << it_one_branch.pe;
_pi << it_one_branch.pi; // require change in param list
p2 << it_one_branch.p2 = [];
@@

if infile ps pe
then
  begin
    validate_positions2 12 ps pe;
    (if not (p2 = [])
    then validate_last_position 13 p2);
    let es = update_term es ps in
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: %s (%s)"
	    (List.hd ps).line (List.hd ps).col
	    (List.hd pe).line_end (List.hd pe).col_end it es))
  end

@for_one_branch@
position ps,pe,p2;
position pi : script:ocaml() { subrelevant(5,pi,false) };
statement S,S1;
expression e1,e2,e3;
@@

(
(
for (e1@pi;e2;e3) \( {@pe ... } \& S1@p2 \)
|
for (e1@pi; ;e3) \( {@pe ... } \& S1@p2 \)
|
for (e1@pi;e2; ) \( {@pe ... } \& S1@p2 \)
|
for (e1@pi; ; ) \( {@pe ... } \& S1@p2 \)
|
for (e1@pi;e2;e3)@pe S1
|
for (e1@pi; ;e3)@pe S1
|
for (e1@pi;e2; )@pe S1
|
for (e1@pi; ; )@pe S1
|
for (e1;e2@pi;e3) \( {@pe ... } \& S1@p2 \)
|
for ( ;e2@pi;e3) \( {@pe ... } \& S1@p2 \)
|
for (e1;e2@pi; ) \( {@pe ... } \& S1@p2 \)
|
for ( ;e2@pi; ) \( {@pe ... } \& S1@p2 \)
|
for (e1;e2@pi;e3)@pe S1
|
for ( ;e2@pi;e3)@pe S1
|
for (e1;e2@pi; )@pe S1
|
for ( ;e2@pi; )@pe S1
|
for (e1;e2;e3@pi) \( {@pe ... } \& S1@p2 \)
|
for ( ;e2;e3@pi) \( {@pe ... } \& S1@p2 \)
|
for (e1; ;e3@pi) \( {@pe ... } \& S1@p2 \)
|
for ( ; ;e3@pi) \( {@pe ... } \& S1@p2 \)
|
for (e1;e2;e3@pi)@pe S1
|
for ( ;e2;e3@pi)@pe S1
|
for (e1; ;e3@pi)@pe S1
|
for ( ; ;e3@pi)@pe S1
)
&
S@ps
)

@script:ocaml@
e1 << for_one_branch.e1 = "";
e2 << for_one_branch.e2 = "";
e3 << for_one_branch.e3 = "";
ps << for_one_branch.ps;
pe << for_one_branch.pe;
p2 << for_one_branch.p2 = [];
@@

if infile ps pe
then
  begin
    validate_positions2 14 ps pe;
    (if not (p2 = [])
    then validate_last_position 15 p2);
    let e1 = update_term e1 ps in
    let e2 = update_term e2 ps in
    let e3 = update_term e3 ps in
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: for (%s;%s;%s)"
	    (List.hd ps).line (List.hd ps).col
	    (List.hd pe).line_end (List.hd pe).col_end e1 e2 e3))
  end

@topde@
position ps : script:ocaml() { fullyrelevant(6,ps,false) };
declaration s : script:ocaml() { hasbrace(s)};
@@

s@ps

@script:ocaml@
s << topde.s;
ps << topde.ps;
@@

if infile ps ps
then
  begin
    validate_positions 16 ps;
    let s = update_term s ps in
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: %s"
	    (List.hd ps).line (List.hd ps).col
	    (List.hd ps).line_end (List.hd ps).col_end s))
  end

@topde2@
position ps : script:ocaml() { subrelevant(600,ps,false) };
declaration s : script:ocaml() { not(hasbrace(s)) };
@@

s@ps

@script:ocaml@
s << topde2.s;
ps << topde2.ps;
@@

if infile ps ps
then
  begin
    validate_positions 160 ps;
    let s = update_term s ps in
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: %s"
	    (List.hd ps).line (List.hd ps).col
	    (List.hd ps).line_end (List.hd ps).col_end s))
  end

@str@
position pi : script:ocaml() { subrelevant(9,pi,false) };
position ps;
identifier s;
field f;
@@

struct s { ...
f@pi@ps
... };

@script:ocaml@
ps << str.ps;
f << str.f;
s << str.s;
@@

if infile ps ps
then
  begin
    validate_positions 22 ps;
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: struct %s %s"
	    (List.hd ps).line (List.hd ps).col
	    (List.hd ps).line_end (List.hd ps).col_end s f))
  end

@istr2a@
position pi : script:ocaml() { subrelevant(192,pi,false) };
position ps;
position np : script:ocaml() { not(subrelevant(1922,np,false)) };
identifier x,y;
initializer f;
expression e;
type T;
@@

(
T x = { ..., .y@np= { ..., f@pi,@ps ..., },..., };
|
T x = { ..., [e@np]= { ..., f@pi,@ps ..., },..., };
)

@script:ocaml@
ps << istr2a.ps = [];
pi << istr2a.pi;
f << istr2a.f;
t << istr2a.T;
y << istr2a.y = "";
e << istr2a.e = "";
@@

  let ps = if ps = [] then pi else ps in
  if infile pi ps
  then
    begin
      validate_positions2 2222 pi ps;
      Printf.printf "%s\n"
	(escape
	   (Printf.sprintf "%d.%d-%d.%d: %s %s %s"
	      (List.hd pi).line (List.hd pi).col
	      (List.hd ps).line_end (List.hd ps).col_end t
	      (if y = "" then e else y)
	      f))
    end

@istr2@
position pi : script:ocaml() { subrelevant(92,pi,false) };
position ps;
identifier x;
initializer f;
type T;
@@

T x = { ...,
f@pi,@ps
..., };

@script:ocaml@
ps << istr2.ps = [];
pi << istr2.pi;
f << istr2.f;
t << istr2.T;
@@

  let ps = if ps = [] then pi else ps in
  if infile pi ps
  then
    begin
      validate_positions2 222 pi ps;
      Printf.printf "%s\n"
	(escape
	   (Printf.sprintf "%d.%d-%d.%d: %s %s"
	      (List.hd pi).line (List.hd pi).col
	      (List.hd ps).line_end (List.hd ps).col_end t f))
    end

@istr3a@
position pi : script:ocaml() { subrelevant(1920,pi,false) };
position ps;
position np : script:ocaml() { not(subrelevant(19202,np,false)) };
initializer f;
identifier y;
expression e;
declarer d;
@@

(
d(...) = { ..., .y@np= { ..., f@pi,@ps ..., },..., };
|
d(...) = { ..., [e@np]= { ..., f@pi,@ps ..., },..., };
)

@script:ocaml@
ps << istr3a.ps = [];
pi << istr3a.pi;
f << istr3a.f;
d << istr3a.d;
y << istr3a.y;
@@

  let ps = if ps = [] then pi else ps in
  if infile pi ps
  then
    begin
      validate_positions2 2223 pi ps;
      Printf.printf "%s\n"
	(escape
	   (Printf.sprintf "%d.%d-%d.%d: %s %s %s"
	      (List.hd pi).line (List.hd pi).col
	      (List.hd ps).line_end (List.hd ps).col_end d y f))
    end

@istr3@
position pi : script:ocaml() { subrelevant(920,pi,false) };
position ps;
initializer f;
declarer d;
@@

d(...) = { ...,
f@pi,@ps
..., };

@script:ocaml@
ps << istr3.ps = [];
pi << istr3.pi;
f << istr3.f;
d << istr3.d;
@@

  let ps = if ps = [] then pi else ps in
  if infile pi ps
  then
    begin
      validate_positions2 223 pi ps;
      Printf.printf "%s\n"
	(escape
	   (Printf.sprintf "%d.%d-%d.%d: %s %s"
	      (List.hd pi).line (List.hd pi).col
	      (List.hd ps).line_end (List.hd ps).col_end d f))
    end

@enm@
position pi : script:ocaml() { subrelevant(10,pi,false) };
position ps,pe;
identifier s;
identifier f;
expression e;
@@

(
enum s { ...,
f@pi@ps,@pe
..., }
|
enum { ...,
f@pi@ps,@pe
..., }
|
enum s { ...,
f@pi = e@ps,@pe
..., }
|
enum { ...,
f@pi = e@ps,@pe
..., }
|
enum s { ...,
f = e@pi@ps,@pe
..., }
|
enum { ...,
f = e@pi@ps,@pe
..., }
)

@script:ocaml@
pi << enm.pi;
ps << enm.ps;
pe << enm.pe = [];
f << enm.f;
s << enm.s = "";
@@

if infile pi ps
then
  begin
    let pe = if pe = [] then ps else pe in
    validate_positions2 24 pi pe;
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: enum %s%s"
	    (List.hd pi).line (List.hd pi).col
	    (List.hd pe).line_end (List.hd pe).col_end
	    (if s = "" then "" else (s^" "))
	    f))
  end

@de disable decl_init@
expression e;
position pi : script:ocaml() { funsubrelevant(6,pi,true) };
position ps;
declaration S;
type T;
identifier x;
constant c;
symbol true,false;
@@

(
T x;
|
T x = \(c\|true\|false\);
|
(
S@ps
&
e@pi
)
)

@tde@
type T;
idexpression T i;
position de.ps;
declaration de.S;
@@

(
S@ps
&
i
)

@script:ocaml@
t << tde .T;
i << tde.i;
ps << de.ps;
@@

hashaddv variables ps (i,t)

@ide2@
type t; // : script:ocaml() { not(isptr(t)) };
idexpression t i;
identifier fn;
position de.ps,pa;
declaration de.S;
@@

(
S@ps
&
i@pa = fn(...)
)

@script:ocaml@
fn << ide2.fn;
i << ide2.i;
ps << de.ps;
@@

hashaddv inits ps (i,fn)

@ide exists@
type t; // : script:ocaml() { not(isptr(t)) };
idexpression t i;
identifier fn;
position de.ps,p != ide2.pa;
declaration de.S;
@@

i = fn(...)
...
(
S@ps
&
i@p
)

@script:ocaml@
fn << ide.fn;
i << ide.i;
ps << de.ps;
@@

hashaddv inits ps (i,fn)

@fi@
position pi : script:ocaml() { relevant(pi) };
position pt : script:ocaml() { subrelevant(7,pt,false) };
position pl,pls,pr,ok;
parameter list params;
identifier i,f;
type T,T1;
@@

(
(
static@pls T@pt f (...) {@pr ... }
|
static@pls T f@pi (...) {@pr ... }
|
static@pls T f (...,T1@pt i,...) {@pr ... }
|
static@pls T f (...,T1 i@pi,...) {@pr ... }
|
T@pt@pl f (...) {@pr ... }
|
T@pl f@pi (...) {@pr ... }
|
T@pl f (...,T1@pt i,...) {@pr ... }
|
T@pl f (...,T1 i@pi,...) {@pr ... }
)
&
T f@ok (params) { ... }
)

@script:ocaml@
pl << fi.pl = [];
pls << fi.pls = [];
pr << fi.pr;
t << fi.T;
f << fi.f;
params << fi.params;
@@

let (pl,front) = if pl = [] then (pls,"static ") else (pl,"") in
if infile pl pr
then
  begin
    validate_positions2 17 pl pr;
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: %s%s %s(%s)"
	    (List.hd pl).line (List.hd pl).col
	    (List.hd pr).line_end (List.hd pr).col_end front t f params))
  end

@fi2 exists@
position fi.ok,p2;
identifier f;
@@

f@ok (...) { ... when any
 }@p2

@script:ocaml@
p2 << fi2.p2;
@@

validate_positions 18 p2

@rs@
position ps;
position pi : script:ocaml() { relevant(pi) };
position pt : script:ocaml() { subrelevant(8,pt,false) };
identifier l;
statement S;
declaration D;
type T;
identifier i;
position p : script:ocaml(i) { not(i = (List.hd p).current_element) };
expression e;
@@

// atomic statements
(
break@pi;@S@ps
|
continue@pi;@S@ps
|
return@pi;@S@ps
|
return e@pt;@S@ps
|
goto l@pi;@S@ps
|
l@pi:@S@ps
|
e@pt;@S@ps
|
T@pt i;@D@ps
|
T i@pi;@D@ps
|
T@pt i@p = e;@D@ps
|
T i@p@pi = e;@D@ps
|
T i@p = e@pt;@D@ps
)


@ehc exists@
statement rs.S;
declaration rs.D;
position rs.ps;
expression e1 != 0;
identifier l;
@@

(
if (...) { ... \(S@ps\|D@ps\) ... \(goto l; \| return e1;\) }
|
if (...) { ... \(goto l;@S@ps \| return e1;@S@ps\) }
|
l: ... \(S@ps\|D@ps\) ... \(goto l;@S@ps \| return e1;@S@ps\)
|
l: ... \(goto l;@S@ps \| return e1;@S@ps\)
)

@script:ocaml depends on ehc@
ps << rs.ps;
pt << rs.pt = [];
s << rs.S = "";
d << rs.D = "";
i << rs.i = "";
@@

let inside = i = "" || not(i = (List.hd ps).current_element) in
let inside2 =
  List.for_all (function p -> p.line >= (List.hd ps).line && p.line >= (List.hd ps).current_element_line) pt in
if inside && inside2 && infile ps ps
then
  begin
    validate_positions 19 ps;
    let s = if s = "" then d else s in
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: EHC: %s"
	    (List.hd ps).line (List.hd ps).col
	    (List.hd ps).line_end (List.hd ps).col_end s))
  end

@script:ocaml depends on !ehc@
ps << rs.ps;
pt << rs.pt = [];
s << rs.S = "";
d << rs.D = "";
i << rs.i = "";
@@

let inside = i = "" || not(i = (List.hd ps).current_element) in
let inside2 =
  (* see commit 8acb42070ec4c87a9baab5c7bac626030d5bef28 *)
  List.for_all (function p -> p.line >= (List.hd ps).line && p.line >= (List.hd ps).current_element_line) pt in
if inside && inside2 && infile ps ps
then
  begin
    validate_positions 20 ps;
    let s = if s = "" then d else s in
    let s = update_term s ps in
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: %s"
	    (List.hd ps).line (List.hd ps).col
	    (List.hd ps).line_end (List.hd ps).col_end s))
  end

@braces exists@
position pi1 : script:ocaml() { relevant(pi1) };
position pi2 : script:ocaml() { relevant(pi2) };
@@

{@pi1 ... }@pi2

@script:ocaml@
pi1 << braces.pi1;
pi2 << braces.pi2;
@@

if infile pi1 pi2
then
  begin
    validate_positions 200 pi1;
    validate_positions 210 pi2;
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: {"
	    (List.hd pi1).line (List.hd pi1).col
	    (List.hd pi1).line_end (List.hd pi1).col_end));
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: }"
	    (List.hd pi2).line (List.hd pi2).col
	    (List.hd pi2).line_end (List.hd pi2).col_end))
  end

@braces2 exists@
position pi1 : script:ocaml() { relevant(pi1) };
@@

{@pi1 ... }

@script:ocaml@
pi1 << braces2.pi1;
@@

if infile pi1 pi1
then
  begin
    validate_positions 200 pi1;
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: {"
	    (List.hd pi1).line (List.hd pi1).col
	    (List.hd pi1).line_end (List.hd pi1).col_end))
  end

@if1@
position p1 : script:ocaml() { relevant p1 };
@@

if (...) { ... }@p1

@script:ocaml@
p1 << if1.p1;
@@

if infile p1 p1
then
  begin
    validate_positions 60 p1;
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: }"
	    (List.hd p1).line (List.hd p1).col
	    (List.hd p1).line_end (List.hd p1).col_end))
  end

@if2@
position p1 : script:ocaml() { relevant p1 };
position p2 : script:ocaml() { relevant p2 };
statement S;
@@

if (...) { ... }@p1 else@p2 S

@script:ocaml@
p1 << if2.p1;
p2 << if2.p2;
@@

if infile p1 p2
then
  begin
    validate_positions2 61 p1 p2;
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: } else"
	    (List.hd p1).line (List.hd p1).col
	    (List.hd p2).line_end (List.hd p2).col_end))
  end

@tydecl2@
position pi : script:ocaml() { subrelevant(250,pi,false) };
position ps,pe,p0;
identifier s;
type T;
declarer d;
declaration sde;
@@

(
(
static@p0 const T s@pi = {@ps ... }@pe;
|
static@p0 const T@pi s = {@ps ... }@pe;
|
static@p0 const T@pi s[] = {@ps ... }@pe;
|
static@p0 T s@pi = {@ps ... }@pe;
|
static@p0 T@pi s[] = {@ps ... }@pe;
|
static@p0 T@pi s = {@ps ... }@pe;
|
static@p0 d@pi(...) = {@ps ... }@pe;
|
T@p0 s@pi = {@ps ... }@pe;
|
d@p0@pi(...) = {@ps ... }@pe;
)
&
sde
)

@script:ocaml@
p0 << tydecl2.p0;
ps << tydecl2.ps;
pe << tydecl2.pe;
sde << tydecl2.sde;
@@

if infile ps ps
then
  begin
    validate_positions2 250 p0 ps;
    validate_positions 260 pe;
    let s = List.hd (Str.split(Str.regexp_string "{") sde) ^ "{" in
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: %s"
	    (List.hd p0).line (List.hd p0).col
	    (List.hd ps).line_end (List.hd ps).col_end s))
  end

@tydecl@
position pi : script:ocaml() { relevant(pi) };
position ps,pe,p0;
identifier s;
type t;
@@

(
t
&
(
struct@p0 s@pi {@ps ... }@pe
|
union@p0 s@pi {@ps ... }@pe
|
enum@p0 s@pi {@ps ... }@pe
|
struct@p0@pi {@ps ... }@pe
|
union@p0@pi {@ps ... }@pe
|
enum@p0@pi {@ps ... }@pe
)
)

@script:ocaml@
p0 << tydecl.p0;
ps << tydecl.ps;
pe << tydecl.pe;
t << tydecl.t;
@@

if infile ps ps
then
  begin
    validate_positions2 25 p0 ps;
    validate_positions 26 pe;
    let s = List.hd (Str.split(Str.regexp_string "{") t) ^ "{" in
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: struct %s"
	    (List.hd p0).line (List.hd p0).col
	    (List.hd ps).line_end (List.hd ps).col_end s))
  end

@decl@
position pi : script:ocaml() { relevant(pi) };
position pe;
identifier s;
type T;
declarer d;
@@

(
T s = { ... }@pe;@pi
|
d(...) = { ... }@pe;@pi
|
struct s { ... }@pe;@pi
|
union s { ... }@pe;@pi
|
enum s { ... }@pe;@pi
|
struct { ... }@pe;@pi
|
union { ... }@pe;@pi
|
enum { ... }@pe;@pi
)

@script:ocaml@
pe << decl.pe;
pi << decl.pi;
@@

validate_positions2 27 pe pi

@tdeclarr@
type T;
idexpression T i;
position ps;
declarer d;
@@

d@ps(...,<+...i...+>,...);

@script:ocaml@
t << tdeclarr.T;
i << tdeclarr.i;
ps << tdeclarr.ps;
@@

hashaddv variables ps (i,t)

@def@
position pl,pr;
position pi : script:ocaml() { subrelevant(12,pi,false) };
identifier i;
expression e;
@@

// no way to put position variable on #define - will surely cause problems...
// it seems that the position variable is on the macro, but Coccinelle
// moves it to the #define.  This means that we can't find changes in macro
// names
(
#define i@pi@pl e@pr
|
#define i@pl e@pi@pr
)

@script:ocaml@
pl << def.pl;
pr << def.pr;
i << def.i;
e << def.e;
@@

if infile pl pr
then
  begin
    let pl = List.hd pl in
    let pl = {pl with Coccilib.col = 0} in
    validate_positions2 29 [pl] pr;
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: #define %s %s"
	    pl.line pl.col
	    (List.hd pr).line_end (List.hd pr).col_end i e))
  end

@def2@
position pl,pr;
position pi : script:ocaml() { subrelevant(13,pi,false) };
identifier i;
identifier list is;
expression e;
@@

// no way to put position variable on #define - will surely cause problems...
// as above, pl only gets attached to the #define
#define i(@pl is) e@pi@pr

@script:ocaml@
pl << def2.pl;
pr << def2.pr;
i << def2.i;
is << def2.is;
e << def2.e;
@@

if infile pl pr
then
  begin
    let pl = List.hd pl in
    let pl = {pl with Coccilib.col = 0} in
    validate_positions2 30 [pl] pr;
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: #define %s ( %s ) %s"
	    pl.line pl.col
	    (List.hd pr).line_end (List.hd pr).col_end i is e))
  end

@def3@
position pl,pr;
position pi : script:ocaml() { subrelevant(13,pi,false) };
identifier i;
identifier list is;
@@

// no way to put position variable on #define - will surely cause problems...
// as above, pl only gets attached to the #define
#define i(@pl is)@pi@pr

@script:ocaml@
pl << def3.pl;
pr << def3.pr;
i << def3.i;
is << def3.is;
@@

if infile pl pr
then
  begin
    let pl = List.hd pl in
    let pl = {pl with Coccilib.col = 0} in
    validate_positions2 30 [pl] pr;
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: #define %s ( %s )"
	    pl.line pl.col
	    (List.hd pr).line_end (List.hd pr).col_end i is))
  end

@inc@
position p : script:ocaml() { relevant(p) };
expression e;
@@

#include e@p

@script:ocaml@
p << inc.p;
e << inc.e;
@@

if infile p p
then
  begin
    let p = List.hd p in
    let p = {p with Coccilib.col = 0} in
    validate_positions2 31 [p] [p];
    Printf.printf "%s\n"
      (escape
	 (Printf.sprintf "%d.%d-%d.%d: #include %s"
	    p.line p.col p.line_end p.col_end e))
  end

@script:ocaml@
@@

flush stderr; flush stdout;
let allok =
  Hashtbl.fold
    (fun line pieces rest ->
      List.fold_left
	(fun rest (scol,ecol,found) ->
	  let ok = merge scol ecol !found in
	  (if not ok
	  then
	    begin
	      Printf.printf "problem at: %d %d %d\n" line scol ecol;
	      Printf.printf "found %s\n"
		(String.concat " "
		   (List.map (function (s,e) -> Printf.sprintf "%d-%d" s e)
		      !found))
	    end);
	  ok && rest)
	rest !pieces)
    relevant_lines true in
flush stderr; flush stdout;
if not allok
then (Printf.printf "Failed\n"; Coccilib.exit())
