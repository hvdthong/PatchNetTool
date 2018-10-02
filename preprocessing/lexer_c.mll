{
(*
 * This file is part of PatchNet, licensed under the terms of the GPL v2.
 * See copyright.txt in the PatchNet source code for more information.
 * The PatchNet source code can be obtained at
 * https://github.com/hvdthong/PatchNetTool
 *)

(* Adapted from the lexer of Coccinelle, which has the following license: *)
(* Yoann Padioleau
 *
 * Copyright (C) 2002, 2006, 2007, 2008, 2009, Ecole des Mines de Nantes
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)

let ctr = ref 10

type tokens = Ttype | Tsign | Tstorage | Tvis | Tlifetime | Ttcons
| Tkwd | Tasm | Tinline | Tattribute | TattributeNoarg
| Trestrict | Tnamespace | Tnew | Tdelete | TComment | Tdecimal | Texec | Teof
| TCppDirectiveOther | TDefine | TUndef | TPragma | TInclude | TIfdef | TIfndef
| TIfdefelif | TEndif | TIfdefelse | TCppConcatOp | TIdent | TChar | TString
| TInt | TFloat | TUnknown | TDefParamVariadic
| TOCro | TCCro | TOPar | TCPar | TOBrace | TCBrace | TOp | TIncDec
| TAssign | TCompare | TBool | TBit | TEllipsis | TPtrOp | TDot | TComma
| TPtVirg | TWhy | TDotDot | TBang | TTilde 

type entry = Type of tokens | Info of string

let tok2c = function
    Ttype -> "Ttype"
  | Tsign -> "Tsign"
  | Tstorage -> "Tstorage"
  | Tvis -> "Tvis"
  | Tlifetime -> "Tlifetime"
  | Ttcons -> "Ttcons"
  | Tkwd -> "Tkwd"
  | Tasm -> "Tasm"
  | Tinline -> "Tinline"
  | Tattribute -> "Tattribute"
  | TattributeNoarg -> "TattributeNoarg"
  | Trestrict -> "Trestrict"
  | Tnamespace -> "Tnamespace"
  | Tnew -> "Tnew"
  | Tdelete -> "Tdelete"
  | TComment -> "TComment"
  | Tdecimal -> "Tdecimal"
  | Texec -> "Texec"
  | Teof -> "Teof"
  | TCppDirectiveOther -> "TCppDirectiveOther"
  | TDefine -> "TDefine"
  | TUndef -> "TUndef"
  | TPragma -> "TPragma"
  | TInclude -> "TInclude"
  | TIfdef -> "TIfdef"
  | TIfndef -> "TIfndef"
  | TIfdefelif -> "TIfdefelif"
  | TEndif -> "TEndif"
  | TIfdefelse -> "TIfdefelse"
  | TCppConcatOp -> "TCppConcatOp"
  | TIdent -> "TIdent"
  | TChar -> "TChar"
  | TString -> "TString"
  | TInt -> "TInt"
  | TFloat -> "TFloat"
  | TUnknown -> "TUnknown"
  | TDefParamVariadic -> "TDefParamVariadic"
  | TOCro -> "TOCro"
  | TCCro -> "TCCro"
  | TOPar -> "TOPar"
  | TCPar -> "TCPar"
  | TOBrace -> "TOBrace"
  | TCBrace -> "TCBrace"
  | TOp -> "TOp"
  | TIncDec -> "TIncDec"
  | TAssign -> "TAssign"
  | TCompare -> "TCompare"
  | TBool -> "TBool"
  | TBit -> "TBit"
  | TEllipsis -> "TEllipsis"
  | TPtrOp -> "TPtrOp"
  | TDot -> "TDot"
  | TComma -> "TComma"
  | TPtVirg -> "TPtVirg"
  | TWhy -> "TWhy"
  | TDotDot -> "TDotDot"
  | TBang -> "TBang"
  | TTilde -> "TTilde"

let entry2c = function Type t -> "tok-"^(tok2c t) | Info s -> s

let (tbl : (entry,(int * string list option ref)) Hashtbl.t) =
  Hashtbl.create 101

(* 0 is reserved for dummy values to pad vectors, so that they will have
   the same size *)

let generic = "__generic__0__name__"

let add_index entry use =
  let entry =
    match entry with
      Info s -> Info(String.lowercase_ascii s)
    | x -> x in
  let cell =
    try snd(Hashtbl.find tbl entry)
    with Not_found ->
      begin
	ctr := !ctr + 1;
	let ct = ref None in
	Hashtbl.add tbl entry (!ctr,ct);
	ct
      end in
  match use with
    Some commit ->
      (match !cell with
	None -> cell := Some [commit]
      | Some c ->
	  if List.mem commit c
	  then ()
	  else cell := Some (commit::c))
  | None -> ()

let gtr nm v =
  match v with
    None -> true
  | Some x -> List.length x >= 5

let from_index entry force =
  let entry =
    match entry with
      Info s -> Info(String.lowercase_ascii s)
    | x -> x in
  try
    let (res,ct) = Hashtbl.find tbl entry in
    if force || gtr entry !ct
    then Some res
    else Some (fst(Hashtbl.find tbl (Info generic)))
  with _ -> None

let from_text_index word =
  let entry = Info(String.lowercase_ascii word) in
  try
    let (res,ct) = Hashtbl.find tbl entry in
    if gtr entry !ct
    then Some res
    else None
  with _ -> None

let available entry =
  let entry =
    match entry with
      Info s -> Info(String.lowercase_ascii s)
    | x -> x in
  try Some(Hashtbl.find tbl entry)
  with Not_found -> None

let ops =
  ["+";"*";"-";"/";"%";"++";"--";
    "=";"-=";"+=";"*=";"/=";"&=";"|=";"^=";"<<=";">>=";
    "==";"!=";">=";"<=";"<";">";"&&";"||";">>";"<<";"&";"|";"^"]

let types =
  [Ttype; Tsign; Tstorage; Tvis; Tlifetime; Ttcons;
    Tkwd; Tasm; Tinline; Tattribute; TattributeNoarg;
    Trestrict; Tnamespace; Tnew; Tdelete; TComment; Tdecimal; Texec; Teof;
    TCppDirectiveOther; TDefine; TUndef; TPragma; TInclude; TIfdef; TIfndef;
    TIfdefelif; TEndif; TIfdefelse; TCppConcatOp; TIdent; TChar; TString;
    TInt; TFloat; TUnknown; TDefParamVariadic;
    TOCro; TCCro; TOPar; TCPar; TOBrace; TCBrace; TOp; TIncDec;
    TAssign; TCompare; TBool; TBit; TEllipsis; TPtrOp; TDot; TComma;
    TPtVirg; TWhy; TDotDot; TBang; TTilde]

let init _ =
  let _ = add_index (Info generic) None in
  List.iter (function x -> add_index (Info x) None) ops;
  List.iter (function x -> add_index (Type x) None) types

let current_commit = ref ""

let return tok info =
  match from_index (Type tok) true with
    Some tok ->
      let (atok1,atok2) =
	let i = Info info in
	match (from_index i false,from_index i true) with
	  (Some atok1,Some atok2) -> (atok1,atok2)
	| _ -> (tok,tok) in
      [Printf.sprintf "x%dx%dx%d" tok atok1 atok2]
  | _ -> failwith "not possible"

let print_dictionary o =
  let l = Hashtbl.fold (fun k v r -> (v,k) :: r) tbl [] in
  let l = List.sort compare l in
  List.iter
    (function ((n,ct),k) ->
      if gtr k !ct
      then
	match k with
	  Type tok -> Printf.fprintf o "%d: %s\n" n (tok2c tok)
	| Info s -> Printf.fprintf o "%d: %s\n" n s)
    l

let getnum num extra =
  let extra = (* clunky... *)
    match extra with
      [] -> ""
    | [y] -> Printf.sprintf "%c" y
    | [y1;y2] -> Printf.sprintf "%c%c" y1 y2
    | [y1;y2;y3] -> Printf.sprintf "%c%c%c" y1 y2 y3
    | _-> failwith "not supported" in
  if Str.string_match (Str.regexp "[0x]+$") num 0
  then "0"^extra
  else
    if Str.string_match (Str.regexp "[0x]*1$") num 0
    then "1"^extra
    else
      if num = "-1" || Str.string_match (Str.regexp "0xf+$") num 0
      then "-1"^extra
      else "__number__" (* don't expect absolute addresses or floats in code *)

(*****************************************************************************)
(*
 * Warning: ocamllex uses side effects on lexbuf.
 * For instance one must do
 *
 *  let info = tokinfo lexbuf in
 *  TComment (info +> tok_add_s (comment lexbuf))
 *
 * rather than
 *
 *   TComment (tokinfo lexbuf +> tok_add_s (comment lexbuf))
 *
 * because of the "weird" order of evaluation of OCaml.
 *
 *
 *
 * note: can't use Lexer_parser._lexer_hint here to do different
 * things, because now we call the lexer to get all the tokens
 * (tokens_all), and then we parse. So we can't have the _lexer_hint
 * info here. We can have it only in parse_c. For the same reason, the
 * typedef handling here is now useless.
 *)
(*****************************************************************************)

(*****************************************************************************)

exception Lexical of string

let tok     lexbuf  = Lexing.lexeme lexbuf

let error_radix s =
  ("numeric " ^ s ^ " constant contains digits beyond the radix:")

let hash_of_list xs =
  let h = Hashtbl.create 101 in
  List.iter (fun (k, v) -> Hashtbl.add h k v) xs;
  h

let keyword_table = hash_of_list [

  (* c: *)
  "void",   (function _ -> return Ttype "void");
  "char",   (function _ -> return Ttype "char");
  "short",  (function _ -> return Ttype "short");
  "int",    (function _ -> return Ttype "int");
  "long",   (function _ -> return Ttype "long");
  "float",  (function _ -> return Ttype "float");
  "double", (function _ -> return Ttype "double");
  "size_t", (function _ -> return Ttype "size_t");
  "ssize_t", (function _ -> return Ttype "ssize_t");
  "ptrdiff_t", (function _ -> return Ttype "ptrdiff_t");

  "unsigned", (function _ -> return Tsign "unsigned");
  "signed",   (function _ -> return Tsign "signed");

  "auto",     (function _ -> return Tstorage "auto");
  "register", (function _ -> return Tstorage "register");
  "extern",   (function _ -> return Tvis "extern");
  "static",   (function _ -> return Tvis "static");

  "const",    (function _ -> return Tlifetime "const");
  "volatile", (function _ -> return Tlifetime "volatile");

  "struct",  (function _ -> return Ttcons "struct");
  "union",   (function _ -> return Ttcons "union");
  "enum",    (function _ -> return Ttcons "enum");
  "typedef", (function _ -> return Ttcons "typedef");

  "if",      (function _ -> return Tkwd "if");
  "else",     (function _ -> return Tkwd "else");
  "break",   (function _ -> return Tkwd "break");
  "continue", (function _ -> return Tkwd "continue");
  "switch",  (function _ -> return Tkwd "switch");
  "case",     (function _ -> return Tkwd "case");
  "default", (function _ -> return Tkwd "default");
  "for",     (function _ -> return Tkwd "for");
  "do",      (function _ -> return Tkwd "do");
  "while",   (function _ -> return Tkwd "while");
  "return",  (function _ -> return Tkwd "return");
  "goto",    (function _ -> return Tkwd "goto");

  "sizeof", (function _ -> return Tkwd "sizeof");


  (* gccext: cppext: linuxext: synonyms *)
  "asm",     (function _ -> return Tasm "asm");
  "__asm__", (function _ -> return Tasm "asm");
  "__asm",   (function _ -> return Tasm "asm");

  "inline",     (function _ -> return Tinline "inline");
  "__inline__", (function _ -> return Tinline "inline");
  "__inline",   (function _ -> return Tinline "inline");

  "__attribute__", (function _ -> return Tattribute "attribute");
  "__attribute", (function _ -> return Tattribute "attribute");

  "typeof", (function _ -> return Tkwd "typeof");
  "__typeof__", (function _ -> return Tkwd "typeof");
  "__typeof", (function _ -> return Tkwd "typeof");

        (* found a lot in expanded code *)
  "__extension__", (function _ -> return TattributeNoarg "extension");


  (* gccext: alias *)
  "__signed__",     (function _ -> return Tsign "signed");

  "__const__",     (function _ ->  return Tlifetime "const");
  "__const",       (function _ ->  return Tlifetime "const");

  "__volatile__",  (function _ -> return Tlifetime "volatile");
  "__volatile",    (function _ -> return Tlifetime "volatile");

  (* windowsext: *)
  "__declspec", (function _ -> return Tattribute "declspec");

  "__stdcall", (function _ -> return TattributeNoarg "stdcall");
  "__cdecl",   (function _ -> return TattributeNoarg "cdecl");
  "WINAPI",    (function _ -> return TattributeNoarg "WINAPI");
  "APIENTRY",  (function _ -> return TattributeNoarg "APIENTRY");
  "CALLBACK",  (function _ -> return TattributeNoarg "CALLBACK");

  (* c99:  *)
  (* no just "restrict" ? maybe for backward compatibility they avoided
   * to use restrict which people may have used in their program already
   *)
  "__restrict",     (function _ -> return Trestrict "restrict");
  "__restrict__",   (function _ -> return Trestrict "restrict");

 ]



}

(*****************************************************************************)
let letter = ['A'-'Z' 'a'-'z' '_']
let extended_letter = ['A'-'Z' 'a'-'z' '_' ':' '<' '>' '~'](*for c++, not used*)
let digit  = ['0'-'9']

let cplusplus_ident = (letter | '$') (letter | digit | '$') *
let cplusplus_ident_ext = (letter | '~' | '$') (letter | digit | '~' | '$') *

(* not used for the moment *)
let punctuation = ['!' '\"' '#' '%' '&' '\'' '(' ')' '*' '+' ',' '-' '.' '/' ':'
		   ';' '<' '=' '>' '?' '[' '\\' ']' '^' '{' '|' '}' '~']
let space = [' ' '\t' '\n' '\r' '\011' '\012' ]
let additionnal = [ ' ' '\b' '\t' '\011' '\n' '\r' '\007' ]
(* 7 = \a = bell in C. this is not the only char allowed !!
 * ex @ and $ ` are valid too
 *)

let cchar = (letter | digit | punctuation | additionnal)

let sp = [' ' '\t']+
let spopt = [' ' '\t']*

let dec = ['0'-'9']
let oct = ['0'-'7']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']

let decimal = ('0' | (['1'-'9'] dec*))
let octal   = ['0']        oct+
let hexa    = ("0x" |"0X") hex+


let pent   = dec+
let pfract = dec+
let sign = ['-' '+']
let exp  = ['e''E'] sign? dec+
let real = pent exp | ((pent? '.' pfract | pent '.' pfract? ) exp?)
let ddecimal = ((pent? '.' pfract | pent '.' pfract? ))

let id = letter (letter | digit) *

(*****************************************************************************)
rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)

  (* note: this lexer generates tokens for comments!! so can not give
   * this lexer as-is to the parsing function. The caller must preprocess
   * it, e.g. by using techniques like cur_tok ref in parse_c.ml.
   *
   * update: we now also generate a separate token for newlines, so now
   * the caller may also have to reagglomerate all those commentspace
   * tokens if it was assuming that spaces were agglomerate in a single
   * token.
   *)

  | [' ' '\t' '\r' '\n' '\011' '\012' ]+
      { token lexbuf }
  | "/*"
      { failwith
	  (Printf.sprintf "%s: comments should be removed" !current_commit) }


  (* C++ comment are allowed via gccext, but normally they are deleted by cpp.
   * So need this here only when dont call cpp before.
   * note that we don't keep the trailing \n; it will be in another token.
   *)
  | "//" [^'\r' '\n' '\011']*
      { failwith
	  (Printf.sprintf "%s: comments should be removed" !current_commit) }

  (* ----------------------------------------------------------------------- *)
  (* cpp *)
  (* ----------------------------------------------------------------------- *)

  (* old:
   *   | '#'		{ endline lexbuf} // should be line, and not endline
   *   and endline = parse  | '\n' 	{ token lexbuf}
   *                        |	_	{ endline lexbuf}
   *)

  (* less?:
   *  have found a # #else  in "newfile-2.6.c",  legal ?   and also a  #/* ...
   *    => just "#" -> token {lexbuf} (that is ignore)
   *  il y'a 1 #elif  sans rien  apres
   *  il y'a 1 #error sans rien  apres
   *  il y'a 2  mov dede, #xxx    qui genere du coup exn car
   *  entouré par des #if 0
   *  => make as for comment, call a comment_cpp that when #endif finish the
   *   comment and if other cpp stuff raise exn
   *  il y'a environ 10  #if(xxx)  ou le ( est collé direct
   *  il y'a des include"" et include<
   *  il y'a 1 ` (derriere un #ifndef linux)
   *)



  (* ---------------------- *)
  (* misc *)
  (* ---------------------- *)

  (* bugfix: I want now to keep comments for the cComment study
   * so cant do:    sp [^'\n']+ '\n'
   * http://gcc.gnu.org/onlinedocs/gcc/Pragmas.html
   *)

  | "#" spopt "ident"   sp  [^'\n' '\r']* ('\n' | "\r\n")
      { return TCppDirectiveOther "#ident" }
  | "#" spopt "line"    sp  [^'\n' '\r']* ('\n' | "\r\n")
      { return TCppDirectiveOther "#line" }
  | "#" spopt "error"   sp  [^'\n' '\r']* ('\n' | "\r\n")
      { return TCppDirectiveOther "#error" }
  | "#" spopt "warning" sp  [^'\n' '\r']* ('\n' | "\r\n")
      { return TCppDirectiveOther "#warning" }
  | "#" spopt "abort"   sp  [^'\n' '\r']* ('\n' | "\r\n")
      { return TCppDirectiveOther "#abort" }

  | "#" [' ' '\t']* ('\n' | "\r\n")
      { return TCppDirectiveOther (tok lexbuf) }

  (* only after cpp, ex: # 1 "include/linux/module.h" 1 *)
  | "#" sp pent sp  '\"' [^ '\"']* '\"' (spopt pent)*  spopt ('\n' | "\r\n")
      { return TCppDirectiveOther (tok lexbuf) }



  (* ------------------------ *)
  (* #define, #undef, #pragma *)
  (* ------------------------ *)

  (* the rest of the lexing/parsing of define is done in fix_tokens_define
   * where we parse until a TCppEscapedNewline and generate a TDefEol
   *)
  | "#" [' ' '\t']* "define" { return TDefine "#define" }

  (* note: in some cases can have stuff after the ident as in #undef XXX 50,
   * but I currently don't handle it cos I think it's bad code.
   *)
  | "#" [' ' '\t']* "undef" { return TUndef "#undef" }

  (* note: in some cases can have stuff after the ident as in #undef XXX 50,
   * but I currently don't handle it cos I think it's bad code.
   *)
  | ("#" [' ' '\t']* "pragma") { return TPragma "#pragma" }

  (* ---------------------- *)
  (* #include *)
  (* ---------------------- *)

  (* The difference between a local "" and standard <> include is computed
   * later in parser_c.mly. So redo a little bit of lexing there; ugly but
   * simpler to generate a single token here.  *)
  | (("#" [' ''\t']* "include" [' ' '\t']*))
    (('\"' ([^ '\"']+) '\"' |
     '<' [^ '>']+ '>' |
      ['A'-'Z''_']+
    ) as filename)
      { return TInclude filename }
  (* gccext: found in glibc *)
  | (("#" [' ''\t']* "include_next" [' ' '\t']*))
    (('\"' ([^ '\"']+) '\"' |
     '<' [^ '>']+ '>' |
      ['A'-'Z''_']+
    ) as filename)
      { return TInclude filename }

  (* ---------------------- *)
  (* #ifdef *)
  (* ---------------------- *)

  (* The ifdef_mark will be set later in
   * Parsing_hacks.set_ifdef_parenthize_info
   * when working on the ifdef view.
   *)

  (* '0'+ because sometimes it is a #if 000 *)
  | "#" [' ' '\t']* "if" [' ' '\t']* '0'+ [^'\n']*
      { return TIfdef "TIfdefBool-false" }

  | "#" [' ' '\t']* "if" [' ' '\t']* '1' [^'\n']*
      { return TIfdef "TIfdefBool-true" }

 (* DO NOT cherry pick to lexer_cplusplus !!! often used for the extern "C" { *)
  | "#" [' ' '\t']* "if" sp "defined" sp "(" spopt "__cplusplus" spopt ")"
    [^'\n' '\r']*
      { return TIfdef "TIfdefMisc-false" }

 (* DO NOT cherry pick to lexer_cplusplus !!! *)
  | "#" [' ' '\t']* "ifdef" [' ' '\t']* "__cplusplus"   [^'\n']*
      (* don't want the final newline *)
      { return TIfdef "TIfdefMisc-false" }

  (* in glibc *)
  | "#" spopt ("ifdef"|"if") sp "__STDC__"   [^'\n']*
      (* hope that there are no comments in the ifdef line... *)
      { return TIfdef "TIfdefVersion-true" }


  (* linuxext: different possible variations (we do not manage all of them):

    #if LINUX_VERSION_CODE >= KERNEL_VERSION(2,4,0)
    #if LINUX_VERSION_CODE <= KERNEL_VERSION(2,4,2)
    #if LINUX_VERSION_CODE < KERNEL_VERSION(2,5,0)
    #if LINUX_VERSION_CODE > KERNEL_VERSION(2,3,0)
    #if LINUX_VERSION_CODE < 0x020600
    #if LINUX_VERSION_CODE >= 0x2051c
    #if (LINUX_VERSION_CODE < KERNEL_VERSION(2,5,0))
    #if !(LINUX_VERSION_CODE > KERNEL_VERSION(2,5,73))
    #if STREAMER_IOCTL && (LINUX_VERSION_CODE < KERNEL_VERSION(2,5,0))
    #if LINUX_VERSION_CODE >= KERNEL_VERSION(2,4,20)  &&  LINUX_VERSION_CODE < KERNEL_VERSION(2,5,0)
    #if LINUX_VERSION_CODE >= KERNEL_VERSION(2,4,20) && \
    # if defined(MODULE) && LINUX_VERSION_CODE >= KERNEL_VERSION(2,1,30)
    #if LINUX_VERSION_CODE > LinuxVersionCode(2,3,12)
    #elif LINUX_VERSION_CODE >= KERNEL_VERSION(2,1,93)
    #ifndef LINUX_VERSION_CODE
    #if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,2,0) || \
    (LINUX_VERSION_CODE > ASC_LINUX_VERSION(2,3,0) && \
    LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,4,0))
    #if (KERNEL_VERSION(2,4,0) > LINUX_VERSION_CODE)
    #if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,0)
    # if defined(MODULE) && LINUX_VERSION_CODE >= KERNEL_VERSION(2,1,30)

  *)

(*
  (* linuxext: must be before the generic rules for if and ifdef *)
  | "#" spopt "if" sp "("?  "LINUX_VERSION_CODE" sp (">=" | ">") sp [^'\n']*
      { return TIfdef "TIfdefVersion-true" }
  (* linuxext: *)
  | "#" spopt "if" sp "!" "("?  "LINUX_VERSION_CODE" sp (">=" | ">") sp [^'\n']*
  | "#" spopt "if" sp ['(']?  "LINUX_VERSION_CODE" sp ("<=" | "<") sp [^'\n']*
      { return TIfdef "TIfdefVersion-false" }
*)



  (* can have some ifdef 0  hence the letter|digit even at beginning of word *)
  | "#" [' ''\t']* "ifdef"  [' ''\t']+
    (((letter|digit) ((letter|digit)*)) as x) [^'\n']*
      { return TIfdef x }
  | "#" [' ''\t']* "ifndef" [' ''\t']+
     (((letter|digit) ((letter|digit)*)) as x) [^'\n']*
      { return TIfndef x }
  | "#" [' ''\t']* "if" [' ' '\t']+ ([^'\n']* as str_guard)
      { return TIfdef str_guard }
  | "#" [' ' '\t']* "if" (('(' [^'\n']*) as str_guard)
      { return TIfdef str_guard }
  | "#" [' ' '\t']* "elif" ([^'\n']* as str_guard)
      { return TIfdefelif str_guard }


  | "#" [' ''\t']* "endif"  [' ''\t']+ (letter|digit) ((letter|digit)*) [' ''\t']*
      { return TEndif "TEndif" }
  (* bugfix: can have #endif LINUX  but at the same time if I eat everything
   * until next line, I may miss some TComment which for some tools
   * are important such as aComment
   *)
  | "#" [' ' '\t']* "endif" (*[^'\n']* '\n'*)
      { return TEndif "TEndif" }
  (* can be at eof *)
  (*| "#" [' ' '\t']* "endif"                { return TEndif "#endif" }*)

  | "#" [' ' '\t']* "else" (*([' ' '\t' '\n'] | "\r\n")*)
      (* don't include trailing \n like for #if, etc
      doesn't seem needed from crx.cocci, but good to be uniform *)
      { return  TIfdefelse "TIfdefelse" }




  (* ---------------------- *)
  (* #define body *)
  (* ---------------------- *)

  (* only in cpp directives normally *)
  | "\\" ('\n' | "\r\n") { [] }

  (* We must generate separate tokens for #, ## and extend the grammar.
   * Note there can be "elaborated" idents in many different places, in
   * expression but also in declaration, in function name. So having 3 tokens
   * for an ident does not work well with how we add info in
   * ast_c. Was easier to generate just one token, just one info,
   * even if have later to reanalyse those tokens and unsplit. But then,
   * handling C++ lead to having not just a string for ident but something
   * more complex. Also when we want to parse elaborated function headers
   * (e.g. void METH(foo)(int x)), we need anyway to go from a string
   * to something more. So having also for C something more than just
   * string for ident is natural.
   *
   * todo: our heuristics in parsing_hacks rely on TIdent. So maybe
   * an easier solution would be to augment the TIdent type such as
   *   TIdent of string * info * cpp_ident_additionnal_info
   *
   * old:
   * |  id   ([' ''\t']* "##" [' ''\t']* id)+
   *   { let info = tokinfo lexbuf in
   *     TIdent (tok lexbuf, info)
   *   }
   * |  "##" spopt id
   *   { let info = tokinfo lexbuf in
   *     TIdent (tok lexbuf, info)
   *   }
   *
   *)
  (* cppext: string concatenation of idents, also ##args for variadic macro. *)
  | "##" { return TCppConcatOp "TCppConcatOp" }

  (* cppext: stringification.
   * bugfix: this case must be after the other cases such as #endif
   * otherwise take precedent.
   *)
  |  "#" spopt id
      { return TIdent (tok lexbuf) }
  (* the ... next to id, e.g. arg..., works with ##, e.g. ##arg *)
  | ((id as s)  "...")
      { return TDefParamVariadic s }





  (* ----------------------------------------------------------------------- *)
  (* C symbols *)
  (* ----------------------------------------------------------------------- *)
   (* stdC:
    ...   &&   -=   >=   ~   +   ;   ]
    <<=   &=   ->   >>   %   ,   <   ^
    >>=   *=   /=   ^=   &   -   =   {
    !=    ++   <<   |=   (   .   >   |
    %=    +=   <=   ||   )   /   ?   }
        --   ==   !    *   :   [
    recent addition:    <:  :>  <%  %>
    only at processing: %:  %:%: # ##
   *)


  | '['   { return TOCro "[" }   | ']' { return TCCro "]" }
  | '('   { return TOPar "(" }   | ')' { return TCPar ")" }
  | '{'   { return TOBrace "{" } | '}' { return TCBrace "}" }

  | '+'   { return TOp "+" }  | '*' { return TOp "*" }
  | '-'   { return TOp "-" }  | '/' { return TOp "/" }
  | '%'   { return TOp "%" }

  | "++"  { return TIncDec "++" } | "--" { return TIncDec "--" }

  | "="   { return TAssign "=" }

  | "-="  { return TAssign "-=" }
  | "+="  { return TAssign "+=" }
  | "*="  { return TAssign "*=" }
  | "/="  { return TAssign "/=" }
  | "%="  { return TAssign "%=" }
  | "&="  { return TAssign "&=" }
  | "|="  { return TAssign "|=" }
  | "^="  { return TAssign "^=" }
  | "<<=" { return TAssign "<<=" }
  | ">>=" { return TAssign ">>=" }

  | "==" { return TCompare "==" } | "!=" { return TCompare "!=" }
  | ">=" { return TCompare ">=" } | "<=" { return TCompare "<=" }
  | "<"  { return TCompare "<" }  | ">"  { return TCompare ">" }

  | "&&" { return TBool "&&" }    | "||" { return TBool "||" }
  | ">>" { return TBit ">>" }     | "<<" { return TBit "<<" }
  | "&"  { return TBit "&" }      | "|"  { return TBit "|" }
  | "^"  { return TBit "^" }
  | "..." { return TEllipsis "..." }
  | "->"   { return TPtrOp "->" }  | '.'  { return TDot "." }
  | ','    { return TComma "comma" }
  | ";"    { return TPtVirg ";" }
  | "?"    { return TWhy "?" }    | ":"   { return TDotDot ":" }
  | "!"    { return TBang "!" }   | "~"   { return TTilde "~" }

  | "<:" { return TOCro "[" }   | ":>" { return TCCro "]" }
  | "<%" { return TOBrace "{" } | "%>" { return TCBrace "}" }



  (* ----------------------------------------------------------------------- *)
  (* C keywords and ident *)
  (* ----------------------------------------------------------------------- *)

  (* StdC: must handle at least name of length > 509, but can
   * truncate to 31 when compare and truncate to 6 and even lowerise
   * in the external linkage phase
   *)
  | letter (letter | digit) *
      { let s = tok lexbuf in
        let tok =
	  try Some(Hashtbl.find keyword_table s)
	  with _ -> None in
	match tok with
	| Some f -> f()
	      
           (* parse_typedef_fix.
            *    if Lexer_parser.is_typedef s
            *    then TypedefIdent (s, info)
            *    else TIdent (s, info)
            *
            * update: now this is no more useful, cos
            * as we use tokens_all, it first parse all as an ident and
            * later transform an indent in a typedef. so the typedef job is
            * now done in parse_c.ml.
            *)

	| None -> return TIdent s
      }

  (* ----------------------------------------------------------------------- *)
  (* C constant *)
  (* ----------------------------------------------------------------------- *)

  | "'"
      { let s = char lexbuf   in
        return TChar s
      }
  | '\"'
      { let s = string lexbuf in
        return TString
	(Printf.sprintf "\"%s\"" (* undo if we change back to a number rep *)
	   (String.concat "" (Str.split (Str.regexp ",") s)))
      }
  (* wide character encoding, TODO L'toto' valid ? what is allowed ? *)
  | 'L' "'"
      { let s = char lexbuf   in
        return TChar s
      }
  | 'L' '\"'
      { let s = string lexbuf in
        return TString
	(Printf.sprintf "\"%s\"" (* undo if we change back to a number rep *)
	   (String.concat "" (Str.split (Str.regexp ",") s)))
      }


  (* Take care of the order ? No because lex tries the longest match. The
   * strange diff between decimal and octal constant semantic is not
   * understood too by refman :) refman:11.1.4, and ritchie.
   *)

  | decimal as x
      { return TInt (getnum x []) }
  | hexa as x
      { return TInt (getnum x []) }
  | octal as x
      { return TInt (getnum x []) }
  | (decimal as x) (['u' 'U'] as y)
      { return TInt (getnum x [y]) }
  | (hexa as x) (['u' 'U'] as y)
      { return TInt (getnum x [y]) }
  | (octal as x) (['u' 'U'] as y)
      { return TInt (getnum x [y]) }
  | (decimal as x) (['l' 'L'] as y)
      { return TInt (getnum x [y]) }
  | (hexa as x) (['l' 'L'] as y)
      { return TInt (getnum x [y]) }
  | (octal as x) (['l' 'L'] as y)
      { return TInt (getnum x [y]) }
  | (( decimal | hexa | octal) as x) (['l' 'L'] as y1) (['u' 'U'] as y2)
  | (( decimal | hexa | octal) as x) (['u' 'U'] as y1) (['l' 'L'] as y2)
      { return TInt (getnum x [y1;y2]) }
  | (( decimal | hexa | octal) as x) (['l' 'L'] as y1) (['l' 'L'] as y2)
      { return TInt (getnum x [y1;y2]) }
  | (( decimal | hexa | octal) as x) (['u' 'U'] as y1) (['l' 'L'] as y2)
      (['l' 'L'] as y3)
      { return TInt (getnum x [y1;y2;y3]) }
  | (real ['f' 'F']) as x { return TFloat x }
  | (real ['l' 'L']) as x { return TFloat x }
  | (real as x)           { return TFloat x }

  | ['0'] ['0'-'9']+
      { Printf.eprintf "LEXER: %s %s\n" (error_radix "octal") (tok lexbuf);
        return TUnknown "TUnknown"
      }
  | ("0x" |"0X") ['0'-'9' 'a'-'z' 'A'-'Z']+
      { Printf.eprintf "LEXER: %s %s\n" (error_radix "hexa") (tok lexbuf);
        return TUnknown "TUnknown"
      }


 (* !!! to put after other rules !!! otherwise 0xff
  * will be parsed as an ident.
  *)
  | ['0'-'9']+ letter (letter | digit) *
      { Printf.eprintf
	  "%s: LEXER: ZARB integer_string, certainly a macro: %s\n"
	  !current_commit (tok lexbuf);
        return TIdent (tok lexbuf)
      }

(* gccext: http://gcc.gnu.org/onlinedocs/gcc/Binary-constants.html *)
(*
 | "0b" ['0'-'1'] { TInt (((tok lexbuf)<!!>(??,??)) +> int_of_stringbits) }
 | ['0'-'1']+'b' { TInt (((tok lexbuf)<!!>(0,-2)) +> int_of_stringbits) }
*)


  (*------------------------------------------------------------------------ *)
  | eof { [("-1"); ("-1")] }

  | (_ as x)
      {
        return TUnknown ("TUnknown-"^String.make 1 x)
      }



(*****************************************************************************)
and char = parse
  | "'"                                { "" } (* allow empty char *)
  | (_ as x)                           { String.make 1 x ^ restchars lexbuf }
  (* todo?: as for octal, do exception  beyond radix exception ? *)
  | (("\\" (oct | oct oct | oct oct oct)) as x     ) { x ^ restchars lexbuf }
  (* this rule must be after the one with octal, lex try first longest
   * and when \7  we want an octal, not an exn.
   *)
  | (("\\x" ((hex | hex hex))) as x           )      { x ^ restchars lexbuf }
  | (("\\" (_ as v))           as x           )
	{
          (match v with (* Machine specific ? *)
          | 'n' -> ()  | 't' -> ()   | 'v' -> ()  | 'b' -> () | 'r' -> ()
          | 'f' -> () | 'a' -> ()
	  | '\\' -> () | '?'  -> () | '\'' -> ()  | '\"' -> ()
          | 'e' -> () (* linuxext: ? *)
	  | _ ->
              Printf.eprintf "%s: LEXER: unrecognised symbol in char: %s\n"
		!current_commit (tok lexbuf);
	  );
          x ^ restchars lexbuf
	}
  | _
      { Printf.eprintf "LEXER: unrecognised symbol in char: %s\n" (tok lexbuf);
        tok lexbuf ^ restchars lexbuf
      }

and restchars = parse
  | "'"                                { "" }
  | "\n"
      { Printf.eprintf "%s: LEXER: newline not expected in character\n"
	  !current_commit;
        tok lexbuf }
  | (_ as x)                           { String.make 1 x ^ restchars lexbuf }
  (* todo?: as for octal, do exception  beyond radix exception ? *)
  | (("\\" (oct | oct oct | oct oct oct)) as x     ) { x ^ restchars lexbuf }
  (* this rule must be after the one with octal, lex try first longest
   * and when \7  we want an octal, not an exn.
   *)
  | (("\\x" ((hex | hex hex))) as x           )      { x ^ restchars lexbuf }
  | (("\\" (_ as v))           as x           )
	{
          (match v with (* Machine specific ? *)
          | 'n' -> ()  | 't' -> ()   | 'v' -> ()  | 'b' -> () | 'r' -> ()
          | 'f' -> () | 'a' -> ()
	  | '\\' -> () | '?'  -> () | '\'' -> ()  | '\"' -> ()
          | 'e' -> () (* linuxext: ? *)
	  | _ ->
              Printf.eprintf "LEXER: unrecognised symbol in char: %s\n"
		(tok lexbuf);
	  );
          x ^ restchars lexbuf
	}
  | _
      { Printf.eprintf "LEXER: unrecognised symbol in char: %s\n" (tok lexbuf);
        tok lexbuf ^ restchars lexbuf
      }


(*****************************************************************************)

(* todo? factorise code with char ? but not same ending token so hard. *)
and string  = parse
  | '\"'                                      { "" }
  | "SSS"                                     { " " ^ string lexbuf }
  | "TTT"                                     { "\t" ^ string lexbuf }
  | (_ as x)                                  { String.make 1 x^string lexbuf}
  | ("\\" (oct | oct oct | oct oct oct)) as x { x ^ string lexbuf }
  | ("\\x" (hex | hex hex)) as x              { x ^ string lexbuf }
  | ("\\" (_ as v)) as x
       { (match v with (* Machine specific ? *)
         | 'n' -> ()  | 't' -> ()   | 'v' -> ()  | 'b' -> () | 'r' -> ()
         | 'f' -> () | 'a' -> ()
	 | '\\' -> () | '?'  -> () | '\'' -> ()  | '\"' -> ()
         | 'e' -> () (* linuxext: ? *)

         (* old: "x" -> 10 gccext ? todo ugly, I put a fake value *)

         (* cppext:  can have   \ for multiline in string too *)
         | '\n' -> Lexing.new_line lexbuf
         | _ ->
	     Printf.eprintf "LEXER: unrecognised symbol in string: %s\n"
	       (tok lexbuf);
	 );
          x ^ string lexbuf
       }

  | eof { Printf.eprintf "%s: LEXER: WIERD end of file in string\n"
	    !current_commit;
	  ""}
