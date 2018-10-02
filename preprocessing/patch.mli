(*
 * This file is part of PatchNet, licensed under the terms of the GPL v2.
 * See copyright.txt in the PatchNet source code for more information.
 * The PatchNet source code can be obtained at
 * https://github.com/hvdthong/PatchNetTool
 *)

val get_commits : string ->
    (string * string * string * string * string * string * string * string *
       string option * string list) list
