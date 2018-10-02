(*
 * This file is part of PatchNet, licensed under the terms of the GPL v2.
 * See copyright.txt in the PatchNet source code for more information.
 * The PatchNet source code can be obtained at
 * https://github.com/hvdthong/PatchNetTool
 *)

val run_get_words :
    ((string * string * string * string * string * string list) * 'c)
    list -> (string * int list * int list * string list) list

