(*
 * This file is part of PatchNet, licensed under the terms of the GPL v2.
 * See copyright.txt in the PatchNet source code for more information.
 * The PatchNet source code can be obtained at
 * https://github.com/hvdthong/PatchNetTool
 *)

val quiet : bool ref
val before : string ref
val after : string ref

val me : int ref

val getone : (string * string list) ->
  (string * (int * string * int * int * int * int * string) list) list *
    string list

exception Failed
