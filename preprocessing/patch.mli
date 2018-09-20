type from_required = Stable | RC | NoFrom

val get_patches : string -> from_required -> string * string -> string ->
  (int * int * int) option ->
    (string * string * string * string * string * string * string *
       string option * string list * (int*int*int*int*int)) list

val get_stable_commits : string ->
    (string * string * string * string * string * string * string *
       string option * string list * (int*int*int*int*int)) list

val line_limit : int ref

