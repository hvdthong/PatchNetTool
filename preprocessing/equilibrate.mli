val equilibrate :
    (string * Stables.is_stable * string * string *
       string * string list *
       (int*int*int*int*int)) list
  -> int -> string ->
    (string * Stables.is_stable * string * string *
       string * string list *
       (int*int*int*int*int)) list

val stables_and_nonstables :
    (string * Stables.is_stable * string * string *
       string * string list *
       (int*int*int*int*int)) list ->
      string ->
    (string * Stables.is_stable * string * string *
       string * string list *
       (int*int*int*int*int)) list

val nonstables_only :
    (string * Stables.is_stable * string * string *
       string * string list *
       (int*int*int*int*int)) list
  -> int -> string ->
    (string * Stables.is_stable * string * string *
       string * string list *
       (int*int*int*int*int)) list

val trivial_avoid :
    (string * Stables.is_stable * string * string *
       string * string list *
       (int*int*int*int*int)) list ->
      string ->
    (string * Stables.is_stable * string * string *
       string * string list *
       (int*int*int*int*int)) list

val trivial_select : int -> 'a list -> 'a list

