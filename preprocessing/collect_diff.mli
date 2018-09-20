val quiet : bool ref
val before : string ref
val after : string ref

val me : int ref

val getone : (string * string list) ->
  (string * (int * string * int * int * int * int * string) list) list *
    string list

exception Failed
