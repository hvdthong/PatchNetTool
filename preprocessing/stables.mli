type is_stable = Stable | NotStable | Unknown

val stable_subjects : (string * string, string list ref) Hashtbl.t
val stable_commits  : (string, string list ref) Hashtbl.t

val old_versions : (string * string) list
val new_versions : (string * string) list
val rcs : (string * string) list

val stables : (string * string) list -> (string * string) list ->
  (int * int * int)

