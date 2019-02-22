(* TODO: make interval pivot undefined unless
 * the interval is ready to split *)


type ordering = LT | EQ | GT

type range_result =
  InvalidBounds |
  EmptyInterval |
  NewRange of float * float |
  Answer of float


type interval = {
  low : float;
  high : float;
  min : float;
  max : float;
  count : float;
  count_under : float;
  capacity : float;
  sum : float;
  left : interval option;
  right : interval option;
}


(* named argument conventions
 * high : float    -- (high end of interval)
 * low  : float    -- (low end of interval)
 * il   : interval -- an interval itself of the interval type
 * num  : float    -- numerical data
 * qtile : float   -- quantile
 * min  : float    -- observed minimum in interval
 * max  : float    -- observed maximum in interval
 * atally : float  -- tally from the above intervals
*)


val initial_size : float
val growth_factor : float


val interval_global_max : il:interval -> float

val interval_global_min : il:interval -> float

(* the nominal midpoint of an interval, disregarding the mean
 * the upper and lower bounds are allowed to be infinite
 *)
val midpoint : low:float -> high:float -> float option

val interval_midpoint : il:interval -> float option

(* the pivot of an interval is where the interval would be split *)
val interval_pivot : il:interval -> float



(* string representation of an interval *)
val string_of_interval : interval -> string

(* create an interval with no contents *)
val empty_interval : ?low:float -> ?high:float -> unit -> interval


(* make a singleton interval *)
val make_singleton : ?capacity:float -> ?low:float -> ?high:float -> num:float -> unit -> interval

(* add a new item to an interval, bypassing any and all capacity or
 * range checks *)
val interval_add_bypass : il:interval -> num:float -> interval

(* add a new item to a given interval, failing if the interval would be pushed over capacity *)
val interval_add_nonrec_exn : il:interval -> num:float -> interval
val interval_add_nonrec_opt : il:interval -> num:float -> interval option

(* recursively add a new item to the interval collection as a whole *)
val interval_add_one : il:interval -> num:float -> interval

(* add many items to an interval *)
val interval_add_list : il:interval -> nums:(float list) -> interval

(* create an interval out of a list of floats *)
val interval_of_float_list : nums:(float list) -> interval

val interval_cdf_nonrec_exn : min:float -> max:float -> num:float -> float

val interval_cdf_nonrec_opt : min:float -> max:float -> num:float -> float option

val interval_tally_below_opt : ?atally:float -> il:interval -> num:float -> unit -> float option

val interval_cdf_opt : il:interval -> num:float -> float option

val interval_get_new_range : higuess:float -> loguess:float -> il:interval -> qtile:float -> range_result

val interval_get_value_for_quantile : ?tries:int -> ?higuess:float -> ?loguess:float -> il:interval -> qtile:float -> unit -> float option


val interval_get_qth_value : il:interval -> qtile:float -> tol:float -> float option


val invert_real_func : func:(float -> float) -> tol:float -> lo:float -> hi:float -> target:float -> float option

val invert_real_func_aux : func:(float -> float) -> tol:float -> lo:float -> loOut:float -> hi:float -> hiOut:float -> target:float -> float option
