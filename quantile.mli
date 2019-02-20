
(* named argument conventions
 * high : float    -- (high end of interval)
 * low  : float    -- (low end of interval)
 * il   : interval -- an interval itself of the interval type
 * num  : float    -- numerical data
*)


val initial_size : float
val growth_factor : float

val midpoint : low:float -> high:float -> float option

type interval = {
  low : float;
  high : float;
  count : float;
  capacity : float;
  sum : float;
  left : interval option;
  right : interval option;
}

val string_of_interval : interval -> string

val empty_infinite_interval : interval

val empty_bounded_interval : low:float -> high:float -> interval

val interval_midpoint : il:interval -> float option

val interval_pivot : il:interval -> float

val make_singleton : num:float -> interval

val interval_add_bypass : il:interval -> num:float -> interval

val interval_add_nonrec_exn : il:interval -> num:float -> interval

val interval_add_nonrec_opt : il:interval -> num:float -> interval option

val interval_add_one : il:interval -> num:float -> interval

val interval_add_list : il:interval -> nums:(float list) -> interval

val interval_of_float_list : nums:(float list) -> interval
