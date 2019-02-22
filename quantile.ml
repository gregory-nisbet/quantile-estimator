(* TODO: can midpoint be NaN or pos inf or neg inf? *)

(* TODO: low and high are present just for debugging *)

(* TODO: come up with more compressed representation *)

open Printf

let within ~tol (x : float) (y : float) = begin
  ((x -. y) *. (x -. y)) < (tol *. tol)
end

let from_opt ?exn x = begin
  match (x, exn) with
  | (None, None) -> failwith "from_opt: cannot cast from None"
  | (None, Some exn) -> raise exn
  | (Some x, _) -> x
end

let initial_size = 8.0
let growth_factor = 2.0

let interval_initial_buffer_guess = 50

let int = int_of_float

let (||.) (x : float) y = max x y

let (&&.) (x : float) y = min x y


let value_default ~option ~def = begin
  match option with
  | None -> def
  | Some x -> x
end

(* compute the midpoint of an interval. *)
let midpoint ~low ~high = begin
  if low = neg_infinity then (
    if high > 0.0 then
      (Some 0.0)
    else
      (Some ((high *. 2.0) -. 1.0))
  ) else if high = infinity then (
    if low < 0.0 then
      (Some 0.0)
    else
      (Some ((low *. 2.0) +. 1.0))
  ) else if low <= high then
    Some (low +. ((high -. low) /. 2.0))
  else
    None
end

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

type direction = Left | Right

type range_result =
  InvalidBounds |
  EmptyInterval |
  NewRange of float * float |
  Answer of float

type ordering = LT | EQ | GT

exception MustCreateChildInterval of direction

exception EdfEmptyIntervalException

let rec interval_global_max ~il = begin
  let my_max = il.max in
  (match il.right with
  | None -> my_max
  | Some r -> my_max ||. interval_global_max ~il:r)
end

let rec interval_global_min ~il = begin
  let my_min = il.min in
  (match il.left with
  | None -> my_min
  | Some l -> my_min &&. interval_global_min ~il:l)
end

let string_of_interval il = begin
  let buf = Buffer.create interval_initial_buffer_guess in
  let rec aux ~ilopt = (
    match ilopt with
    | None -> (bprintf buf "nil"; ())
    | Some il -> (
      bprintf buf "((%f %f) :bounds (%f %f) :layer %d :total %d :cap %d :sum %f"
        il.min il.max
        il.low il.high
        (int il.count) (int il.count_under) (int il.capacity)
        il.sum;
      bprintf buf " ";
      aux ~ilopt:il.left;
      bprintf buf " ";
      aux ~ilopt:il.right;
      bprintf buf ")"
    )
  ) in
  (aux (Some il); Buffer.contents buf)
end

let empty_interval ?low:(l0=neg_infinity) ?high:(h0=infinity) () = {
  low = l0;
  high = h0;
  count = 0.0;
  count_under = 0.0;
  capacity = initial_size;
  sum = 0.0;
  min = infinity;
  max = neg_infinity;
  left = None;
  right = None;
}


let interval_midpoint ~il = begin
  midpoint ~low:il.low ~high:il.high
end


let make_singleton ?capacity:(c0=initial_size) ?low:(l0=neg_infinity) ?high:(h0=infinity) ~num () = {
  low = l0;
  high = h0;
  min = num;
  max = num;
  count = 1.0;
  count_under = 1.0;
  sum = num;
  capacity = c0;
  left = None;
  right = None;
}


let interval_pivot ~il = begin
  if il.count > 0.0 then
    il.sum /. il.count
  else (
    match interval_midpoint ~il with
    | None -> 0.0
    | Some d -> d
  )
end

let interval_add_bypass ~il ~num = begin
  { il with
    sum = il.sum +. num;
    min = il.min &&. num;
    max = il.max ||. num;
    count = il.count +. 1.0;
    count_under = il.count +. 1.0; }
end

let interval_add_nonrec_exn ~il ~num = begin
  if il.count <= (il.capacity -. 1.0) then
    interval_add_bypass ~il ~num
  else (
    let pivot = interval_pivot ~il in
    (if (num <= pivot) then
      raise (MustCreateChildInterval Left)
    else
      raise (MustCreateChildInterval Right)))
end

let interval_add_nonrec_opt ~il ~num = begin
  match interval_add_nonrec_exn ~il ~num with
  | res -> Some res
  | exception MustCreateChildInterval _ -> None
end

let rec interval_add_one ~il ~num = begin
  match interval_add_nonrec_exn ~il ~num with
  | res -> res
  | exception (MustCreateChildInterval dir) -> (
    let pivot = interval_pivot ~il in
    (match (dir, (il.left, il.right)) with
    | (Left, (None, _)) -> (
      let new_left = make_singleton ~capacity:(il.capacity *. growth_factor) ~low:il.low ~high:pivot ~num () in
      { il with count_under = il.count_under +. 1.0 ; left = Some new_left }
    )
    | (Left, (Some l, _)) -> (
      { il with count_under = il.count_under +. 1.0 ; left = Some (interval_add_one ~il:l ~num) }
    )
    | (Right, (_, None)) -> (
      let new_right = make_singleton ~capacity:(il.capacity *. growth_factor) ~low:pivot ~high:il.high ~num () in
      { il with count_under = il.count_under +. 1.0 ; right = Some new_right }
    )
    | (Right, (_, Some r)) -> (
      { il with count_under = il.count_under +. 1.0 ; right = Some (interval_add_one ~il:r ~num) }
    )
    )
  )
end

let rec interval_add_list ~il ~nums = begin
  let out_ref = ref il in
  (List.iter (fun num -> out_ref := interval_add_one ~il:(!out_ref) ~num) nums;
  (!out_ref))
end

let interval_of_float_list ~nums = begin
  let il = empty_interval () in
  interval_add_list ~il ~nums
end


let interval_cdf_nonrec_exn ~min ~max ~num = begin
  if min = infinity || max = neg_infinity then
    raise EdfEmptyIntervalException
  else if min = max then (
    (* in this case, there's only one item stored,
     * so the inequalities are strict rather than weak *)
    if num < min then 0.0
    else if num > min then 1.0
    else if num = min then 0.5
    else failwith "interval cdf failed, probably as a result of NaN"
  )
  else if num <= min then 0.0
  else if num >= max then 1.0
  else (
    (num -. min) /. (max -. min)
  )
end

let interval_cdf_nonrec_opt ~min ~max ~num = begin
  try Some (interval_cdf_nonrec_exn ~min ~max ~num)
  with EdfEmptyIntervalException -> None
end

let rec interval_tally_below_opt ?atally ~il ~num () = begin
  let skipped_left_contribution = (match (il.left) with
  | Some l -> l.count_under
  | None -> 0.0) in
  let a0 = (match atally with
  | None -> 0.0
  | Some atally -> atally
  ) in
  match interval_cdf_nonrec_opt ~min:il.min ~max:il.max ~num with
  | None -> atally
  | Some prob -> (
    (* our contribution to the overall expected number of items below us
     * is the expected number of items below us on this level specifically. *)
    let contribution = il.count *. prob in
    let pivot = interval_pivot ~il in
    let look_left = num <= pivot in
    let new_tally = a0 +. contribution in
    (match (look_left, (il.left, il.right)) with
    | (true, (None, _))    -> Some (skipped_left_contribution +. new_tally)
    | (true, (Some l, _))  -> interval_tally_below_opt ~atally:new_tally ~il:l ~num ()
    | (false, (_, None))   -> Some (skipped_left_contribution +. new_tally)
    | (false, (_, Some r)) -> interval_tally_below_opt ~atally:(skipped_left_contribution +. new_tally) ~il:r ~num ()
    )
  )
end

let interval_cdf_opt ~il ~num = begin
  match interval_tally_below_opt ~il ~num () with
  | None -> None
  | Some tally -> Some (tally /. il.count_under)
end

let interval_quantile_cmp_step_opt ~il ~qtile ~guess = begin
  match interval_cdf_opt ~il ~num:guess with
  | None -> None
  | Some q -> (
    if qtile > q then Some LT
    else if qtile < q then Some GT
    else Some EQ
  )
end


let interval_get_new_range ~higuess ~loguess ~il ~qtile = begin
  if higuess = loguess then (
    let cmp_opt = interval_quantile_cmp_step_opt ~il ~qtile ~guess:higuess in
    (match cmp_opt with
    | None -> EmptyInterval
    | Some LT -> InvalidBounds
    | Some GT -> InvalidBounds
    | Some EQ -> Answer loguess)
  )
  else (
    let guess = (higuess +. loguess) /. 2.0 in
    let hi = interval_quantile_cmp_step_opt ~il ~qtile ~guess:higuess in
    let lo = interval_quantile_cmp_step_opt ~il ~qtile ~guess:loguess in
    let cmp_opt = interval_quantile_cmp_step_opt ~il ~qtile ~guess in
    match (lo, hi, cmp_opt) with
    | (  None,     None,    None) -> EmptyInterval
    | (Some EQ,       _,       _) -> Answer loguess
    | (Some GT,       _,       _) -> InvalidBounds
    | (      _, Some EQ,       _) -> Answer higuess
    | (      _, Some LT,       _) -> InvalidBounds
    | (Some LT, Some GT, cmp_opt) -> (
      match cmp_opt with
      | None -> failwith "impossible"
      | Some LT -> NewRange (guess, higuess)
      | Some EQ -> Answer guess
      | Some GT -> NewRange (loguess, higuess))
    | None, Some _, _ -> failwith "impossible"
    | Some _, None, _ -> failwith "impossible"
    | None, None, Some _ -> failwith "impossible")
end



let rec interval_get_value_for_quantile ?tries ?higuess ?loguess ~il ~qtile () = begin
  let tries = (match tries with
  | None -> 16
  | Some t -> t) in
  let higuess = (match higuess with
  | None -> interval_global_max ~il
  | Some h -> h) in
  let loguess = (match loguess with
  | None -> interval_global_min ~il
  | Some l -> l) in
  let guess = (higuess +. loguess) /. 2.0 in
  (if tries >= 0 then (
    let guess = (higuess +. loguess) /. 2.0 in
    let cmp_opt = interval_quantile_cmp_step_opt ~il ~qtile ~guess in
    let tries' = tries - 1 in
    (match (cmp_opt, (tries >= 0)) with
    | (None, _) -> None
    | (_, false) -> Some guess
    | ((Some GT), true) -> interval_get_value_for_quantile ~tries:tries' ~higuess:guess ~loguess ~il ~qtile ()
    | ((Some EQ), true) -> Some guess
    | ((Some LT), true) -> interval_get_value_for_quantile ~tries:tries' ~higuess ~loguess:guess ~il ~qtile ()))
  else
    Some guess)
end

let rec invert_real_func_aux ~func ~tol ~lo ~loOut ~hi ~hiOut ~target = begin
  if within ~tol loOut target then Some lo
  else if within ~tol hiOut target then Some hi
  else if loOut >= hiOut then None
  else if target > hiOut then None
  else if target < loOut then None
  else (
    let guess = (hi +. lo) /. 2.0 in
    let guess_out = func guess in
    (if guess_out > hiOut then None
    else if guess_out < loOut then None
    else if guess_out = target then Some guess
    else if guess_out < target then (
      invert_real_func_aux ~func ~tol ~lo:guess ~loOut:guess_out ~hi ~hiOut ~target)
    else if guess_out > target then (
      invert_real_func_aux ~func ~tol ~lo ~loOut ~hi:guess ~hiOut:guess_out ~target)
    else (failwith "impossible")))
end

let invert_real_func ~func ~tol ~lo ~hi ~target = begin
  let loOut = func lo in
  let hiOut = func hi in
  (if within ~tol loOut target then Some lo
  else if within ~tol hiOut target then Some hi
  else if loOut >= hiOut then None
  else if target > hiOut then None
  else if target < loOut then None
  else invert_real_func_aux ~func ~tol ~lo ~loOut ~hi ~hiOut ~target)
end

exception SmallInterval

let interval_get_qth_value ~il ~qtile ~tol = begin
  let func num = from_opt ~exn:SmallInterval (interval_cdf_opt ~il ~num) in
  let lo = interval_global_min il in
  let hi = (interval_global_max il *. (1.00001)) in
  (match invert_real_func ~func ~target:qtile ~tol ~lo ~hi with
  | None -> None
  | exception SmallInterval -> None
  | Some x -> Some x)
end
