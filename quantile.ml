open Printf

let initial_size = 8.0
let growth_factor = 2.0

let interval_initial_buffer_guess = 50

let int = int_of_float

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
  count : float;
  capacity : float;
  sum : float;
  left : interval option;
  right : interval option;
}

type add_result =
  | AddToLeft
  | AddToRight
  | Interval of interval

type direction = Left | Right

exception MustCreateChildInterval of direction

let string_of_interval il = begin
  let buf = Buffer.create interval_initial_buffer_guess in
  let rec aux ~ilopt = (
    match ilopt with
    | None -> (bprintf buf "nil"; ())
    | Some il -> (
      bprintf buf "((%f %f) (%d %d) %f"
        il.low il.high
        (int il.count) (int il.capacity)
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

let empty_infinite_interval = {
  low = neg_infinity;
  high = infinity;
  count = 0.0;
  capacity = initial_size;
  sum = 0.0;
  left = None;
  right = None;
}

let empty_bounded_interval ~low ~high = {
  low;
  high;
  count = 0.0;
  capacity = initial_size;
  sum = 0.0;
  left = None;
  right = None;
}

let interval_midpoint ~il = begin
  midpoint ~low:il.low ~high:il.high
end

let make_singleton ~num = {
  low = neg_infinity;
  high = infinity;
  count = 1.0;
  sum = num;
  capacity = initial_size;
  left = None;
  right = None;
}

let make_bounded_singleton ?capacity ~num ~low ~high = begin
  let capacity = value_default ~option:capacity ~def:initial_size in
  {
    low;
    high;
    count = 1.0;
    sum = num;
    capacity = capacity;
    left = None;
    right = None;
  }
end

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
    count = il.count +. 1.0 }
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
      let new_left = make_bounded_singleton ~capacity:(il.capacity *. growth_factor) ~low:il.low ~high:pivot ~num in
      { il with left = Some new_left }
    )
    | (Left, (Some l, _)) -> (
      { il with left = Some (interval_add_one ~il:l ~num) }
    )
    | (Right, (_, None)) -> (
      let new_right = make_bounded_singleton ~capacity:(il.capacity *. growth_factor) ~low:pivot ~high:il.high ~num in
      { il with right = Some new_right }
    )
    | (Right, (_, Some r)) -> (
      { il with right = Some (interval_add_one ~il:r ~num) }
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
  let il = empty_infinite_interval in
  interval_add_list ~il ~nums
end
