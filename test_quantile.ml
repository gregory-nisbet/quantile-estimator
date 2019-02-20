open OUnit
open Quantile
open Printf

let from_opt x = begin
  match x with
  | None -> failwith ""
  | Some x -> x
end

let test_midpoint () = begin
  (* [0, 1] *)
  assert_equal (Some 0.5) (midpoint ~low:0.0 ~high:1.0);
  (* [1, 0] malformed *)
  assert_equal None (midpoint ~low:1.0 ~high:0.0);
  (* [-inf, inf] should be zero *)
  assert_equal (Some 0.0) (midpoint ~low:neg_infinity ~high:infinity);
  (* [-inf, -1] should be -3 *)
  assert_equal (Some (-3.0)) (midpoint ~low:neg_infinity ~high:(-1.0));
end


let test_singleton_interval () = begin
  let il = (make_singleton ~num:4.5) in
  let expected = "((-inf inf) (1 8) 4.500000 nil nil)" in
  (assert_equal expected (string_of_interval il);
  assert_equal (Some 0.0) (interval_midpoint ~il);
  assert_equal (4.5) (interval_pivot ~il))
end


let test_empty_infinite_interval () = begin
  (* VERY particular. six decimal places for sum *)
  let expected = "((-inf inf) (0 8) 0.000000 nil nil)" in
  (assert_equal expected (string_of_interval empty_infinite_interval);
  assert_equal (Some 0.0) (interval_midpoint ~il:empty_infinite_interval))
end

let test_interval_add_bypass () = begin
  let expected = "((-inf inf) (2 8) 4.000000 nil nil)" in
  let il = empty_infinite_interval in
  let il = interval_add_bypass ~il ~num:1.0 in
  let il = interval_add_bypass ~il ~num:3.0 in
  assert_equal expected (string_of_interval il)
end

let test_interval_add_many_1 () = begin
  let expected = "((-inf inf) (1 8) 10.000000 nil nil)" in
  let nums = [ 10.0 ] in
  let il = interval_of_float_list nums in
  assert_equal expected (string_of_interval il)
end

let test_interval_add_many_2 () = begin
  let expected = "((-inf inf) (2 8) 0.000000 nil nil)" in
  let nums = [ 10.0; (-10.0) ] in
  let il = interval_of_float_list nums in
  let msg = string_of_interval il in
  (eprintf "%s\n" msg;
  assert_equal expected msg)
end

let test_interval_add_many_3 () = begin
  (* expected may appear inscrutable, but I've vetted it
   * there are 8 items in the top interval, consisting of the entire real line.
   * all of them are ten, so their sum is 80.
   *
   * the ninth ten should be rounded down, and 5.0 is less than 10, so that should appear in the lower subinterval
   * the 99.0 should be the only thing in the upper interval
   *)
  let expected = "((-inf inf) (8 8) 80.000000 ((-inf 10.000000) (2 16) 15.000000 nil nil) ((10.000000 inf) (1 16) 99.000000 nil nil))" in
  let nums = [ 10.0; 10.0; 10.0;
               10.0; 10.0; 10.0;
               10.0; 10.0; 10.0;
                5.0; 99.0;] in
  let il = interval_of_float_list nums in
  let msg = string_of_interval il in
  (eprintf "%s\n" msg;
  assert_equal expected msg)
end


let suite = "Test Quantile" >::: [
  "test_midpoint" >:: test_midpoint;
  "test_empty_infinite_interval" >:: test_empty_infinite_interval;
  "test_singleton_interval" >:: test_singleton_interval;
  "test_interval_add_nonrec" >:: test_interval_add_bypass;
  "test_interval_add_many_1" >:: test_interval_add_many_1;
  "test_interval_add_many_2" >:: test_interval_add_many_2;
  "test_interval_add_many_3" >:: test_interval_add_many_3
]

let _ =
run_test_tt_main suite
