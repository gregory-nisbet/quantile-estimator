open OUnit
open Quantile
open Printf

let within ~tol (x : float) (y : float) = begin
  ((x -. y) *. (x -. y)) < (tol *. tol)
end

let from_opt ?msg x = begin
  match (x, msg) with
  | None, Some msg -> failwith msg
  | None, None -> failwith "from_opt"
  | Some x, _ -> x
end

let ( *.. ) x y = (float x) *. y

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
  let il = (make_singleton ~num:4.5 ()) in
  let expected = "((4.500000 4.500000) :bounds (-inf inf) :layer 1 :total 1 :cap 8 :sum 4.500000 nil nil)" in
  let msg = string_of_interval il in
  (assert_equal expected msg;
  assert_equal (Some 0.0) (interval_midpoint ~il);
  assert_equal (4.5) (interval_pivot ~il))
end


let test_empty_infinite_interval () = begin
  let expected = "((inf -inf) :bounds (-inf inf) :layer 0 :total 0 :cap 8 :sum 0.000000 nil nil)" in
  (assert_equal expected (string_of_interval (empty_interval ()));
  assert_equal (Some 0.0) (interval_midpoint ~il:(empty_interval ())))
end

let test_interval_add_bypass () = begin
  let expected = "((1.000000 3.000000) :bounds (-inf inf) :layer 2 :total 2 :cap 8 :sum 4.000000 nil nil)" in
  let il = empty_interval () in
  let il = interval_add_bypass ~il ~num:1.0 in
  let il = interval_add_bypass ~il ~num:3.0 in
  let msg = string_of_interval il in
  assert_equal expected msg
end

let test_interval_add_many_1 () = begin
  let expected = "((10.000000 10.000000) :bounds (-inf inf) :layer 1 :total 1 :cap 8 :sum 10.000000 nil nil)" in
  let nums = [ 10.0 ] in
  let il = interval_of_float_list nums in
  assert_equal expected (string_of_interval il)
end

let test_interval_add_many_2 () = begin
  let expected = "((-10.000000 10.000000) :bounds (-inf inf) :layer 2 :total 2 :cap 8 :sum 0.000000 nil nil)" in
  let nums = [ 10.0; (-10.0) ] in
  let il = interval_of_float_list nums in
  let msg = string_of_interval il in
  (assert_equal expected msg)
end

let test_interval_add_many_3 () = begin
  let expected = "((10.000000 10.000000) :bounds (-inf inf) :layer 8 :total 11 :cap 8 :sum 80.000000 ((5.000000 10.000000) :bounds (-inf 10.000000) :layer 2 :total 2 :cap 16 :sum 15.000000 nil nil) ((99.000000 99.000000) :bounds (10.000000 inf) :layer 1 :total 1 :cap 16 :sum 99.000000 nil nil))" in
  let nums = [ 10.0; 10.0; 10.0;
               10.0; 10.0; 10.0;
               10.0; 10.0; 10.0;
                5.0; 99.0;] in
  let il = interval_of_float_list nums in
  let msg = string_of_interval il in
  (assert_equal expected msg)
end


let test_interval_tally_1 () = begin
  let nums = [ ] in
  let il = interval_of_float_list nums in
  ((let x = interval_tally_below_opt ~il ~num:(-100.0) () in
  assert_equal x None);
  (let x = interval_tally_below_opt ~il ~num:(0.0) () in
  assert_equal x None);)
end


let test_interval_tally_2_a () = begin
  let nums = [ 0.0 ] in
  let il = interval_of_float_list nums in
  (let x = interval_tally_below_opt ~il ~num:(-100.0) () in
  assert_equal x (Some 0.0))
end


let test_interval_tally_2_b () = begin
  let nums = [ 0.0 ] in
  let il = interval_of_float_list nums in
  (let x = interval_tally_below_opt ~il ~num:(0.0) () in
  assert_equal x (Some 0.5))
end


let test_interval_tally_2_c () = begin
  let nums = [ 0.0 ] in
  let il = interval_of_float_list nums in
  (let x = interval_tally_below_opt ~il ~num:(100.0) () in
  assert_equal x (Some 1.0))
end


let test_interval_tally_3_a () = begin
  let nums = [ 0.0 ; 1024.0 ] in
  let il = interval_of_float_list nums in
  (let x = interval_tally_below_opt ~il ~num:(0.0) () in
  assert_equal x (Some (2 *.. 0.0)))
end


let test_interval_tally_3_b () = begin
  let nums = [ 0.0 ; 1024.0 ] in
  let il = interval_of_float_list nums in
  (let x = interval_tally_below_opt ~il ~num:(1024.0) () in
  assert_equal x (Some (2 *.. 1.0)))
end

let test_interval_tally_3_c () = begin
  let nums = [ 0.0 ; 1024.0 ] in
  let il = interval_of_float_list nums in
  (let x = interval_tally_below_opt ~il ~num:(0.0) () in
  assert_equal x (Some (2 *.. 0.5)))
end

let test_interval_tally_4_a () = begin
  let nums = [ 10.0; 10.0; 10.0;
               10.0; 10.0; 10.0;
               10.0; 10.0; 10.0;
                5.0; 99.0;] in
  let il = interval_of_float_list nums in
  let x = from_opt (interval_tally_below_opt ~il ~num:(-100.0) ()) in
  assert_equal x (11 *.. 0.0)
end


let test_interval_tally_4_b () = begin
  let nums = [ 10.0; 10.0; 10.0;
               10.0; 10.0; 10.0;
               10.0; 10.0; 10.0;
                5.0; 99.0;] in
  let il = interval_of_float_list nums in
  let x = from_opt (interval_tally_below_opt ~il ~num:(5.25) ()) in
  assert_equal x (0.1)
end


let test_interval_tally_4_c () = begin
  let nums = [ 10.0; 10.0; 10.0;
               10.0; 10.0; 10.0;
               10.0; 10.0; 10.0;
                5.0; 99.0;
                5.5;
                6.0; ] in
  let il = interval_of_float_list nums in
  let x = from_opt (interval_tally_below_opt ~il ~num:(5.25) ()) in
  assert_equal x 0.2
end


let test_interval_tally_4_d () = begin
  let nums = [ 10.0; 10.0; 10.0;
               10.0; 10.0; 10.0;
               10.0; 10.0; 10.0;
                5.0; 99.0;
                5.5;
                6.0; ] in
  let il = interval_of_float_list nums in
  let x = from_opt (interval_tally_below_opt ~il ~num:(5.25) ()) in
  let x' = from_opt (interval_tally_below_opt ~il ~num:(5.15) ()) in
  assert_equal (x' < x) true
end

let test_find_quantile_1_a () = begin
  let nums = [ 10.0 ] in
  let il = interval_of_float_list nums in
  let x = from_opt (interval_get_qth_value ~il ~qtile:0.5 ~tol:0.01) in
  assert_equal x 10.0
end


let test_find_quantile_1_b () = begin
  let nums = [ 10.0 ] in
  let il = interval_of_float_list nums in
  let x = from_opt (interval_get_qth_value ~il ~qtile:0.5 ~tol:0.01) in
  assert_equal x 10.0
end


let test_find_quantile_1_c () = begin
  let nums = [ 10.0 ] in
  let il = interval_of_float_list nums in
  let x = from_opt (interval_get_qth_value ~il ~qtile:0.5 ~tol:0.01) in
  assert_equal x 10.0
end


let test_find_quantile_2_a () = begin
  let nums = [ 0.0 ; 1.0 ] in
  let il = interval_of_float_list nums in
  let x = from_opt (interval_get_qth_value ~il ~qtile:0.0 ~tol:0.001) in
  assert_equal (within ~tol:0.001 0.0 x) true
end


let test_find_quantile_2_b () = begin
  let nums = [ 0.0 ; 1.0 ] in
  let il = interval_of_float_list nums in
  let x = from_opt (interval_get_qth_value ~il ~qtile:0.1 ~tol:0.001) in
  assert_equal (within ~tol:0.001 0.1 x) true
end

let test_find_quantile_2_c () = begin
  let nums = [ 0.0 ; 1.0 ] in
  let il = interval_of_float_list nums in
  let x = from_opt (interval_get_qth_value ~il ~qtile:0.5 ~tol:0.001) in
  assert_equal (within ~tol:0.001 0.5 x) true
end

let test_find_quantile_3_a () = begin
  let nums = [ 0.0 ; 0.0 ; 0.0 ;
               0.0 ; 0.0 ; 0.0 ;
               0.0 ; 0.0 ; 0.0 ;
               45.0 ; 44.0 ; 46.3 ] in
  let il = interval_of_float_list nums in
  let x = from_opt (interval_get_qth_value ~il ~qtile:0.95 ~tol:0.01) in
  assert_equal (within ~tol:0.1 45.7 x) true
end


let test_invert_1 () = begin
  let func x = x *. x in
  let tol = 0.0001 in
  let lo = 0.0 in
  let hi = 100.0 in
  let target = 100.0 in
  let out = invert_real_func ~func ~tol ~hi ~lo ~target in
  let out = from_opt out in
  assert_equal (within ~tol out 10.0) true
end


let test_invert_2 () = begin
  let func x = x *. x in
  let tol = 0.0001 in
  let lo = 0.0 in
  let hi = 100.0 in
  let target = 99999.0 in
  let out = invert_real_func ~func ~tol ~hi ~lo ~target in
  assert_equal out None
end




let suite = "Test Quantile" >::: [
  "test_midpoint" >:: test_midpoint;
  "test_empty_infinite_interval" >:: test_empty_infinite_interval;
  "test_singleton_interval" >:: test_singleton_interval;
  "test_interval_add_bypass" >:: test_interval_add_bypass;
  "test_interval_add_many_1" >:: test_interval_add_many_1;
  "test_interval_add_many_2" >:: test_interval_add_many_2;
  "test_interval_add_many_3" >:: test_interval_add_many_3;
  "test_interval_tally_1" >:: test_interval_tally_1;
  "test_interval_tally_2_a" >:: test_interval_tally_2_a;
  "test_interval_tally_2_b" >:: test_interval_tally_2_b;
  "test_interval_tally_2_c" >:: test_interval_tally_2_c;
  "test_interval_tally_3_a" >:: test_interval_tally_3_a;
  "test_interval_tally_3_b" >:: test_interval_tally_3_b;
  "test_interval_tally_4_a" >:: test_interval_tally_4_a;
  "test_interval_tally_4_b" >:: test_interval_tally_4_b;
  "test_interval_tally_4_c" >:: test_interval_tally_4_c;
  "test_interval_tally_4_d" >:: test_interval_tally_4_d;
  "test_find_quantile_1_a" >:: test_find_quantile_1_a;
  "test_find_quantile_1_b" >:: test_find_quantile_1_b;
  "test_find_quantile_2_a" >:: test_find_quantile_2_a;
  "test_find_quantile_2_b" >:: test_find_quantile_2_b;
  "test_find_quantile_2_c" >:: test_find_quantile_2_c;
  "test_find_quantile_3_a" >:: test_find_quantile_3_a;
  "test_invert_1" >:: test_invert_1;
  "test_invert_2" >:: test_invert_2;
]

let _ =
run_test_tt_main suite
