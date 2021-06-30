(* 
                         CS 51 Problem Set 2
            Higher Order Functional Programming -- Testing
 *)

open Mapfold ;;

open Test_simple ;;      (* a really simple unit testing framework *)
  
let test () =
  unit_test ((negate_all []) = [])
            "negate_all empty";
  unit_test ((negate_all [1; -2; 0]) = [-1; 2; 0])
            "negate_all mixed";
  unit_test ((sum []) = 0)
            "sum empty";
  unit_test ((sum [1; 2; 3; 4; 5]) = 15)
            "sum mixed";
  unit_test ((sum_rows []) = [])
            "sum_rows empty";
  unit_test ((sum_rows [[1; 2]; [3; 4]]) = [3; 7])
            "sum_rows mixed";
  unit_test ((filter_odd []) = [])
            "filter_odd empty";
  unit_test ((filter_odd [1; 4; 5; -3]) = [1; 5; -3])
            "filter_odd mixed";
  unit_test ((num_occurs 0 []) = 0)
            "num_occurs empty";
  unit_test ((num_occurs 4 [1; 3; 4; 5; 4]) = 2)
            "num_occurs mixed";
  unit_test ((super_sum []) = 0)
            "super_sum empty";
  unit_test ((super_sum [[1; 2; 3]; []; [5]]) = 11)
            "super_sum mixed";
  unit_test ((filter_range [] (0, 0)) = [])
            "filter_range empty";
  unit_test ((filter_range [1; 3; 4; 5; 2] (3, 2)) = [])
            "filter_range invalid range";
  unit_test ((filter_range [1; 3; 4; 5; 2] (1, 3)) = [1; 3; 2])
            "filter_range mixed";
  unit_test ((floats_of_ints []) = [])
            "floats_of_ints empty";
  unit_test ((floats_of_ints [1; 2; 3]) = [1.; 2.; 3.])
            "floats_of_ints mixed";
  unit_test ((log10s []) = [])
            "log10s empty";
  unit_test ((log10s [1.0; 10.0; -10.0]) = [Some 0.; Some 1.; None])
            "log10s mixed";
  unit_test ((deoptionalize []) = [])
            "deoptionalize empty";
  unit_test ((deoptionalize [Some 3; None; Some 5; Some 10]) = [3; 5; 10])
            "deoptionalize mixed";
  unit_test ((some_sum []) = 0)
            "some_sum empty";
  unit_test ((some_sum [Some 3; None; Some 5; Some 10]) = 18)
            "some_sum mixed";
  unit_test ((mult_odds []) = 1)
            "mult_odds empty";
  unit_test ((mult_odds [1; 3; 0; 2; -5]) = -15)
            "multi_odds mixed";
  unit_test ((concat [[]]) = [])
            "concat empty";
  unit_test ((concat [[1; 2]; []; [3; 4; 5]; [6]]) = [1; 2; 3; 4; 5; 6])
            "concat mixed";
  unit_test ((filter_by_year [] 2021) = [])
            "filter_by_year empty";
  unit_test ((filter_by_year [("Joe", 2010); ("Bob", 2010); ("Tom", 2013)] 2010) = ["Joe"; "Bob"])
            "filter_by_year mixed"
  ;;

test () ;;
