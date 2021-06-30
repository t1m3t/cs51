(*
                         CS 51 Problem Set 1
                Core Functional Programming -- Testing
*)                           

open Ps1 ;;

(* The Absbook module contains simple functions for unit testing:
   `unit_test` and `unit_test_within`. *)
open CS51Utils ;;
open Absbook ;;
  
let nonincreasing_test () =
  unit_test (nonincreasing [])
            "nonincreasing empty";
  unit_test (nonincreasing [7])
            "nonincreasing single";
  unit_test (nonincreasing [4;4;4])
            "nonincreasing repeat";
  unit_test (not (nonincreasing [2;1;2]))
            "nonincreasing inc at start";
  unit_test (nonincreasing [2;2;1])
            "nonincreasing dups";
  unit_test (nonincreasing [9;8;7;6;5;5;5;4;4;-2])
            "nonincreasing long with neg";
  unit_test (not (nonincreasing [9;8;7;6;7;5;5;5;5;4;3]))
            "nonincreasing long inc at mid" ;
  unit_test (nonincreasing [10;8;7;5;5;-2])
            "nonincreasing negs" ;
  unit_test (not (nonincreasing [-2;-1;-1;0;1;5]))
            "nonincreasing negs all inc" ;;

let merge_test () =
  unit_test (merge [1; 3; 5] [2; 4; 6] = [1; 2; 3; 4; 5; 6])
            "merge [1; 3; 5] [2; 4; 6]";
  unit_test (merge [1; 2; 5] [2; 4; 6] = [1; 2; 2; 4; 5; 6])
            "merge [1; 2; 5] [2; 4; 6]";
  unit_test (merge [1; 3; 5] [2; 4; 6; 12] = [1; 2; 3; 4; 5; 6; 12])
            "merge [1; 3; 5] [2; 4; 6; 12]";
  unit_test (merge [1; 3; 5; 700; 702] [2; 4; 6; 12] = [1; 2; 3; 4; 5; 6; 12; 700; 702])
            "merge [1; 3; 5; 700; 702] [2; 4; 6; 12]";
  unit_test (merge [1; 3; 5; 7; 9] [] = [1; 3; 5; 7; 9])
            "merge [1; 3; 5; 7; 9] []";
  unit_test (merge [] [1; 3; 5; 7; 9] = [1; 3; 5; 7; 9])
            "merge [] [1; 3; 5; 7; 9]" ;;

let unzip_test () = 
  unit_test (unzip [] = ([], []))
            "unzip []";
  unit_test (unzip [(true, false); (false, false); (true, false)] = ([true; false; true], [false; false; false]))
            "unzip [(true, false); (false, false); (true, false)]";
  unit_test (unzip [(true, true); (true, false); (false, true)] = ([true; true; false], [true; false; true]))
            "unzip [(true, true); (true, false); (false, true)]";
  unit_test (unzip [(true, false)] = ([true], [false]))
            "unzip [(true, false)]" ;;

let to_run_length_test () = 
  unit_test (to_run_length [] = [])
            "to_run_length []";
  unit_test (to_run_length ['a'; 'a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'a'; 'd'; 'd'; 'd'; 'd'] = [(5, 'a'); (3, 'b'); (1, 'a'); (4, 'd')])
            "to_run_length ['a'; 'a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'a'; 'd'; 'd'; 'd'; 'd']";
  unit_test (to_run_length ['a'; 'a'; 'a'; 'a'] = [(4, 'a')])
            "to_run_length ['a'; 'a'; 'a'; 'a']";
  unit_test (to_run_length ['a'; 'a'; 'b'; 'b'; 'c'] = [(2, 'a'); (2, 'b'); (1, 'c')])
            "to_run_length ['a'; 'a'; 'b'; 'b'; 'c']" ;;

let from_run_length_test () =
  unit_test (from_run_length [] = [])
            "from_run_length empty";
  unit_test (from_run_length [(5, 'a'); (3, 'b'); (1, 'a'); (4, 'd')] = ['a'; 'a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'a'; 'd'; 'd'; 'd'; 'd'])
            "from_run_length [(5, 'a'); (3, 'b'); (1, 'a'); (4, 'd')]";
  unit_test (from_run_length [(4, 'a')] = ['a'; 'a'; 'a'; 'a'])
            "from_run_length [(4, 'a')]";
  unit_test (from_run_length [(2, 'a'); (2, 'b'); (1, 'c')] = ['a'; 'a'; 'b'; 'b'; 'c'])
            "from_run_length [(2, 'a'); (2, 'b'); (1, 'c')]" ;;
  
let test_all () =
  nonincreasing_test () ;
  merge_test () ;
  unzip_test () ;
  to_run_length_test () ;
  from_run_length_test () 
  ;;

let _ = test_all () ;;
