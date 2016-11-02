#!/bin/bash
# This script runs the test code for the MattC compiler
"../src/mattc" "part_1/ints.mc" "part_1/bools.mc" "part_1/controlFlow.mc" "part_1/io.mc" "part_1/strings.mc" "part_1/floats.mc" "part_1/variables.mc" "part_1/functions.mc" "part_1/casts.mc" "part_1/letNew.mc" "part_1/bisection_recursive.mc" "part_1/bisection_iterative.mc" -e -o "part_2/test_1.mc" "part_2/test_2.mc" "part_2/test_3.mc" "part_2/test_4.mc" "part_2/test_6.mc" "part_2/test_7.mc" "part_2/test_8.mc" "part_2/test_10.mc" "part_2/test_fib.mc" "part_3/test_1.mc" "part_3/test_2.mc" "part_3/test_3.mc" "part_3/test_4.mc" "part_3/test_5.mc" "part_3/test_6.mc" "part_3/test_7.mc" "part_3/test_8.mc" "part_3/test_9.mc" "part_3/test_10.mc" "part_3/test_11.mc" -v -ov -i "part_4/test_read.data" "part_4/test_read.mc"