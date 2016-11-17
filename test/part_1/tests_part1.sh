#!/bin/bash
# This script runs the test code for the MattC compiler
"../../bin/mattc" -v "ints.mc" "bools.mc" "controlFlow.mc" "io.mc" "strings.mc" "floats.mc" "variables.mc" "functions.mc" "casts.mc" "letNew.mc" "bisection_recursive.mc" "bisection_iterative.mc"