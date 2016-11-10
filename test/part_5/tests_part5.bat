@ECHO off
REM This script runs the test code for the MattC compiler
REM -i "test_read.data" "test_read.mc" 
"../../src/mattc" -c -e -s -out "test_1.program" "test_1.mc" -out "test_2.program" "test_2.mc" -o -out "test_1_optimised.program" "test_1.mc" -out "test_2_optimised.program" "test_2.mc"