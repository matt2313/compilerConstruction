@ECHO off
REM This script runs the test code for the MattC compiler
REM -i "test_read.data" "test_read.mc" 
"../../src/mattc" -e -o -v -ov -i "test_read.data" "test_read.mc"  "test_1.mc" "test_2.mc" "test_3.mc" "test_4.mc" -i "test_5.data" "test_5.mc" "test_6.mc" "test_7.mc" "test_8.mc" "test_9.mc" "test_10.mc"