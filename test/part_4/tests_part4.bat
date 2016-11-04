@ECHO off
REM This script runs the test code for the MattC compiler
REM -i "test_read.data" "test_read.mc" 
"../../src/mattc" -e -v -o -ov -i "test_read.data" "test_read.mc"  "test_1.mc" "test_2.mc" "test_3.mc" "test_4.mc" -i "test_5.data" "test_5.mc" "test_6.mc" "test_7.mc" -i "test_8.data" "test_8.mc" "test_9.mc" -i "test_10.data" "test_10.mc"