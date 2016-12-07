#!/bin/bash
# This script runs regression tests for the MattC compiler

TestsCompleted=0
TestsPassed=0
TestsFailed=0

# Checks if a given program returns 0
# 1st parameter is the program to test
# 2nd parameter is the name of the test (used for output)
function runSimpleTestCase
{
    let "TestsCompleted += 1"
    $1 > /dev/null
    if [ $? = 0 ];
        then
            let "TestsPassed += 1"
            printf "$2: PASS\n"
        else
            let "TestsFailed += 1"
            printf "\n$2: FAIL\n"
    fi
}

# Like runSimpleTestCase but treats a failure as a pass and vice-versa
function runSimpleTestCaseInverted
{
    let "TestsCompleted += 1"
    $1 > /dev/null 2>&1
    if [ $? = 0 ];
        then
            let "TestsFailed += 1"
            printf "\n$2: FAIL (FALSE POSITIVE)\n"
        else
            let "TestsPassed += 1"
            printf "$2: PASS (TRUE NEGATIVE)\n"
    fi
}

runSimpleTestCase "../bin/mattc part_1/ints.mc"                "part_1/ints               "
runSimpleTestCase "../bin/mattc part_1/bools.mc"               "part_1/bools              "
runSimpleTestCase "../bin/mattc part_1/controlFlow.mc"         "part_1/controlFlow        "
runSimpleTestCase "../bin/mattc part_1/io.mc"                  "part_1/io                 "
runSimpleTestCase "../bin/mattc part_1/strings.mc"             "part_1/strings            "
runSimpleTestCase "../bin/mattc part_1/floats.mc"              "part_1/floats             "
runSimpleTestCase "../bin/mattc part_1/variables.mc"           "part_1/variables          "
runSimpleTestCase "../bin/mattc part_1/functions.mc"           "part_1/functions          "
runSimpleTestCase "../bin/mattc part_1/casts.mc"               "part_1/casts              "
runSimpleTestCase "../bin/mattc part_1/letNew.mc"              "part_1/letNew             "
runSimpleTestCase "../bin/mattc part_1/bisection_recursive.mc" "part_1/bisection_recursive"
runSimpleTestCase "../bin/mattc part_1/bisection_iterative.mc" "part_1/bisection_iterative"
printf "\n"
runSimpleTestCaseInverted "../bin/mattc -e part_1/test_fail_1.mc" "part_1/test_fail_1"
runSimpleTestCaseInverted "../bin/mattc -e part_1/test_fail_2.mc" "part_1/test_fail_2"
runSimpleTestCaseInverted "../bin/mattc -e part_1/test_fail_3.mc" "part_1/test_fail_3"
runSimpleTestCaseInverted "../bin/mattc -e part_1/test_fail_4.mc" "part_1/test_fail_4"
runSimpleTestCaseInverted "../bin/mattc -e -exp Int(100) part_1/test_fail_5.mc" "part_1/test_fail_5"
printf "\n"
runSimpleTestCase "../bin/mattc -e -o -exp Int(111) part_2/test_1.mc" "part_2/test_1   "
runSimpleTestCase "../bin/mattc -e -o -exp Float(11.1) part_2/test_2.mc" "part_2/test_2   "
runSimpleTestCase "../bin/mattc -e -o -exp Bool(false) part_2/test_3.mc" "part_2/test_3   "
runSimpleTestCase "../bin/mattc -e -o -exp String(hello_world!) part_2/test_4.mc" "part_2/test_4   "
runSimpleTestCase "../bin/mattc -e -o -exp Float(125.) -i part_2/test_5.data part_2/test_5.mc" "part_2/test_5   "
runSimpleTestCase "../bin/mattc -e -o -exp Int(10) part_2/test_6.mc" "part_2/test_6   "
runSimpleTestCase "../bin/mattc -e -o -exp String(10_8_6_4_2_0_) part_2/test_7.mc" "part_2/test_7   "
runSimpleTestCase "../bin/mattc -e -o part_2/test_8.mc" "part_2/test_8   "
runSimpleTestCase "../bin/mattc -e -o -exp String(hello_world!) -i part_2/test_9.data part_2/test_9.mc" "part_2/test_9   "
runSimpleTestCase "../bin/mattc -e -o -exp Int(2565) part_2/test_10.mc" "part_2/test_10  "
runSimpleTestCase "../bin/mattc -e -o part_2/test_fib.mc" "part_2/test_fib "
printf "\n"
runSimpleTestCase "../bin/mattc -e -o -exp Int(30) part_3/test_1.mc"   "part_3/test_1   "
runSimpleTestCase "../bin/mattc -e -o -exp Int(245) part_3/test_2.mc"   "part_3/test_2   "
runSimpleTestCase "../bin/mattc -e -o -exp Int(6283) part_3/test_3.mc"   "part_3/test_3   "
runSimpleTestCase "../bin/mattc -e -o -exp Int(0) part_3/test_4.mc"   "part_3/test_4   "
runSimpleTestCase "../bin/mattc -e -o -exp Int(50) part_3/test_5.mc"   "part_3/test_5   "
runSimpleTestCase "../bin/mattc -e -o -exp Int(14) part_3/test_6.mc"   "part_3/test_6   "
runSimpleTestCase "../bin/mattc -e -o -exp String(iiiii) part_3/test_7.mc"   "part_3/test_7   "
runSimpleTestCase "../bin/mattc -e -o -exp String(When_you_divide_by_3_you_get_33%_of_the_total) part_3/test_8.mc"   "part_3/test_8   "
runSimpleTestCase "../bin/mattc -e -o part_3/test_9.mc"   "part_3/test_9   "
runSimpleTestCase "../bin/mattc -e -o part_3/test_10.mc"  "part_3/test_10  "
runSimpleTestCase "../bin/mattc -e -o -exp Int(7654321) part_3/test_11.mc"  "part_3/test_11  "
printf "\n"
runSimpleTestCase "../bin/mattc -e -o -exp String(1,2.5,true,test_string) -i part_4/test_read.data part_4/test_read.mc" "part_4/test_read"
runSimpleTestCase "../bin/mattc -e -o -exp Int(38) part_4/test_1.mc"                             "part_4/test_1   "
runSimpleTestCase "../bin/mattc -e -o -exp String(31.915,10.,67,false,hello_world!) part_4/test_2.mc"                             "part_4/test_2   "
runSimpleTestCase "../bin/mattc -e -o -exp String() part_4/test_3.mc"                             "part_4/test_3   "
runSimpleTestCase "../bin/mattc -e -o -exp Float(3.14) part_4/test_4.mc"                             "part_4/test_4   "
runSimpleTestCase "../bin/mattc -e -o -exp Int(-358) -i part_4/test_5.data part_4/test_5.mc"       "part_4/test_5   "
runSimpleTestCase "../bin/mattc -e -o -exp Int(321) part_4/test_6.mc"                             "part_4/test_6   "
runSimpleTestCase "../bin/mattc -e -o -exp Float(6.283185306) part_4/test_7.mc"                             "part_4/test_7   "
runSimpleTestCase "../bin/mattc -e -o -exp Int(3628926) -i part_4/test_8.data part_4/test_8.mc"       "part_4/test_8   "
runSimpleTestCase "../bin/mattc -e -o -exp Int(55) part_4/test_9.mc"                             "part_4/test_9   "
runSimpleTestCase "../bin/mattc -e -o -exp Int(1) -i part_4/test_10.data part_4/test_10.mc"     "part_4/test_10  "
printf "\n"
runSimpleTestCase "../bin/mattc -c -e -s -exp Int(4) -out part_5/test_1.program part_5/test_1.mc" "part_5/test_1"
runSimpleTestCase "../bin/mattc -c -e -s -exp Int(15) -out part_5/test_2.program part_5/test_2.mc" "part_5/test_2"
runSimpleTestCase "../bin/mattc -c -e -s -exp Int(100) -out part_5/test_3.program part_5/test_3.mc" "part_5/test_3"
runSimpleTestCase "../bin/mattc -c -e -s -exp Int(20) -out part_5/test_4.program part_5/test_4.mc" "part_5/test_4"
runSimpleTestCase "../bin/mattc -c -e -s -exp Int(4) -o -out part_5/test_1_optimised.program part_5/test_1.mc" "part_5/test_1_optimised"
runSimpleTestCase "../bin/mattc -c -e -s -exp Int(15) -o -out part_5/test_2_optimised.program part_5/test_2.mc" "part_5/test_2_optimised"
runSimpleTestCase "../bin/mattc -c -e -s -exp Int(100) -o -out part_5/test_3_optimised.program part_5/test_3.mc" "part_5/test_3_optimised"
runSimpleTestCase "../bin/mattc -c -e -s -exp Int(20) -o -out part_5/test_4_optimised.program part_5/test_4.mc" "part_5/test_4_optimised"
printf "\n"
(cd part_6 && make > /dev/null)
runSimpleTestCase "part_6/test_1" "part_6/test_1"
runSimpleTestCase "part_6/test_2" "part_6/test_2"
runSimpleTestCase "part_6/test_3" "part_6/test_3"
runSimpleTestCase "part_6/test_4" "part_6/test_4"
runSimpleTestCase "part_6/test_1_opt" "part_6/test_1_opt"
runSimpleTestCase "part_6/test_2_opt" "part_6/test_2_opt"
runSimpleTestCase "part_6/test_3_opt" "part_6/test_3_opt"
runSimpleTestCase "part_6/test_4_opt" "part_6/test_4_opt"
printf "\n"
(cd part_7 && make > /dev/null)
runSimpleTestCase "part_7/test_1" "part_7/test_1"
runSimpleTestCase "part_7/test_2" "part_7/test_2"
runSimpleTestCase "part_7/test_3" "part_7/test_3"
runSimpleTestCase "part_7/test_4" "part_7/test_4"
runSimpleTestCase "part_7/test_1_opt" "part_7/test_1_opt"
runSimpleTestCase "part_7/test_2_opt" "part_7/test_2_opt"
runSimpleTestCase "part_7/test_3_opt" "part_7/test_3_opt"
runSimpleTestCase "part_7/test_4_opt" "part_7/test_4_opt"
printf "\n"
(cd part_8 && make > /dev/null)
runSimpleTestCase "part_8/test_conv"      "part_8/test_conv "
runSimpleTestCase "part_8/test_class"     "part_8/test_class"
runSimpleTestCase "part_8/test_conv_opt"  "part_8/test_conv_opt "
runSimpleTestCase "part_8/test_class_opt" "part_8/test_class_opt"
printf "\n"
printf "Tests Completed: $TestsCompleted\n"
printf "Tests Passed:    $TestsPassed\n"
printf "Tests Failed:    $TestsFailed\n"