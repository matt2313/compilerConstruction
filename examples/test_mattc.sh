#!/bin/bash
# This script runs the test code for the MattC compiler
"../src/mattc" "ints.mc"
"../src/mattc" "bools.mc"
"../src/mattc" "controlFlow.mc"
"../src/mattc" "io.mc"
"../src/mattc" "strings.mc"
"../src/mattc" "floats.mc"
"../src/mattc" "variables.mc"
"../src/mattc" "functions.mc"
"../src/mattc" "casts.mc"
"../src/mattc" "letNew.mc"
"../src/mattc" "bisection_recursive.mc"
"../src/mattc" "bisection_iterative.mc"