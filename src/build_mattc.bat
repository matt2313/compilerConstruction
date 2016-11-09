@ECHO off
REM This script builds the binaries for the MattC compiler
ocamlc -c parseTreeType.ml
ocamlc -c parseTreeEvaluator.ml
ocamlc -c parseTreeOptimiser.ml

ocamlc -c instructionSetType.ml
ocamlc -c instructionSetConvert.ml
ocamlc -c instructionSetEvaluate.ml

ocamllex mattc_lex.mll
ocamlyacc -v mattc_par.mly

ocamlc -c mattc_par.mli
ocamlc -c mattc_par.ml
ocamlc -c mattc_lex.ml
ocamlc -c mattc.ml

ocamlc -o mattc.exe parseTreeType.cmo parseTreeEvaluator.cmo parseTreeOptimiser.cmo instructionSetType.cmo instructionSetConvert.cmo instructionSetEvaluate.cmo mattc_lex.cmo mattc_par.cmo mattc.cmo

echo Compilation Complete