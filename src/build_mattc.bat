@ECHO off
REM This script builds the binaries for the MattC compiler
ocamllex mattc_lex.mll
ocamlyacc -v mattc_par.mly
ocamlc -c mattc_par.mli
ocamlc -c mattc_par.ml
ocamlc -c mattc_lex.ml
ocamlc -c mattc.ml
ocamlc -o mattc.exe mattc_lex.cmo mattc_par.cmo mattc.cmo
echo Compilation Complete