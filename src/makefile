all: unix
	

set-windows:
	$(eval outname := ../bin/mattc.exe)
windows: set-windows mattc
	
set-unix:
	$(eval outname := ../bin/mattc)
unix: set-unix mattc
	
.PHONY: set-windows
.PHONY: windows
.PHONY: set-unix
.PHONY: unix
.PHONY: all


mattc: parseTreeType.cmo parseTreeEvaluator.cmo parseTreeOptimiser.cmo instructionSetType.cmo instructionSetConvert.cmo instructionSetEvaluate.cmo mattc_par.cmo mattc_lex.cmo mattc.cmo
	ocamlc -o $(outname) parseTreeType.cmo parseTreeEvaluator.cmo parseTreeOptimiser.cmo instructionSetType.cmo instructionSetConvert.cmo instructionSetEvaluate.cmo mattc_lex.cmo mattc_par.cmo mattc.cmo

parseTreeType.cmo:
	ocamlc -c "parseTreeType.ml"

parseTreeEvaluator.cmo:
	ocamlc -c "parseTreeEvaluator.ml"

parseTreeOptimiser.cmo:
	ocamlc -c "parseTreeOptimiser.ml"

instructionSetType.cmo:
	ocamlc -c "instructionSetType.ml"

instructionSetConvert.cmo:
	ocamlc -c "instructionSetConvert.ml"

instructionSetEvaluate.cmo:
	ocamlc -c "instructionSetEvaluate.ml"

mattc_lex.cmo:
	ocamllex "mattc_lex.mll"
	ocamlc -c "mattc_lex.ml"

mattc_par.cmo:
	ocamlyacc -v "mattc_par.mly"
	ocamlc -c "mattc_par.mli"
	ocamlc -c "mattc_par.ml"

mattc.cmo:
	ocamlc -c "mattc.ml"
