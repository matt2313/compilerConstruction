all: unix
	

set-windows:
	$(eval outname := "../bin/mattc.exe")
windows: set-windows mattc
	
set-unix:
	$(eval outname := "../bin/mattc")
unix: set-unix mattc
	
.PHONY: set-windows
.PHONY: set-unix
.PHONY: windows
.PHONY: unix

mattc: mattc.cmo
	ocamlc -o $(outname) parseTreeType.cmo parseTreeEvaluator.cmo parseTreeOptimiser.cmo instructionSetType.cmo instructionSetConvert.cmo instructionSetEvaluate.cmo mattc_lex.cmo mattc_par.cmo mattc.cmo

parseTreeType.cmo: parseTreeType.ml
	ocamlc -c "parseTreeType.ml"

parseTreeEvaluator.cmo: parseTreeType.cmo parseTreeEvaluator.ml
	ocamlc -c "parseTreeEvaluator.ml"

parseTreeOptimiser.cmo: parseTreeType.cmo parseTreeEvaluator.cmo parseTreeOptimiser.ml
	ocamlc -c "parseTreeOptimiser.ml"

instructionSetType.cmo: parseTreeType.cmo parseTreeEvaluator.cmo parseTreeOptimiser.cmo instructionSetType.ml
	ocamlc -c "instructionSetType.ml"

instructionSetConvert.cmo: parseTreeType.cmo parseTreeEvaluator.cmo parseTreeOptimiser.cmo instructionSetType.cmo instructionSetConvert.ml
	ocamlc -c "instructionSetConvert.ml"

instructionSetEvaluate.cmo: parseTreeType.cmo parseTreeEvaluator.cmo parseTreeOptimiser.cmo instructionSetType.cmo instructionSetConvert.cmo instructionSetEvaluate.ml
	ocamlc -c "instructionSetEvaluate.ml"

instructionSetX86.cmo: parseTreeType.cmo parseTreeEvaluator.cmo parseTreeOptimiser.cmo instructionSetType.cmo instructionSetConvert.cmo instructionSetEvaluate.cmo instructionSetX86.ml
	ocamlc -c "instructionSetX86.ml"

mattc_lex.cmo: parseTreeType.cmo parseTreeEvaluator.cmo parseTreeOptimiser.cmo instructionSetType.cmo instructionSetConvert.cmo instructionSetEvaluate.cmo instructionSetX86.cmo mattc_lex.mll
	ocamllex "mattc_lex.mll"
	ocamlc -c "mattc_lex.ml"

mattc_par.cmo: parseTreeType.cmo parseTreeEvaluator.cmo parseTreeOptimiser.cmo instructionSetType.cmo instructionSetConvert.cmo instructionSetEvaluate.cmo instructionSetX86.cmo mattc_par.mly
	ocamlyacc -v "mattc_par.mly"
	ocamlc -c "mattc_par.mli"
	ocamlc -c "mattc_par.ml"

mattc.cmo: parseTreeType.cmo parseTreeEvaluator.cmo parseTreeOptimiser.cmo instructionSetType.cmo instructionSetConvert.cmo instructionSetEvaluate.cmo instructionSetX86.cmo mattc_par.cmo mattc_lex.cmo mattc.ml
	ocamlc -c "mattc.ml"
