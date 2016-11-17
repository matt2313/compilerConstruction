all: unix
	

unix: test_1 test_1_opt test_2 test_2_opt
	
windows: test_1.exe test_1_opt.exe test_2.exe test_2_opt.exe
	
.PHONY: windows
.PHONY: unix


test_1.exe: test_1.mc ../../bin/mattc.exe
	"../../bin/mattc" -out "test_1.s" -x86 "../../bin/template_windows" test_1.mc
	gcc -o "test_1.exe" "test_1.s"
test_1_opt.exe: test_1.mc ../../bin/mattc.exe
	"../../bin/mattc" -o -out "test_1_opt.s" -x86 "../../bin/template_windows" test_1.mc
	gcc -o "test_1_opt.exe" "test_1_opt.s"
test_1: test_1.mc ../../bin/mattc
	"../../bin/mattc" -out "test_1.s" -x86 "../../bin/template_ubuntu" test_1.mc
	gcc -o "test_1" "test_1.s"
test_1_opt: test_1.mc ../../bin/mattc
	"../../bin/mattc" -o -out "test_1_opt.s" -x86 "../../bin/template_ubuntu" test_1.mc
	gcc -o "test_1_opt" "test_1_opt.s"


test_2.exe: test_2.mc ../../bin/mattc.exe
	"../../bin/mattc" -out "test_2.s" -x86 "../../bin/template_windows" test_2.mc
	gcc -o "test_2.exe" "test_2.s"
test_2_opt.exe: test_2.mc ../../bin/mattc.exe
	"../../bin/mattc" -o -out "test_2_opt.s" -x86 "../../bin/template_windows" test_2.mc
	gcc -o "test_2_opt.exe" "test_2_opt.s"
test_2: test_2.mc ../../bin/mattc
	"../../bin/mattc" -out "test_2.s" -x86 "../../bin/template_ubuntu" test_2.mc
	gcc -o "test_2" "test_2.s"
test_2_opt: test_2.mc ../../bin/mattc
	"../../bin/mattc" -o -out "test_2_opt.s" -x86 "../../bin/template_ubuntu" test_2.mc
	gcc -o "test_2_opt" "test_2_opt.s"