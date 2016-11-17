all: unix
	

unix: test_1 test_1_opt test_2 test_2_opt test_3 test_3_opt test_4 test_4_opt
	
windows: test_1.exe test_1_opt.exe test_2.exe test_2_opt.exe test_3.exe test_3_opt.exe test_4.exe test_4_opt.exe
	
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


test_3.exe: test_3.mc ../../bin/mattc.exe
	"../../bin/mattc" -out "test_3.s" -x86 "../../bin/template_windows" test_3.mc
	gcc -o "test_3.exe" "test_3.s"
test_3_opt.exe: test_3.mc ../../bin/mattc.exe
	"../../bin/mattc" -o -out "test_3_opt.s" -x86 "../../bin/template_windows" test_3.mc
	gcc -o "test_3_opt.exe" "test_3_opt.s"
test_3: test_3.mc ../../bin/mattc
	"../../bin/mattc" -out "test_3.s" -x86 "../../bin/template_ubuntu" test_3.mc
	gcc -o "test_3" "test_3.s"
test_3_opt: test_3.mc ../../bin/mattc
	"../../bin/mattc" -o -out "test_3_opt.s" -x86 "../../bin/template_ubuntu" test_3.mc
	gcc -o "test_3_opt" "test_3_opt.s"


test_4.exe: test_4.mc ../../bin/mattc.exe
	"../../bin/mattc" -out "test_4.s" -x86 "../../bin/template_windows" test_4.mc
	gcc -o "test_4.exe" "test_4.s"
test_4_opt.exe: test_4.mc ../../bin/mattc.exe
	"../../bin/mattc" -o -out "test_4_opt.s" -x86 "../../bin/template_windows" test_4.mc
	gcc -o "test_4_opt.exe" "test_4_opt.s"
test_4: test_4.mc ../../bin/mattc
	"../../bin/mattc" -out "test_4.s" -x86 "../../bin/template_ubuntu" test_4.mc
	gcc -o "test_4" "test_4.s"
test_4_opt: test_4.mc ../../bin/mattc
	"../../bin/mattc" -o -out "test_4_opt.s" -x86 "../../bin/template_ubuntu" test_4.mc
	gcc -o "test_4_opt" "test_4_opt.s"



test: test_1 test_1_opt test_2 test_2_opt test_3 test_3_opt test_4 test_4_opt
	"./test_1"
	"./test_1_opt"
	"./test_2"
	"./test_2_opt"
	"./test_3"
	"./test_3_opt"
	"./test_4"
	"./test_4_opt"

test_windows: test_1.exe test_1_opt.exe test_2.exe test_2_opt.exe test_3.exe test_3_opt.exe test_4.exe test_4_opt.exe
	test_1
	test_1_opt
	test_2
	test_2_opt
	test_3
	test_3_opt
	test_4
	test_4_opt