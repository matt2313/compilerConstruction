all: unix
	

unix: test_conv test_conv_opt test_class test_class_opt
	
windows: test_conv.exe test_conv_opt.exe test_class.exe test_class_opt.exe
	
.PHONY: windows
.PHONY: unix


test_conv.exe: test_conv.mc ../../bin/mattc.exe
	"../../bin/mattc" -out "test_conv.s" -x86 "../../bin/template_windows" test_conv.mc
	gcc -o "test_conv.exe" "test_conv.s"
test_conv_opt.exe: test_conv.mc ../../bin/mattc.exe
	"../../bin/mattc" -o -out "test_conv_opt.s" -x86 "../../bin/template_windows" test_conv.mc
	gcc -o "test_conv_opt.exe" "test_conv_opt.s"
test_conv: test_conv.mc ../../bin/mattc
	"../../bin/mattc" -out "test_conv.s" -x86 "../../bin/template_ubuntu" test_conv.mc
	gcc -o "test_conv" "test_conv.s"
test_conv_opt: test_conv.mc ../../bin/mattc
	"../../bin/mattc" -o -out "test_conv_opt.s" -x86 "../../bin/template_ubuntu" test_conv.mc
	gcc -o "test_conv_opt" "test_conv_opt.s"


test_class.exe: test_class.mc ../../bin/mattc.exe
	"../../bin/mattc" -out "test_class.s" -x86 "../../bin/template_windows" test_class.mc
	gcc -o "test_class.exe" "test_class.s"
test_class_opt.exe: test_class.mc ../../bin/mattc.exe
	"../../bin/mattc" -o -out "test_class_opt.s" -x86 "../../bin/template_windows" test_class.mc
	gcc -o "test_class_opt.exe" "test_class_opt.s"
test_class: test_class.mc ../../bin/mattc
	"../../bin/mattc" -out "test_class.s" -x86 "../../bin/template_ubuntu" test_class.mc
	gcc -o "test_class" "test_class.s"
test_class_opt: test_class.mc ../../bin/mattc
	"../../bin/mattc" -o -out "test_class_opt.s" -x86 "../../bin/template_ubuntu" test_class.mc
	gcc -o "test_class_opt" "test_class_opt.s"



test: test_conv test_conv_opt test_class test_class_opt
	"./test_conv"
	"./test_conv_opt"
	"./test_class"
	"./test_class_opt"

test_windows: test_conv.exe test_conv_opt.exe test_class.exe test_class_opt.exe
	test_conv
	test_conv_opt
	test_class
	test_class_opt