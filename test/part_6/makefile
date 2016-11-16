all: unix
	

unix: test_1
	
windows: test_1.exe
	
.PHONY: windows
.PHONY: unix


test_1.exe: test_1.mc ../../bin/mattc.exe
	"../../bin/mattc" -out "test_1.s" -x86 "../../bin/template_windows" test_1.mc
	gcc -o "test_1.exe" "test_1.s"

test_1: test_1.mc ../../bin/mattc
	"../../bin/mattc" -out "test_1.s" -x86 "../../bin/template_ubuntu" test_1.mc
	gcc -o "test_1" "test_1.s"