ARCH=x86_64
#ARCH=aarch64

all: anpr.cpp
	c++ -I/usr/include/opencv4 anpr.cpp -lopencv_core -lopencv_imgcodecs -o ../../bin/linux-$(ARCH)/anpr

run: ../../bin/linux-$(ARCH)/anpr
	../../bin/linux-$(ARCH)/anpr
	
