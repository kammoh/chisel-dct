CC=gcc
CXX=g++
CXXFLAGS ?= -O3 -fomit-frame-pointer 
CFLAGS=$(CXXFLAGS)
LINKFLAGS=

all: acm libacm.dylib

debug: 
	CXXFLAGS=-g $(MAKE) synth
clean:
	rm -f *.o acm libacm.*
acm: acm-main cmdline.o chains_util.o 21.costs.o synth.o ac1.o adder.o 
	$(CXX) $(CXXFLAGS) -o acm acm.o cmdline.o chains_util.o 21.costs.o ac1.o adder.o $(LINKFLAGS)

acm-main: acm.cpp acm.h
	$(CXX) $(CXXFLAGS) -DACM_MAIN -c -o acm.o acm.cpp

acm-no-main: acm.cpp acm.h
	$(CXX) $(CXXFLAGS) -c -o acm_nomain.o acm.cpp

libacm.dylib: acm-no-main chains_util.o  21.costs.o synth.o
	$(CXX) -dynamiclib chains_util.o 21.costs.o acm_nomain.o cmdline.o ac1.o adder.o -o libacm.dylib

