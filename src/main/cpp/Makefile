UNAME := $(shell uname)
CXXFLAGS = -Wno-c++11-compat-deprecated-writable-strings -O3 -fomit-frame-pointer 
CFLAGS=$(CXXFLAGS)
ifeq ($(UNAME), Linux)
LD_FLAGS = -shared -fPIC
SUFFIX = so
else ifeq ($(UNAME), Darwin)
LD_FLAGS = -dynamiclib
SUFFIX = dylib
else # Windows?! TODO FIXME
LD_FLAGS = -shared -fPIC
SUFFIX = dll
endif

OBJ_FILES = chains_util.o 21.costs.o acm.o ac1.o adder.o 

all: libacm

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

clean:
	rm -f $(OBJ_FILES) libacm.$(SUFFIX)

libacm: $(OBJ_FILES)
	$(CXX) $(LD_FLAGS) -o $@.$(SUFFIX) $^

