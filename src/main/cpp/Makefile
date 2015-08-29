UNAME := $(shell uname)
CXXFLAGS = -Wno-c++11-compat-deprecated-writable-strings -O3 -fomit-frame-pointer -g0 
CFLAGS=$(CXXFLAGS)
LIB_PREFIX=lib
ifeq ($(UNAME), Linux)
LD_FLAGS = -shared -fPIC
LIB_SUFFIX = so
else ifeq ($(UNAME), Darwin)
LD_FLAGS = -dynamiclib
LIB_SUFFIX = dylib
else # Windows?! TODO FIXME
LD_FLAGS = -shared -fPIC
LIB_SUFFIX = dll
LIB_PREFIX =
endif

OBJ_FILES = chains_util.o 21.costs.o acm.o ac1.o adder.o 

all: $(LIB_PREFIX)acm

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

clean:
	rm -f $(OBJ_FILES) $(LIB_PREFIX)acm.$(LIB_SUFFIX)

$(LIB_PREFIX)acm: $(OBJ_FILES)
	$(CXX) $(LD_FLAGS) -o $@.$(LIB_SUFFIX) $^

