CFLAGS += -O3
CFLAGS += $(shell pkg-config --cflags --libs swipl)

CXXFLAGS += $(CFLAGS)

OBJS += test.o
BIN = test

all: $(BIN)

$(BIN): $(OBJS)
	$(CXX) $(CXXFLAGS) -o "$@" "$^"

clean:
	-rm -f $(OBJS)
