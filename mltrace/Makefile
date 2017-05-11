INCLUDE:=../runtime
CFLAGS=-Wall -I$(INCLUDE) -O2 -g
OBJS:=tracetr.o

.PHONY: all clean

all: tracetr

clean:
	rm -f $(OBJS)

%.o: %.c
	$(CC) $(CFLAGS) -c $^ -o $@

tracetr: $(OBJS)
	$(CC) $^ -o $@
