.PHONY: all clean run

all: vm

run: vm
	./vm challenge.bin

vm: vm.c
	clang -g -O3 $^ -o $@

clean:
	rm vm
