.PHONY: all clean run

all: vm

run: vm
	./vm challenge.bin

vm: vm.c
	clang -O3 $^ -o $@

clean:
	rm vm
