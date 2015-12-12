.PHONY: all clean run coins vault

all: vm teleporter

run: vm
	./vm challenge.bin

vm: vm.c
	clang -g -O3 $^ -o $@
teleporter: teleporter.c
	clang -g -O3 $^ -o $@
coins:
	runghc Coins.hs
vault:
	runghc Vault.hs

clean:
	rm vm teleporter
