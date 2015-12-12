.PHONY: all clean run coins vault

all: vm teleport

run: vm
	./vm challenge.bin

vm: vm.c
	clang -g -O3 $^ -o $@
teleport: teleport.c
	clang -g -O3 $^ -o $@
coins:
	runghc Coins.hs
vault:
	runghc Vault.hs

clean:
	rm vm teleport
