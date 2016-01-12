.PHONY: all clean run coins vault scripted

all: vm teleport

run: vm
	./vm challenge.bin
scripted: vm
	./vm challenge.bin < script.txt

vm: vm.c
	clang -g -O3 $^ -o $@
vmfast: vmfast.c
	clang -g -O3 $^ -o $@
teleport: teleport.c
	clang -g -O3 $^ -o $@
coins:
	runghc Coins.hs
vault:
	runghc Vault.hs

clean:
	rm vm teleport
