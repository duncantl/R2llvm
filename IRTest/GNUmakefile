LLVM_BIN=$(HOME)/LLVM3.7/clang+llvm-3.7.0-x86_64-apple-darwin/bin/

LLC=$(LLVM_BIN)/llc
CLANG=$(LLVM_BIN)/clang
OPT=0

%.ll: %.c
	$(CLANG) -O$(OPT) -S -emit-llvm $< -I$(R_HOME)/include

%.cpp: %.ll
	$(LLC) -march=cpp $< -o $@

%_opt.ll: %.ll
	  opt -mem2reg -S $< -o $@