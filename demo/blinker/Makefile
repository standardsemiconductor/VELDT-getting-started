TOP := Blinker

all: $(TOP).bin

$(TOP).bin: $(TOP).asc
	icepack $< $@

$(TOP).asc: $(TOP).json $(TOP).pcf 
	nextpnr-ice40 --up5k --package sg48 --pcf $(TOP).pcf --asc $@ --json $<

$(TOP).json: $(TOP).hs
	cabal run clash --write-ghc-environment-files=always -- $(TOP) --verilog
	yosys -q -p "synth_ice40 -top $(TOP) -json $@ -abc2" verilog/$(TOP).topEntity/*.v

prog: $(TOP).bin
	iceprog $<

build: $(TOP).hs
	cabal build $<

clean:
	rm -rf verilog/
	rm -f $(TOP).json
	rm -f $(TOP).asc
	rm -f $(TOP).bin
	rm -f *~
	rm -f *.hi
	rm -f *.o
	rm -f *.dyn_hi
	rm -f *.dyn_o

clean-all:
	$(MAKE) clean
	cabal clean

.PHONY: all clean clean-all prog build
