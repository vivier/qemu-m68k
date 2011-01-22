TESTS=fmovecr fmove fmovem

all: $(TESTS)

%: %.S
	m68k-linux-gnu-gcc  -m68040 -nostartfiles  -nodefaultlibs  -nostdlib -o $@ $<

fmovecr: fmovecr.S
fmove: fmove.S
fmovem: fmovem.S

.PHONY: clean

clean:
	rm -f $(TESTS)
