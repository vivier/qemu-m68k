TESTS=fmovecr fmove fmovem fsub fdiv fmul fabs fgetexp fscale flogn fetox \
      bfins inf cas cas2

all: $(TESTS)

%: %.S
	m68k-linux-gnu-gcc -g -m68040 -nostartfiles  -nodefaultlibs  -nostdlib -o $@ $<

.PHONY: clean

clean:
	rm -f $(TESTS)
