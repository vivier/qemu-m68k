TESTS=fmovecr fmove fmovem fsub fdiv fmul fabs fgetexp fscale flogn fetox \
      bfins

all: $(TESTS)

%: %.S
	m68k-linux-gnu-gcc  -m68040 -nostartfiles  -nodefaultlibs  -nostdlib -o $@ $<

.PHONY: clean

clean:
	rm -f $(TESTS)
