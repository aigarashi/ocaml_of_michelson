OM := ../src/ocaml_of_michelson

SRCS := $(wildcard *.tz)
OBJS := $(patsubst %.tz, %.ml,$(SRCS))

all: $(OBJS)

inst.cmi : inst.mli
	ocamlopt -c $<

%.ml: inst.cmi %.tz
	grep '^code' -A -1 $< | $(OM) > $@
	ocamlopt -i $@

clean:
	@rm -rf $(OBJS) inst.cmi *~
