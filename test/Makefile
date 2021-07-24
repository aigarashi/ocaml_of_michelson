OM := ../src/ocaml_of_michelson

SRCS := $(wildcard *.tz)
OBJS := $(patsubst %.tz, %.ml,$(SRCS))

all: inst.cmi $(OBJS)

inst.cmi : inst.mli
	ocamlopt -c $<

%.ml: %.tz
	$(OM) < $< > $@
	ocamlopt -i $@

clean:
	@rm -rf $(OBJS) inst.cmi *~
