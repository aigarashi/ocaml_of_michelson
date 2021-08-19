OM := ../src/ocaml_of_michelson

SRCS := $(wildcard *.tz)
OBJS := $(patsubst %.tz, %.ml,$(SRCS))

all: $(OBJS)

inst.cmi : inst.mli
	ocamlopt -c $<

%.ml: %.tz inst.cmi
	sed -n '/^code/,$$p' < $< | $(OM) -o $@
	ocamlopt -i $@

clean:
	@rm -rf $(OBJS) inst.cmi *~
