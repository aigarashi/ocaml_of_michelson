OM := ../src/ocaml_of_michelson

SRCS := $(wildcard *.tz)
OBJS := $(patsubst %.tz, %.ml,$(SRCS))

all: $(OBJS)

%.ml: %.tz
	$(OM) < $< > $@
	ocamlopt -i -o a.out $@

clean:
	rm -rf $(OBJS) *~
