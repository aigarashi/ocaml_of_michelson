OM := ../src/main.exe

SRCS := $(wildcard *.tz)
OBJS := $(patsubst %.tz, %.ml,$(SRCS))

all: $(OBJS)

inst.cmi : inst.mli
	ocamlopt -c $<

%.ml: %.tz inst.cmi
	sed -n '/^code/,$$p' < $< | $(OM) -o $@ 2> /dev/null
	ocamlopt -i $@

clean:
	@rm -rf $(OBJS) inst.cmi *~
