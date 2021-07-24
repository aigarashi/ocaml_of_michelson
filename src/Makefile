OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLYACC=menhir
OCAMLLEX=ocamllex
OCAMLFIND=ocamlfind
INCLUDES=                 # all relevant -I options here
OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

PROGNAME=ocaml_of_michelson

# The list of object files for prog1
OBJS=syntax.cmx mcParser.cmx mcLexer.cmx instSpec.cmx trans.cmx inliner.cmx main.cmx

DEPEND += mcLexer.ml mcParser.ml

all: $(DEPEND) $(OBJS)
	$(OCAMLFIND) $(OCAMLOPT) -o $(PROGNAME) $(OCAMLFLAGS) ocamlcommon.cmxa $(OBJS)   -package compiler-libs.common

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLFIND) $(OCAMLC) $(OCAMLFLAGS) -c $< -package compiler-libs.common

.mli.cmi:
	$(OCAMLFIND) $(OCAMLC) $(OCAMLFLAGS) -c $< -package compiler-libs.common

.ml.cmx:
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPTFLAGS) -c $< -package compiler-libs.common

mcParser.ml mcParser.mli: mcParser.mly	
	@rm -f $@
	$(OCAMLYACC) -v $<
	@chmod -w $@

mcLexer.ml: mcLexer.mll
	@rm -f $@
	$(OCAMLLEX) $<
	@chmod -w $@

# Clean up
clean:
	rm -f $(PROGNAME)
	rm -f *.cm[iox] *.o *~ mcParser.ml mcPparser.mli parser.output mcLexer.ml .depend

# Dependencies
depend:: $(DEPEND)
	$(OCAMLDEP) $(INCLUDES) -native *.mli *.ml > .depend

-include .depend
