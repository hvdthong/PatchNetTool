PARMAP=$(shell ocamlfind query parmap)

TARGET=getinfo
LEXER_SOURCES = lexer_c.mll
SRC=lcommon.ml patch.ml $(LEXER_SOURCES:.mll=.ml) \
decomment.ml collect_diff.ml words.ml getinfo.ml

INCLUDEDIRS= $(PARMAP)

SYSLIBS=str.cma unix.cma bigarray.cma nums.cma $(PARMAP)/parmap.cma
OPTSYSLIBS=$(SYSLIBS:.cma=.cmxa)

OBJS=    $(SRC:.ml=.cmo)
OPTOBJS= $(SRC:.ml=.cmx)

EXEC=$(TARGET)
OPTEXEC=$(TARGET).opt

INCLUDES=$(INCLUDEDIRS:%=-I %)
OCAMLCFLAGS=-g
OCAMLC=ocamlc $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT=ocamlopt $(OPTFLAGS) $(INCLUDES)
OCAMLDEP=ocamldep
OCAMLLEX=ocamllex

GENERATED= $(LEXER_SOURCES:.mll=.ml)

all: $(EXEC)
opt: $(OPTEXEC)

$(EXEC): $(OBJS)
	$(OCAMLC) -o $(EXEC) $(SYSLIBS) $(OBJS)
$(OPTEXEC): $(OPTOBJS)
	$(OCAMLOPT) -o $(OPTEXEC) $(OPTSYSLIBS) $(OPTOBJS)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $<

$(LEXER_SOURCES:.mll=.ml) :	$(LEXER_SOURCES)
	$(OCAMLLEX) $(LEXER_SOURCES)

# clean rule for others files
clean::
	rm -f *.cm[iox] *.o
	rm -f *~ .*~ #*#
	rm -f .depend

depend:
	$(OCAMLDEP) *.mli *.ml > .depend

.depend:
	$(OCAMLDEP) *.mli *.ml > .depend

include .depend

# ----------------------------------------------------------------------------
# Collecting the data

# The following also produces newres.dict and newtxt.out
newres.out: getinfo
	./getinfo --max-lines 100 --match-stable-lines --match-stable-number > newres.tmp 2>&1

get_function_calls: get_function_calls.ml
	ocamlc -g -o get_function_calls str.cma unix.cma get_function_calls.ml

run: newres.out get_function_calls
	./get_function_calls newres.out > newres_funcalls.out
