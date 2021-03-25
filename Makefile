MODULES=ogit plumbing 
OBJECTS=$(MODULES:=.cmo)
BYTES=$(MODULES:=.byte)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=ogit.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build

utop: build
		OCAMLRUNPARAM=b utop

build:
		$(OCAMLBUILD) $(BYTES)

test:
		$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

run:
		$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

check:
		@bash check.sh
			
finalcheck:
		@bash check.sh final

zip:
		zip project.zip *.ml* *.json *.sh _tags .merlin .ocamlformat .ocamlinit LICENSE Makefile	
			
clean:
		ocamlbuild -clean
			rm -rf _doc.public _doc.private 

