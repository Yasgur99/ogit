MODULES=git command state main 
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
		OCAMLRUNPARAM=b utop

build:
		$(OCAMLBUILD) $(OBJECTS)

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

