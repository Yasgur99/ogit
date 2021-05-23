MODULES=plumbing porcelain command state renderer ogit authors
OBJECTS=$(MODULES:=.cmo)
BYTES=$(MODULES:=.byte)
MLS_WITHOUT_MLIS=plumbing porcelain ogit renderer test
MLS=$(UNITS:=.ml) $(MLS_WITHOUT_MLIS:=.ml)
MLIS_WITH_MLS=authors command
MLIS=$(UNITS:=.mli) $(MLIS_WITH_MLS:=.mli)
DOCS=command.mli
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
		zip project.zip *.ml* *.json *.sh _tags .merlin .ocamlformat .ocamlinit LICENSE Makefile INSTALL.md
			
clean:
		ocamlbuild -clean
			rm -rf _doc.p

docs: docs-public

docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc $(MLS)