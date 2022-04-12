# Makefile to build and check rtfSweave package

ifeq ($(OS), Windows_NT)
	R_CMD := rcmd.exe
	SHELL = c:\scoop\shims\bash.exe
else
	R_CMD := R CMD
endif



.PHONY: build
build:
	cd pkg; R CMD build rtfSweave

.PHONY: check
check:
	cd pkg; R CMD check rtfSweave_*.tar.gz

.PHONY: install
install:
	mkdir -p ~/Rlib
	cd pkg; R CMD INSTALL --library=~/Rlib rtfSweave_*.tar.gz

.PHONY: clean
clean:
	rm -Rf pkg/rtfSweave.Rcheck
	rm -f pkg/rtfSweave_*.tar.gz

.PHONY: web
web: www/content.html
www/content.html: www/content.md
	cd www; fossil test-markdown-render content.md > content.html


.PHONY: buildS
buildS:
	cd pkg; $(R_CMD) build rtfSweave

.PHONY: checkS
checkS:
	cd pkg; $(R_CMD) check rtfSweave*gz

.PHONY: installS
installS:
	cd pkg; $(R_CMD) INSTALL rtfSweave*gz


