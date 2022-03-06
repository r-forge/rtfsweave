# Makefile to build and check rtfSweave package

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
