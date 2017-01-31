R := R --vanilla -e '.libPaths("/home/stefan/lib/R")'
RCMD := R_LIBS=/home/stefan/lib/R R CMD

PKGFILES := $(shell find R man -type f ! -name "*.swp") DESCRIPTION NAMESPACE
PACKAGE := $(shell awk -F": +" '/^Package/ { print $$2 }' DESCRIPTION)
VERSION := $(shell awk -F": +" '/^Version/ { print $$2 }' DESCRIPTION)
R_PKG_tgz := $(PACKAGE)_$(VERSION).tar.gz

all: build check install
build: $(R_PKG_tgz)
check: $(PACKAGE).Rcheck
install: libtmp/$(PACKAGE)

$(R_PKG_tgz): $(PKGFILES)
	$(R) -e 'roxygen2::roxygenise(package.dir=".")' && \
	$(RCMD) build .
	

$(PACKAGE).Rcheck: $(R_PKG_tgz)
	$(RCMD) check $(R_PKG_tgz)

libtmp/$(PACKAGE): $(R_PKG_tgz)
	$(RCMD) INSTALL $(R_PKG_tgz) -l libtmp

check-as-cran:
	R_LIBS=/home/stefan/lib/R R CMD check $(R_PKG_tgz) --as-cran && \
	R_LIBS=/home/stefan/lib/R R-devel CMD check $(R_PKG_tgz) --as-cran


