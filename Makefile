SHELL := /bin/bash
LIBDIR=/Users/kyleb/Rlibs/lib
PACKAGE=lastdose
VERSION=$(shell grep Version DESCRIPTION |awk '{print $$2}')
TARBALL=${PACKAGE}_${VERSION}.tar.gz
PKGDIR=.
CHKDIR=.

## Set libPaths:
export R_LIBS=${LIBDIR}

test:
	Rscript -e 'library(testthat)' -e 'test_file("tests/testthat.R")'

rhub:
	Rscript -e 'rhub::check_for_cran()'

cran:
	make doc
	make build
	R CMD CHECK ${TARBALL} --as-cran

covr:
	Rscript inst/covr/covr.R

ec:
	echo ${VERSION}

all:
	make doc
	make build
	make install

.PHONY: doc
doc:
	Rscript -e 'library(devtools); document()'

build:
	R CMD build --md5 $(PKGDIR)

install:
	R CMD INSTALL --install-tests ${TARBALL}

install-build:
	R CMD INSTALL --build --install-tests ${TARBALL}

check:
	make doc
	make build
	R CMD CHECK ${TARBALL} -o ${CHKDIR}

readme:
	Rscript -e 'rmarkdown::render("README.Rmd")'

site:
	Rscript -e 'pkgdown::build_site()'

spelling:
	Rscript inst/script/_spelling.R
