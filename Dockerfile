FROM ghcr.io/kth-library/kontarion

WORKDIR /rocker_scripts

#COPY rocker_scripts/install_extra_pkgs.sh rocker_scripts/pkgs-cran rocker_scripts/pkgs-github ./
COPY rocker_scripts/* ./

RUN ./install_extra_pkgs.sh

WORKDIR /

RUN R -e 'install.packages(".", repos = NULL, type="source")'

#RUN installGithub.r --deps TRUE kth-library/bibliomatrix@fix-static-site

#CMD R -e "plumber::plumb(system.file('plumber', 'authorbased', 'plumber.R', package = 'bibliomatrix'))$run(port = 8080)"


# pak approach

#ENV RLIBS=/usr/local/lib/R/site-library:/usr/local/lib/R/library:/pak
#RUN mkdir -p /pak

#RUN R -e 'install.packages("pak", repos = "https://r-lib.github.io/p/pak/dev/")
#RUN R -q -e 'pak::pak(c("pkgload", "pkgbuild"), lib = "/pak")'
#RUN R -q -e 'pak::pak(c("r-lib/pkgcache", "r-lib/pkgdepends"), lib = "/pak")'
#RUN R -e '.libPaths()'
# oops - pak errors out below, but devtools::install_github works.... wierd...
#RUN R -q -e 'pak::pak("kth-library/bibliomatrix", lib = "/pak")'
