FROM ghcr.io/kth-library/kontarion

# use R repo source https://packagemanager.rstudio.com/cran/__linux__/focal/latest
COPY inst/docker/rocker_scripts/.Rprofile /root/.Rprofile

WORKDIR /rocker_scripts

#COPY rocker_scripts/install_extra_pkgs.sh rocker_scripts/pkgs-cran rocker_scripts/pkgs-github ./
COPY inst/docker/rocker_scripts/* ./

RUN ./install_extra_pkgs.sh

WORKDIR /

COPY . /kthcorpus

RUN R -e 'update.packages(ask = FALSE)'
RUN R -e 'remotes::install_local("kthcorpus", dependencies = TRUE)'

# install minio client
RUN cd /usr/local/bin && \
  wget https://dl.min.io/client/mc/release/linux-amd64/mc && \
  chmod +x mc

# pak approach

#ENV RLIBS=/usr/local/lib/R/site-library:/usr/local/lib/R/library:/pak
#RUN mkdir -p /pak

#RUN R -e 'install.packages("pak", repos = "https://r-lib.github.io/p/pak/dev/")
#RUN R -q -e 'pak::pak(c("pkgload", "pkgbuild"), lib = "/pak")'
#RUN R -q -e 'pak::pak(c("r-lib/pkgcache", "r-lib/pkgdepends"), lib = "/pak")'
#RUN R -e '.libPaths()'
# oops - pak errors out below, but devtools::install_github works.... wierd...
#RUN R -q -e 'pak::pak("kth-library/bibliomatrix", lib = "/pak")'
