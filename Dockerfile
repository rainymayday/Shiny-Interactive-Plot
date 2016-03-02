FROM r-base:latest
MAINTAINER Rainy "rainy.mayday2013@gmail.com"
RUN apt-get update && apt-get install -y -t   unstable\
        sudo \
        gdebi-core \
        pandoc \
        pandoc-citeproc \
        libcairo2-dev/unstable \
        libxt-dev\
        libcurl4-openssl-dev\
        libpcre++-dev\
        openjdk-7-jdk\
        libxml2-dev\
        libssl-dev


# Download and install libssl 0.9.8
RUN wget --no-verbose http://ftp.us.debian.org/debian/pool/main/o/openssl/libssl0.9.8_0.9.8o-4squeeze14_amd64.deb && \
        dpkg -i libssl0.9.8_0.9.8o-4squeeze14_amd64.deb && \
        rm -f libssl0.9.8_0.9.8o-4squeeze14_amd64.deb

    # Download and install shiny server
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
        VERSION=$(cat version.txt)  && \
        wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
        gdebi -n ss-latest.deb && \
        rm -f version.txt ss-latest.deb

RUN R CMD javareconf


RUN R -e "install.packages(c('shiny','XLConnect', 'lubridate', 'devtools','Rcpp','ISOweek','scales','plyr','DT'), repos='https://cran.rstudio.com/')"
RUN R -e 'setRepositories(ind=1:6); \
  options(repos="http://cran.rstudio.com/"); \
  library(devtools); \
  install_github("ramnathv/rCharts");'


COPY /myapp/* /srv/shiny-server/

EXPOSE 3838

COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod +x /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]
