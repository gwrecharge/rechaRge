#
# Rock R Server Dockerfile with DataSHIELD
#
# https://github.com/datashield/docker-rock
#

FROM obiba/rock:snapshot

LABEL EPFL-ENAC-IT4R <enacit4research@epfl.ch>

ENV RECHARGE_VERSION main

ENV ROCK_LIB /var/lib/rock/R/library

# Additional system dependencies
RUN apt-get update && \
  DEBIAN_FRONTEND=noninteractive apt-get --yes install libmysqlclient-dev && \
  DEBIAN_FRONTEND=noninteractive apt-get --yes autoremove

# Update R packages
#RUN Rscript -e "update.packages(ask = FALSE, repos = c('https://cloud.r-project.org'), instlib = '/usr/local/lib/R/site-library')"

# Install new R packages
RUN Rscript -e "install.packages(c('caRamel', 'ggplot2', 'pak'), repos = c('https://cloud.r-project.org'), lib = '/usr/local/lib/R/site-library')"
RUN Rscript -e "pak::pkg_install('gwrecharge/rechaRge@$RECHARGE_VERSION', lib = '$ROCK_LIB')"
RUN chown -R rock $ROCK_LIB
