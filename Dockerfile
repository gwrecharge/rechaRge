#
# Rock R Server Dockerfile with DataSHIELD
#
# https://github.com/datashield/docker-rock
#

FROM obiba/rock:snapshot

LABEL ENAC-IT4R <enacit4r@epfl.ch>

ENV RECHARGE_VERSION main

ENV ROCK_LIB /var/lib/rock/R/library

# Additional system dependencies
#RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y cmake

# Update R packages
#RUN Rscript -e "update.packages(ask = FALSE, repos = c('https://cloud.r-project.org'), instlib = '/usr/local/lib/R/site-library')"

# Install new R packages
RUN Rscript -e "remotes::install_github('gwrecharge/rechaRge', ref = '$RECHARGE_VERSION', repos = c('https://cloud.r-project.org'), dependencies = TRUE, upgrade = FALSE, lib = '$ROCK_LIB')"
RUN Rscript -e "install.packages(c('caRamel', 'ggplot2'), repos = c('https://cloud.r-project.org'), lib = '/usr/local/lib/R/site-library')"
RUN chown -R rock $ROCK_LIB
