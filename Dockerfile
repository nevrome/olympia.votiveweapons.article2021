FROM rocker/tidyverse:3.5.2

RUN apt-get --yes --force-yes update -qq
RUN apt-get install --yes udunits-bin libproj-dev libgeos-dev libgdal-dev libgdal-dev libudunits2-dev

WORKDIR "home/rstudio"

COPY ./DESCRIPTION ./

RUN R -e "devtools::install('.', dep = TRUE)"

COPY ./code/ ./code/
COPY ./data/ ./data/

RUN mkdir plots

RUN find . -type df -exec chmod 777 {} \;
