<!-- README.md is generated from README.Rmd. Please edit that file -->
![Last-changedate](https://img.shields.io/badge/last%20change-2019--03--26-brightgreen.svg)\](<https://github.com/nevrome/olympia.votiveweapons.article2019/commits/master>)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.5.0-brightgreen.svg)](https://cran.r-project.org/)
[![Licence](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/)
[![ORCiD](https://img.shields.io/badge/ORCiD-0000--0003--3448--5715-green.svg)](http://orcid.org/0000-0003-3448-5715)

Research compendium for ‘Workingtitle’
--------------------------------------

### Compendium DOI:

<http://dx.doi.org/>…

The files at the URL above will generate the results as found in the
publication. The files hosted at
<https://github.com/nevrome/olympia.votiveweapons.article2019> are the
development versions and may have changed since the paper was published

### Author of this repository:

Clemens Schmid (<clemens@nevrome.de>)

### Published in:

Graells R., Schmid C., ...

### Abstract:

...

### Keywords:

Olympia, Bronze Age, ...

### Overview of contents:

This repository contains the directories `code` and `data` to reproduce
the preparations, calculations and figure renderings in this paper. The
`plots` directory contains readily versions of the rendered plots.

### How to reproduce:

As the data and code in this repository are complete and self-contained,
it can be reproduced with any R environment (&gt; version 3.5.0). The
necessary package dependencies are documented in the `DESCRIPTION` file
and can be installed manually or automatically with
`devtools::install()`. If it’s not possible any more to construct a
working environment with these methods due to technological progress,
one can use the Docker image.

A Docker image is a lightweight GNU/Linux virtual computer that can be
run as a piece of software on Windows, Linux, and OSX. To capture the
complete computational environment used for this project we have a
Dockerfile that specifies how to make the Docker image that we developed
this project in. The Docker image includes all of the software
dependencies needed to run the code in this project, including the data
and code itself. To launch the Docker image for this project, first,
[install Docker](https://docs.docker.com/installation/) on your computer
and download the `.tar` file with the paper image [here](...) (not yet
available). At the Docker prompt, you can load and run the image with:

    docker load -i olympiavotiveweapons19_docker_image.tar
    docker run -e PASSWORD=olympia -dp 8787:8787 --name olympiavotiveweapons19 olympiavotiveweapons19

This will start a server instance of RStudio. Then open your web browser
at localhost:8787 or run `docker-machine ip default` in the shell to
find the correct IP address, and log in with
rstudio/olympiavotiveweapons19. Once logged in, use the Files pane
(bottom right) to navigate to the script files. More information about
using RStudio in Docker is available at the
[Rocker](https://github.com/rocker-org)
[wiki](https://github.com/rocker-org/rocker/wiki/Using-the-RStudio-image)
pages.

We developed and tested the package on this Docker container, so this is
the only platform that we’re confident it works on. It was built and
stored with:

    docker build -t olympiavotiveweapons19 .
    docker save -oolympiavotiveweapons19_docker_image.tar olympiavotiveweapons19

### Licenses:

Code: MIT <http://opensource.org/licenses/MIT> year: 2019, copyright
holder: Clemens Schmid

Data: Please contact Raimon Graells for permission to use this data.
