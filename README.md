## Research compendium for ‘Votive Weapons in the Panhellenic Sanctuary of Olympia (10th - 5th Centuries BC): Diachronic Development’

### Compendium DOI:

<http://dx.doi.org/10.17605/OSF.IO/RV2ZF>

The files at the URL above will generate the results as found in the publication. The files hosted at <https://github.com/nevrome/olympia.votiveweapons.article2021> are the development versions and may have changed since the paper was published.

### Authors of this repository:

- Clemens Schmid [![ORCiD](https://img.shields.io/badge/ORCiD-0000--0003--3448--5715-green.svg)](http://orcid.org/0000-0003-3448-5715)
- Raimon Graells i Fabregat [![ORCiD](https://img.shields.io/badge/ORCiD-0000--0002--9057--7510-green.svg)](http://orcid.org/0000-0002-9057-7510)

### Published in:

**(in print)**: Graells R., Schmid C., Votive Weapons in the Panhellenic Sanctuary of Olympia (10th - 5th Centuries BC): Diachronic Development

### Overview of contents:

This repository contains the directories `code` and `data` to reproduce the data preparation, calculations and figure renderings in this paper. The `plots` directory contains readily rendered versions of the plots, with the figure captions in another README.md file in this folder.

### How to reproduce:

As the data and code in this repository are complete and self-contained, it can be reproduced with only an R environment (tested for R v4.1.0). The necessary package dependencies are documented in the `DESCRIPTION` file and can be installed manually or automatically with `devtools::install(repos = "https://mran.microsoft.com/snapshot/2021-08-24")`.

*Edit 2024-04-11*: The Microsoft R Application Network (MRAN) was [retired in 2023](https://techcommunity.microsoft.com/t5/azure-sql-blog/microsoft-r-application-network-retirement/ba-p/3707161), so `devtools::install(repos = "https://mran.microsoft.com/snapshot/2021-08-24")` will not work any more. The individual package versions used for the analysis are documented in the `DESCRIPTION` file. It may be possible to recover them from other CRAN snapshot services like e.g. the [Groundhog R Archive Neighbor (GRAN)](http://groundhogr.com/gran).

### Licenses:

[![Licence](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/) year: 2021, copyright holder: Clemens Schmid
