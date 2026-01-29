# TADAShinyAnalyze

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

## 1. Overview

The overall goal of the `TADAShinyAnalyze` shiny app is to provide a third module to the `TADAShiny` shiny app where users can analyze monitoring location and/or assessment unit (AU) information to support water quality management.

## 2. Installation

First install the most recent version of the EPATADA R Package from GitHub. Then, install and run the TADAShinyJoinToAU application:

``` r
if(!"remotes"%in%installed.packages())
  {install.packages("remotes")}
library(remotes)

remotes::install_github("USEPA/EPATADA", ref = "develop", dependencies = TRUE, force = TRUE)
library(EPATADA)

remotes::install_github("USEPA/TADAShinyAnalyze", ref = "main", dependencies = TRUE, force = TRUE)
library(TADAShinyAnalyze)

run_app()
```

## 3. Running the Web Version

EPA Staging Link: <https://rstudio-connect.dmap-stage.aws.epa.gov/content/019dcc7e-863b-426a-a2a5-1813f53f5702/>

## 4. Open-Source Code Policy

Effective August 8, 2016, the [OMB Mandate: M-16-21; Federal Source Code Policy: Achieving Efficiency, Transparency, and Innovation through Reusable and Open Source Software](https://obamawhitehouse.archives.gov/sites/default/files/omb/memoranda/2016/m_16_21.pdf) applies to new custom-developed code created or procured by EPA consistent with the scope and applicability requirements of Office of Management and Budget's (OMB's) Federal Source Code Policy. In general, it states that all new custom-developed code by Federal Agencies should be made available and reusable as open-source code.

The EPA specific implementation of OMB Mandate M-16-21 is addressed in the [System Life Cycle Management Procedure](https://www.epa.gov/irmpoli8/policy-procedures-and-guidance-system-life-cycle-management-slcm). EPA has chosen to use GitHub as its version control system as well as its inventory of open-source code projects. EPA uses GitHub to inventory its custom-developed, open-source code and generate the necessary metadata file that is then posted to code.gov for broad reuse in compliance with OMB Mandate M-16-21.

If you have any questions or want to read more, check out the [EPA Open Source Project Repo](https://github.com/USEPA/open-source-projects) and [EPA's Interim Open Source Code Guidance](https://www.epa.gov/developers/open-source-software-and-code-repositories).

## 5. Contributing

We encourage stakeholders to test the functionality and provide feedback. Moreover, open source software provides an avenue for water quality data originators and users to develop and share code, and we welcome your contributions! We hope to build a collaborative community dedicated to this effort where TADA users and contributors can discover, share and build the functionality over time.

-   [How to Contribute](https://usepa.github.io/EPATADA/articles/CONTRIBUTING.html)

## 6. License

All contributions to this project will be released under the CCO-1.0 license file dedication. By submitting a pull request or issue, you are agreeing to comply with this waiver of copyright interest.

## 7. Disclaimer

This United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.

## 8. Contact

If you have any questions, please reach out to the TADA Team at mywaterway\@epa.gov.
