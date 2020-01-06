NABat R Data Access and Manipulation Tools
===

Tools for interfacing R with NABat data services.

## Package Description

This package provides a interface to North American Bat Monitoring (NABat) data service's 

[NABat](https://nabatmonitoring.org/#/home)

## Package Status

Proof of concept, community input/collaboration welcome!

## Installation

To install the toolbox in R run the following commands in a R terminal

```R
if(!require(devtools)){install.packages(devtools)}
devtools::install_github("ropensci/ghql")
devtools::install_github("ennsk/nabatr", build_vignettes = TRUE)
library(nabatr)
```

## See Vignettes for examples
```
# In Rstudio (pop up in help panel)
??nabatr

# Web browser popup
browseVignettes('nabatr')
```


## Disclaimer
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](https://www.usgs.gov/visual-id/credit_usgs.html#copyright/ "official USGS copyright policy")

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

