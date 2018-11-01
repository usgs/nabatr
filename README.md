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
devtools::install_github("talbertc-usgs/nabatr")
library(nabatr)
```

## Examples

```r
library(nabatr)
library(sp)

examples:
hawaii_grts = get_grts_data('Hawaii')
spplot(hawaii_grts, zcol='own_NPS')

florida_grts = get_grts_data('Conus', query="state_n_1='Florida'")
spplot(florida_grts, zcol="lat")

two_counties = get_grts_data('Conus', query="((cnty_n_1='Colorado_Larimer')or(cnty_n_1='Colorado_Jackson'))")
spplot(two_conties, zcol='own_STATE')

```


## Disclaimer
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](https://www.usgs.gov/visual-id/credit_usgs.html#copyright/ "official USGS copyright policy")

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

