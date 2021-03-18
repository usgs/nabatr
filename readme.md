NABat R - Data Access and Manipulation Tools <a href='https://nabatmonitoring.org/#/home'><img src='./inst/templates/NABat_Circle_color.jpg' align="right" height="139" /></a>
===

Tools for interfacing R with NABat data services.

## Package Description

This package provides an interface to North American Bat Monitoring (NABat) data service's.  For full access to NABatR, be sure to go to the link below and setup an account with NABat!

[NABat](https://nabatmonitoring.org/#/home)

## Package Status

Proof of concept, community input/collaboration welcome!

## Installation

To install the toolbox in R run the following commands in a R terminal

```R
# If you are on windows, be sure to install Rtools from 
##  https://cran.r-project.org/bin/windows/Rtools and follow 
##  install directions from there

install.packages(c('devtools','dplyr','flextable','ggplot2','htmltools','htmlwidgets',
  'httr','jsonlite','leaflet','lubridate','magrittr','maps','maptools','mapview','officer',
  'plotly','plyr','raster','rgdal','rmarkdown','sp','xml2', 'stringr'))

# Download Master branch and if it's been updated, reinstall latest version. build_vignettes
##  allows you to run the browseVignettes() function in the next cell. force will rebuild
##  the package if any new changes have been made
devtools::install_github('usgs/nabatr', build_vignettes = TRUE, upgrade = 'never', force = TRUE)

library(nabatr)
```
note: Refreshing R on windows: (Ctrl+Shift+F10)  Mac: (Cmd+Shift+Fn+F10)

## See Vignettes for examples (run in RStudio) or See .Rmd files in Examples directory
```
# RStudio pop up in Help tab
??nabatr

# Web browser popup.  Click the HTML link for a vignette and
##  copy and paste the grey chunks of code into your R file.
##  Change any place holders in the code and Run.
browseVignettes('nabatr')

# If browseVignettes doesn't automatically open up a browser window,
##  you can use the following code to get access to the vignettes
##  if they were set TRUE on the install_github()
vignette(package = 'nabatr')

# Use the names from the above command (ex):
# # note: This will popup in your Help tab in Rstudio
vignette('get_sa_data')
```

If you are on windows and want to build the SA or MA report, be
sure to download phantomjs.  See below
```
webshot::install_phantomjs()
```

## Notes
The Examples and Vignettes directories are essentially mirrors of each other.  One is formatted into code blocks without {r} and the other
is formatted with {r}.  ex:  ```{r} code that runs in R (examples directory)``` and ``` code that does not run in R (vignettes directory) ``` 

If ever running any of the functions in branch = 'beta', be sure you are logged onto a USGS machine and on the VPN otherwise the code won't work.  
Use branch = 'beta' in the login and refresh token methods in order to use any get functions with branch = 'beta'.  


## Disclaimer
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](https://www.usgs.gov/visual-id/credit_usgs.html#copyright/ "official USGS copyright policy")

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

