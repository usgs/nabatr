#############################################################################
#     _   _____    ____        __  ____ 
#    / | / /   |  / __ )____ _/ /_/ __ \
#   /  |/ / /| | / __  / __ `/ __/ /_/ /
#  / /|  / ___ |/ /_/ / /_/ / /_/ _, _/ 
# /_/ |_/_/  |_/_____/\__,_/\__/_/ |_|
#
# R Tools for accessing and manipulating North American Bat Monitoring data
# url:  
# Github: 
# Written by: Colin B Talbert 
# Created: 2018-10-31
# Based on wfs access code from sbtools:
# https://github.com/USGS-R/sbtools/blob/869282738eeea73225c8cc388ca4d189aa987899/R/item_get_wfs.R
#############################################################################

library(xml2)
library(httr)
library(rgdal)

HASH_DICT <- list(Alaska = '5b7b54efe4b0f5d578846149',
                  Canada = '5b7b559de4b0f5d57884614d',
                  Conus = '5b7b563ae4b0f5d57884615b',
                  Hawaii = '5b7b5641e4b0f5d57884615d',
                  Mexico = '5b7b5658e4b0f5d57884615f',
                  PuertoRico = '5b7b5660e4b0f5d578846161')

URL_TEMPLATE <- "https://www.sciencebase.gov/catalogMaps/mapping/ows/HASH?service=wfs&request=getcapabilities&version=1.1.0"

get_grts_data = function(grid_frame, query){
  #' Return the spatial layer from the associated wfs query 
  #'
  #' @param grid_frame String name of the grid frame to return
  #' @param query String query to apply to the request ("state_n_1='Florida'")
  #' for query syntac help see: https://docs.geoserver.org/stable/en/user/tutorials/cql/cql_tutorial.html
  #' 
  
  grid_frame <- normalize_grid_frame(grid_frame)
  hash = HASH_DICT[[grid_frame]]
  layer_names = get_wfs_layer_name(hash)
  wfs_url = sub('HASH', hash, URL_TEMPLATE, ignore.case = TRUE)
  wfs_request = sub('request=GetCapabilities', 'request=GetFeature', wfs_url, ignore.case = TRUE)
  wfs_request = paste0(wfs_request, '&outputformat=shape-zip&format_options=filename:shape-zip&typename=', layer_names)
  
  if (missing(query) == FALSE) {
    wfs_request = paste0(wfs_request, '&CQL_FILTER=', query)
  } 
  
  out_fname = tempfile(fileext = '.shp')
  dirname = file.path(tempdir(), basename(tempfile()))
  
  httr::GET(wfs_request, httr::write_disk(out_fname))
  
  unzip(out_fname, exdir = dirname)
  
  layer_sp = rgdal::readOGR(dirname, strsplit(layer_names, ':')[[1]][2])
  
  return(layer_sp)
} 

get_wfs_layer_name = function(hash){
  #' Return the wfs layer name from the WFS associated with a SB item 
  #'
  #' @param hash String SB hash of the item containing the wfs
  #' 
  wfs_url = sub('HASH', hash, URL_TEMPLATE, ignore.case = TRUE)
  caps = xml2::read_xml(wfs_url)
  layer_names = xml2::xml_text(xml2::xml_find_all(caps, '//d1:FeatureType/d1:Name', xml2::xml_ns(caps)))
  layer_names = layer_names[!is.na(layer_names) & !layer_names %in% c('sb:boundingBox', 'sb:footprint')]
  
  return(layer_names)
}

normalize_grid_frame = function(grid_frame){
  #' normalize the passed grid_frame name to allow for flexibility in calling
  #'
  #' @param grid_frame String name of the grid frame to return

  grid_frame = switch(gsub(" ", "", tolower(grid_frame)),
         ak="Alaska",
         alaska='Alaska',
         ca="Canada",
         can="Canada",
         canada="Canada",
         conus='Conus',
         us='Conus',
         usa='Conus',
         unitedstates='Conus',
         hawaii='Hawaii',
         hi='Hawaii',
         mex='Mexico',
         mx='Mexico',
         mexico='Mexico',
         mexico='Mexico',
         mexico='Mexico',
         puertorico='PuertoRico',
         pr='PuertoRico',
         stop('The supplied grid_frame must be one of "Alaska", "Canada", "Conus", "Hawaii", "Mexico", or "Puerto Rico"'))
  
  return(grid_frame)
}

# 
# 
# examples:
# hawaii_grts = get_grts_data('Hawaii')
# spplot(hawaii_grts, zcol='own_NPS')
# 
# florida_grts = get_grts_data('Conus', query="state_n_1='Florida'")
# spplot(florida_grts, zcol="lat")
# 
# two_counties = get_grts_data('Conus', query="((cnty_n_1='Colorado_Larimer')or(cnty_n_1='Colorado_Jackson'))")
# spplot(two_conties, zcol='own_STATE')




