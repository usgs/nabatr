#############################################################################
#     _   _____    ____        __  ____
#    / | / /   |  / __ )____ _/ /_/ __ \
#   /  |/ / /| | / __  / __ `/ __/ /_/ /
#  / /|  / ___ |/ /_/ / /_/ / /_/ _, _/
# /_/ |_/_/  |_/_____/\__,_/\__/_/ |_|
#
# R Tools for accessing and manipulating North American Bat Monitoring data
#
# Written by: Kyle Enns
# Created: 2019-10-2
#############################################################################


#' @title Read in GRTS lookup csv for CONUS
#'
#' @description
#' Reads in dataframe that contains all of the GRTS cells and their
#' 4 corners to build shapefiles from.  Projection: WGS84 and extent: CONUS
#' @keywords species, bats, NABat, grts, CONUS
#' @examples
#'
#' \dontrun{
#' nabatr::grts_coords
#' }
#'
#' @export
#'
grts_coords = read.csv('data/GRTS_coords_CONUS.csv', stringsAsFactors=FALSE)


#' @title Build leaflet map for Acoustic Stationary report in CONUS
#'
#' @import rmarkdown
#' @import leaflet
#' @import htmlwidgets
#' @import htmltools
#'
#' @description
#' Builds a leaflet map using a vector list of grts cells to add to a leaflet map.  Shows
#' where the project's grts cells live spatially.  Keep in mind that the project_id
#' and all_grts must be in CONUS.
#' @param all_grts Character Vector all grts cell ids found from the survey_df dataframe by running
#' unique(survey_df$grts_cell_id)
#' @param grts_with_data (optional) Character Vector or NULL
#' @keywords bats, NABat, GQL
#' @examples
#'
#' \dontrun{
#' map = get_grts_leaflet_map(all_grts       = unique(survey_df_$grts_cell_id),
#'                            grts_with_data = unique(auto_nights_df_$GRTS))
#' }
#'
#' @export
#'
get_grts_leaflet_map = function(all_grts, grts_with_data = NULL){

  # Create grts_template_df dataframe and merge with grts_coords
  grts_template_df = data.frame(GRTS_ID = all_grts)
  grts_df = plyr::join(grts_template_df, grts_coords, by = c('GRTS_ID'), type = "left")

  # Creating map with an Imagery layer
  m = leaflet() %>% #addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.jpg",
    #attribution = 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community',
    #group = "World Imagery")
    addTiles()
  # Loop through all all_grts, create a polygon for each, and add to the leaflet map m
  count = 0
  for (grts_cell in all_grts){
    # Setting color to green or red based on if the grts cell has data or not
    if (is.null(grts_with_data)){
      color_ = '#ff0000'
      color_2 = 'red'
    }else {
      if (grts_cell %in% grts_with_data){
        color_  = '#198a00'
        color_2 = '#198a00'
      } else {
        color_  = '#ff0000'
        color_2 = 'red'
      }
    }

    # Content for the hover
    content = paste0(as.character(grts_cell))
    # Content for the popup (on click)
    content_popup = paste0('<b style = "color:',color_2,';" >GRTS cell</b>', '<br> <div style = "color:', color_2, ';" >', content, '</div>')

    # Creating lat/lon points for the grts polygon
    this_row = subset(grts_df,grts_df$GRTS_ID == grts_cell)
    ll1 = as.numeric(rev(as.character(strsplit(as.character(this_row$lowerleft),',')[[1]])))
    ll2 = as.numeric(rev(as.character(strsplit(as.character(this_row$upperleft),',')[[1]])))
    ll3 = as.numeric(rev(as.character(strsplit(as.character(this_row$upperright),',')[[1]])))
    ll4 = as.numeric(rev(as.character(strsplit(as.character(this_row$lowerright),',')[[1]])))
    lngs = c(ll1[1],ll2[1],ll3[1],ll4[1],ll1[1])
    lats = c(ll1[2],ll2[2],ll3[2],ll4[2],ll1[2])

    # Add this grts polygon to the leaflet map m
    m = m %>% addPolygons(lat = lats, lng = lngs, popup = content_popup,
      color = color_, weight = 1.5, opacity = 1, group = color_,
      label = content,
      labelOptions = labelOptions(style = list('font-size' = '14px',
        'color' = color_2,
        'box-shadow' = '3px 3px rgba(0,0,0,0.25)',
        'border-color' = 'rgba(0,0,0,0.5)',
        'border-radius' = '5px',
        'padding' = '5px 5px 5px 5px')))# padding order is top, right, bottom, left

  }
  # Add legend to leaflet map
  m = m %>% addLegend('bottomright',labels = c('Has survey data', 'No survey data'), colors = c('#198a00', '#ff0000'), opacity =1)

  # Return the leaflet map
  return (m)
}


#' @title Build shapefile from GRTS IDs
#'
#' @import sp
#' @import raster
#' @import rgdal
#' @import plyr
#'
#' @description
#' Builds a grts shapefile from the grts_ids parameter.  note: uses rgdal and spatial packages.
#' @param grts_ids Character Vector GRTS Ids
#' @keywords species, bats, NABat, grts, CONUS
#' @examples
#'
#' \dontrun{
#' }
#'
#' @export
#'
get_grts_shp = function(grts_ids){
  # Call Build polygons dataframe from GRTS IDs function
  grts_shp_df = get_grts_shp_df(grts_ids)
  # Call Build spatial polygons dataframe from GRTS shape dataframe
  grts_spdf   = get_spdf_from_polys_df(grts_shp_df)
  return (grts_spdf)
}


#' @title Build polygons dataframe from GRTS IDs
#'
#' @import sp
#' @import raster
#' @import rgdal
#' @import plyr
#'
#' @param grts_ids Character Vector GRTS Ids
#'
#' @export
#'
get_grts_shp_df = function(grts_ids){
  grts_template_df = data.frame(GRTS_ID = as.integer(grts_ids))
  grts_df = plyr::join(grts_template_df, grts_coords, by = c('GRTS_ID'), type = "left")

  print (grts_ids)
  polys_df = data.frame()
  for (grts_id in grts_ids){
    this_row = subset(grts_df,grts_df$GRTS_ID == grts_id)
    ll1 = as.numeric(rev(as.character(strsplit(as.character(this_row$lowerleft),',')[[1]])))
    ll2 = as.numeric(rev(as.character(strsplit(as.character(this_row$upperleft),',')[[1]])))
    ll3 = as.numeric(rev(as.character(strsplit(as.character(this_row$upperright),',')[[1]])))
    ll4 = as.numeric(rev(as.character(strsplit(as.character(this_row$lowerright),',')[[1]])))
    lngs = c(ll1[1],ll2[1],ll3[1],ll4[1],ll1[1])
    lats = c(ll1[2],ll2[2],ll3[2],ll4[2],ll1[2])

    if (!is.na(ll1)){
      poly_df = data.frame(lng = lngs, lat = lats, GRTS = grts_id)
      if (dim(polys_df)[1]==0){
        polys_df = poly_df
      }else {
        polys_df = rbind(polys_df, poly_df)
      }
    }
  }
  return(polys_df)
}


#' @title Build spatial polygons dataframe from GRTS shape dataframe
#'
#' @import sp
#' @import raster
#' @import rgdal
#' @import plyr
#'
#' @param grts_shp_df Dataframe
#'
#' @export
#'
get_spdf_from_polys_df = function(grts_shp_df){
  # Seperate polygons by grts id
  polys_list  = split(grts_shp_df, grts_shp_df$GRTS)
  # Remove id column from split polygon dfs
  polys_list_ = lapply(polys_list, function(poly_) { poly_["GRTS"] = NULL; poly_ })
  polys       = lapply(polys_list_, Polygon)
  # Add id into the Polygons before converting into a SpatialPolygons object
  polys_ = lapply(seq_along(polys), function(i) Polygons(list(polys[[i]]),
                 ID = names(polys_list_)[i]))
  # Create SpatialPolygons object
  all_polys = SpatialPolygons(polys_, proj4string = CRS("+proj=longlat +datum=WGS84") )

  # Create SpatialPolygonsDataFrame object (adds id)
  all_polys_spdf = SpatialPolygonsDataFrame(all_polys,
                                            data.frame(id = unique(grts_shp_df$GRTS),
                                                       row.names = unique(grts_shp_df$GRTS)))
  return (all_polys_spdf)
}

