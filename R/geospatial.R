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
#
# USGS DISCLAIMER:  This software is in the public domain because it contains
# materials that originally came from the U.S. Geological Survey, an agency
# of the United States Department of Interior. For more information, see the
# [official USGS copyright policy]
# (https://www.usgs.gov/visual-id/credit_usgs.html#copyright/
# "official USGS # copyright policy")
#
# Although this software program has been used by the U.S. Geological Survey
# (USGS), no warranty, expressed or implied, is made by the USGS or the U.S.
# Government as to the accuracy and functioning of the program and related
# program material nor shall the fact of distribution constitute any such
# warranty, and no responsibility is assumed by the USGS in connection
# therewith.
#
# This software is provided "AS IS."
#############################################################################


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
#'
#' @param all_grts Character Vector all grts cell ids found from the survey_df dataframe by running
#' unique(survey_df$grts_cell_id)
#' @param project_df Dataframe output from get_projects()
#' @param project_id Numeric or String a project id
#' @param grts_with_data (optional) Character Vector or NULL
#' @keywords bats, NABat, GQL
#'
#' @export

get_grts_leaflet_map = function(
  all_grts,
  project_df,
  project_id,
  grts_with_data = NULL){

  project_id_ = project_id
  grts_fname = as.character(subset(project_df,
                            project_df$project_id == project_id_)$sample_frame_short)
  # Get grts_fname_df
  grts_fname_df = grts_lookup_df[grts_fname][[1]]
  # Create grts_template_df dataframe and merge with grts_fname_df
  grts_template_df = data.frame(GRTS_ID = all_grts)
  grts_df = plyr::join(grts_template_df, grts_fname_df, by = c('GRTS_ID'), type = "left")

  # Creating map with an Imagery layer
  m = leaflet() %>% addTiles()
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
    content_popup = paste0('<b style = "color:',color_2,';" >GRTS cell</b>',
                          '<br> <div style = "color:', color_2, ';" >', content, '</div>')

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
  m = m %>% addLegend('bottomright',labels = c('Has survey data', 'No survey data'),
    colors = c('#198a00', '#ff0000'), opacity =1)

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
#' @description Builds a grts shapefile from the grts_ids parameter.
#' note: uses rgdal and spatial packages.
#'
#' @param grts_ids Character Vector GRTS Ids
#' @param project_df Dataframe output from get_projects()
#' @param project_id Numeric or String a project id
#' @keywords species, bats, NABat, grts, CONUS
#'
#' @export

get_grts_shp = function(
  grts_ids,
  project_id,
  project_df){

  # Call Build polygons dataframe from GRTS IDs function
  grts_shp_df = get_grts_shp_df(grts_ids = grts_ids,
    project_id = project_id, project_df = project_df)
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
#' @param project_id Numeric or String a project id
#' @param project_df Dataframe output from get_projects()
#'
#' @export

get_grts_shp_df = function(
  grts_ids,
  project_id,
  project_df){

  grts_template_df = data.frame(GRTS_ID = as.integer(grts_ids))
  project_id_ = project_id
  grts_fname = as.character(subset(project_df,
                            project_df$project_id == project_id_)$sample_frame_short)
  grts_fname_df = grts_lookup_df[grts_fname][[1]]
  grts_df = plyr::join(grts_template_df, grts_fname_df, by = c('GRTS_ID'), type = "left")

  polys_df = data.frame()
  for (grts_id in grts_ids){
    this_row = subset(grts_df,grts_df$GRTS_ID == grts_id)
    ll1 = as.numeric(rev(as.character(strsplit(as.character(this_row$lowerleft),',')[[1]])))
    ll2 = as.numeric(rev(as.character(strsplit(as.character(this_row$upperleft),',')[[1]])))
    ll3 = as.numeric(rev(as.character(strsplit(as.character(this_row$upperright),',')[[1]])))
    ll4 = as.numeric(rev(as.character(strsplit(as.character(this_row$lowerright),',')[[1]])))
    lngs = c(ll1[1],ll2[1],ll3[1],ll4[1],ll1[1])
    lats = c(ll1[2],ll2[2],ll3[2],ll4[2],ll1[2])

    if (!is.na(ll1)[1]){
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

get_spdf_from_polys_df = function(
  grts_shp_df){

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


#' @title Get GRTS information from Lat / Lon
#'
#' @description
#' Takes a latitude and longitude in EPSG 4326 (WGS 84)
#' and returns a
#'
#' @param token List token created from get_nabat_gql_token() or
#' get_refresh_token()
#' @param latitude Numeric latitude in EPSG 4326 (WGS 84)
#' @param longitude Numeric latitude in EPSG 4326 (WGS 84)
#' @param branch (optional) String that defaults to 'prod' but can also be
#' 'dev'|'beta'|'local'
#' @param url (optional) String url to use for GQL
#' @param aws_gql (optional) String url to use in aws
#' @param aws_alb (optional) String url to use in aws
#' @param docker (optional) Boolean if being run in docker container or not
#'
#' @export
get_grts_from_ll = function(
  token,
  latitude,
  longitude,
  branch = 'prod',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker = FALSE){

  # Set URL based on branch
  if (is.null(url)) url = get_gql_url(branch)

  # Refresh token
  token = get_refresh_token(token, branch, url, aws_gql, aws_alb, docker)

  # Get headers for token
  tkn_hdr = get_token_headers(token, url, aws_gql, aws_alb, docker)
  headers = tkn_hdr$headers
  token   = tkn_hdr$token

  # Set Query
  query =paste0('
    query RRgrtsSelectionSearchQuery($geometry: JSON!) {
    grtsSelectionSearch(geom: $geometry) {
    nodes {
    grtsId
    grtsCellId
    geom4326 {
    geojson
    }
    location1Name
    subLocation1Name
    sampleFrameId
    priorityState
    priorityFrame
    otherSelections
    effort
    }
    }
    }')

  # Loop through Lat/Lon values
  final_df = data.frame()
  if (length(latitude) == length(longitude)){
    for (pos in 1:length(latitude)){
      lat = latitude[pos]
      lon = longitude[pos]

      # Add Lat/Lon as variables to query API
      pr_variables = paste0('{"geometry":
        {"type":"Point","crs":
        {"type":"name",
        "properties":{"name":"EPSG:4326"}},
        "coordinates":[',paste0(lon, ',' ,lat),']}}')

      # Create body to send to GQL
      pbody = list(query = query, operationName = 'RRgrtsSelectionSearchQuery', variables = pr_variables)
      # Post to nabat GQL
      res      = httr::POST(url, headers, body = pbody, encode='json')
      content   = httr::content(res, as = 'text')
      json = fromJSON(content, flatten = TRUE)
      # Convert GQL output JSON into dataframe
      df   = as.data.frame(json$data$grtsSelectionSearch$nodes, stringsAsFactors = FALSE)
      # Rename headers
      names(df) = tolower(gsub("(?<=[a-z0-9])(?=[A-Z])", "_", names(df), perl = TRUE))

      # Bind data into dataframe
      if (dim(final_df)[1]==0){
        final_df = df
      }else {
        final_df = rbind(final_df, df)
      }
  }
    }else{
      stop('latitude and longitude need to be same lengths.')
    }
  return(final_df)
}
