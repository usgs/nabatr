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
# Created: 2019-9-6
#############################################################################

#' @title Build Report .html file for Acoustic Stationary Project Data
#'
#' @import rmarkdown
#' @import leaflet
#'
#' @description
#' Using the outputs from get_projects(), get_project_surveys(), get_acoustic_bulk_wavs(),
#' and get_observed_nights() this function will create a report .html file to an output_dir.
#' This function can run with these outputs or without.
#' @param token String token created from get_nabat_gql_token function
#' @param output_dir String output directory to save report .html file
#' @param project_id String or Integer project id from NABat ex: 105 or '105'
#' @param file_name (optional) String output file name ex: 'my_report.html'
#' @param project_df (optional) Dataframe from running get_projects()
#' @param survey_df (optional) Dataframe from running get_project_surveys()
#' @param acoustic_bulk_df (optional) Dataframe from running get_acoustic_bulk_wavs()
#' @param manual_nights_df (optional) Dataframe from running get_observed_nights()
#' @param auto_nights_df (optional) Dataframe from running get_observed_nights()
#' @keywords bats, NABat, GQL
#' @examples
#'
#' get_acoustic_stationary_report(username = 'NABat_Username')
#' -- Prompts for password
#
#' @export
#'
get_acoustic_stationary_report = function(token,
                                          output_dir,
                                          project_id,
                                          file_name = NULL,
                                          project_df = NULL,
                                          survey_df = NULL,
                                          acoustic_bulk_df = NULL,
                                          manual_nights_df = NULL,
                                          auto_nights_df = NULL){

  # Specifiy template in data directory
  template = ('data/templates/acoustic_stationary_report.Rmd')

  rmarkdown::render(input = template,
                    output_file = paste0(output_dir, file_name))

}


#' @title Read in GRTS lookup csv for CONUS
#'
#' @description
#' Reads in dataframe that contains all of the GRTS cells and their
#' 4 corners to build shapefiles from.  Projection: WGS84 and extent: CONUS
#' @keywords species, bats, NABat, grts, CONUS
#' @examples
#'
#' nabatr::grts_coords
#
#' @export
#'
grts_coords = read.csv('data/GRTS_coords_CONUS.csv')



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
#' @param project_id String or Integer project id from NABat ex: 105 or '105'
#' @param all_grts String Vector all grts cell ids found from the survey_df dataframe by running
#' unique(survey_df$)
#' @param grts_with_data (optional) String Vector or NULL
#' @keywords bats, NABat, GQL
#' @examples
#'
#' map = get_grts_leaflet_map(project_id     = 283,
#'                            all_grts       = unique(survey_df_$grts_cell_id),
#'                            grts_with_data = unique(auto_nights_df_$GRTS))
#
#' @export
#'
get_grts_leaflet_map = function(project_id, all_grts, grts_with_data = NULL){

  # Create grts_template_df dataframe and merge with grts_coords
  grts_template_df = data.frame(GRTS_ID = all_grts)
  grts_df = plyr::join(grts_template_df, grts_coords, by = c('GRTS_ID'), type = "left")

  polys = SpatialPolygons(list())
  # Creating map with an Imagery layer
  m = leaflet() %>% addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.jpg",
                             attribution = 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community',
                             group = "World Imagery")
  # Loop through all all_grts, create a polygon for each, and add to the leaflet map m
  count = 0
  for (grts_cell in all_grts){

    # Setting color to green or red based on if the grts cell has data or not
    if (is.null(grts_with_data)){
      color_ = '#2fff00'
      color_2 = 'green'
    }else {
      if (grts_cell %in% grts_with_data){
        color_  = '#2fff00'
        color_2 = 'green'
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
    lngs = c(ll1[1],ll2[1],ll3[1],ll4[1])
    lats = c(ll1[2],ll2[2],ll3[2],ll4[2])

    # Add this grts polygon to the leaflet map m
    m = m %>% addPolygons(lat = lats, lng = lngs, popup = content_popup,
                          color = color_, weight = 1.5, opacity = 1, group = color_,
                          label = content,
                          labelOptions = labelOptions(style = list('font-size' = '14px',
                                                                   'color' = color_2,
                                                                   'box-shadow' = '3px 3px rgba(0,0,0,0.25)',
                                                                   'border-color' = 'rgba(0,0,0,0.5)',
                                                                   'border-radius' = '5px',
                                                                   'padding' = '5px 5px 5px 5px'))) # padding order is top, right, bottom, left
  }
  # # Add image to map
  # rr = tags$div(HTML('<img border="0" alt="NABat Website" src="nabat_logo.png" width="150" height="50">'))
  # m = m %>% addControl(rr, position = "bottomleft")

  # Add title to map
  map_title = tags$style(HTML(".leaflet-control.map-title {
                              transform: translate(-50%,15%);
                              position: fixed !important;
                              left: 50%;
                              text-align: center;
                              padding-left: 15px;
                              padding-right: 15px;
                              padding-top: 10px;
                              padding-bottom: 10px;
                              background: rgba(255,255,255,0.5);
                              font-weight: bold;
                              font-size: 20px;
                              text: black;
                              border-radius: 8px;
  }"))

  title = tags$div(map_title, HTML(paste0("Acoustic Stationary GRTS Cells for Project: "),project_id))
  m = m %>% addControl(title, position = "topleft", className="map-title")

  # Return leaflet map m with GRTS cells
  return (m)
  }
