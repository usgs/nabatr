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
# FILE DESCRIPTION:  This file contains functions to help build the Mobile
# Acoustic report build_sa_doc()
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


#' @title Get Stationary Acoustic Species data
#'
#' @import plotly
#'
#' @description Extracts all of the species found within these
#' stationary acoustic wav files (for, manual, automatic, or both detection types)
#' type = 'all' | 'auto' | 'man'
#' format = 'df' | 'vector' | 'vectorNoId' (remove NoID from list)
#'
#' @param sa_bulk_df Dataframe Mobile acoustic bulk data. Can get from
#' get_ma_bulk_wavs() in nabatr
#' @param species_df Dataframe species nabat lookup dataframe
#' @param type String type of data to return for Stationary
#' Acoustic 'auto' | 'man' | 'all' (automatic detection,
#' manual detections, and both)
#' @param format String format of data to return 'df' = dataframe of
#' the species from this data, 'vector' = a string vector of all
#' of the species from this data, 'vectorNoId' = a strin vector of all
#' of the species but exclude 'NoID'
#'
#' \dontrun{
#'
#' proj_species_df = get_sa_species(sa_bulk_df, species_df, 'all','df')
#' proj_species = get_sa_species(sa_bulk_df, species_df, 'all','vectorNoId')
#' man_proj_species  = get_sa_species(sa_bulk_df, species_df, 'man','vectorNoId')
#' auto_proj_species = get_sa_species(sa_bulk_df, species_df, 'auto','vectorNoId')
#'
#'}
#'
#' @export

get_sa_species = function(
  sa_bulk_df,
  species_df,
  type = 'all',
  format = 'df'){

  auto_ids = subset(sa_bulk_df, !is.na(sa_bulk_df$auto_id))$auto_id
  manual_ids = subset(sa_bulk_df, !is.na(sa_bulk_df$manual_id))$manual_id
  auto_species_found = data.frame()
  man_species_found = data.frame()


  if (type == 'auto' | type == 'all'){
    # If Auto data exists
    if (length(unique(sa_bulk_df$auto_id)) > 1 |
        (length(unique(sa_bulk_df$auto_id)) == 1 &
            !is.na(unique(sa_bulk_df$auto_id))[1])){
      # Auto Species
      auto_project_species = (data.frame(id = unique(auto_ids),
        stringsAsFactors = FALSE) %>%
          dplyr::left_join(species_df, by = 'id'))
      auto_species_found =
        subset(auto_project_species, auto_project_species$bat_call) %>%
        dplyr::mutate(detection_type = 'automatic')
      auto_species_detected_wav =
        subset(sa_bulk_df, sa_bulk_df$auto_id %in% auto_species_found$id)
    }
  }

  if (type == 'man' | type == 'all'){
    # If Manual data exists
    if (length(unique(sa_bulk_df$manual_id)) > 1 |
        (length(unique(sa_bulk_df$manual_id)) == 1 &
            !is.na(unique(sa_bulk_df$manual_id))[1])){
      # Manual Species
      man_project_species = (data.frame(id = unique(manual_ids),
        stringsAsFactors = FALSE) %>%
          dplyr::left_join(species_df, by = 'id'))
      man_species_found =
        subset(man_project_species, man_project_species$bat_call) %>%
        dplyr::mutate(detection_type = 'manual')
      man_species_detected_wav =
        subset(sa_bulk_df, sa_bulk_df$manual_id %in% man_species_found$id)
    }
  }

  final_species_df = rbind(auto_species_found, man_species_found)
  final_species = unique(final_species_df$species_code)

  # Return data based on format
  if (format == 'df'){
    return(final_species_df)
  }else if (format == 'vector'){
    return(final_species)
  }else if (format == 'vectorNoId'){
    return(final_species[final_species != 'NoID'])
  }
}



#' @title Get Stationary Acoustic Report Range Maps
#'
#' @description Builds and saves out the species grts map with species range
#' map overlayed and species range map on its own to see full species range.
#' Using a USGS shapefile for species ranges.
#'
#' @param sa_bulk_df Dataframe Mobile acoustic bulk data. Can get from
#' get_ma_bulk_wavs() in nabatr
#' @param project_df Dataframe NABat projects. Can get from
#' get_projects()
#' @param project_id Integer Project Id
#' @param all_species_totals_l_l Dataframe Species totals. Can get from
#' get_all_species_counts_long()
#' @param species_df Dataframe species nabat lookup dataframe
#' @param out_dir String Directory to save the temp directory with
#' all the maps into. Be sure not to end with '/'
#' @param save_bool Boolean Whether to save out maps or not
#' @param species_shp SpatialPolygonsDataFrame Species range maps
#' for NABat. Auto set to global species_range_shps variable
#'
#' @export

get_sa_range_maps = function(
  sa_bulk_df,
  project_df,
  project_id,
  all_species_totals_l_l,
  species_df,
  out_dir,
  save_bool = TRUE,
  species_shp = species_range_shps){

  # # Load in data from data directory and read in as a shpfile
  # species_dir = system.file('data/bat_species_ranges.shp', package = "nabatr")
  # species_shp = rgdal::readOGR(species_dir)[,1:4]

  # Set CRS to WGS
  proj4string(species_shp) =
    CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  proj_species = get_sa_species(sa_bulk_df, species_df, 'all','vectorNoId')
  proj_grts = unique(sa_bulk_df$grts_cell_id)

  # Build species dataframe to merge with NABat's species lookup table
  species_shp_names_df =
    data.frame(species = as.character(unique(species_shp$SCI_NAME)),
    value = rep(1, length(unique(species_shp$SCI_NAME))),
      stringsAsFactors = FALSE)

  # Merge the two together (key = species) Note: Myotis
  ## melanorhinus exists in shapefile but
  ## not in species_df from nabat database
  species_range_df = species_df %>%
    dplyr::left_join(species_shp_names_df, by = c('species'='species')) %>%
    subset(value == 1) %>%
    dplyr::select(-value)

  no_species_range = c()
  species_with_range = c()
  maps_grts_files = c()
  maps_range_files = c()
  # If the species exists in the ranges shapefile than
  ## build it otherwise add it to a no_species_range vector
  for (spc in proj_species){
    if (spc %in% species_range_df$species_code){
      message(paste0('Creating species range maps for: ', spc))
      # Grab species range
      spc_row = subset(species_range_df, species_range_df$species_code == spc)
      spc_shp = subset(species_shp, species_shp$SCI_NAME == spc_row$species)

      # Grab GRTS from data -- Both Auto and Manual species
      spc_spec_totals_df = subset(all_species_totals_l_l,
        all_species_totals_l_l[spc] > 0)
      spc_spec_totals_df_aut = subset(all_species_totals_l_l,
        all_species_totals_l_l[spc] > 0 & all_species_totals_l_l$type == 'auto')
      spc_spec_totals_df_man = subset(all_species_totals_l_l,
        all_species_totals_l_l[spc] > 0 & all_species_totals_l_l$type == 'manual')

      man_grts = spc_spec_totals_df_man$GRTS
      aut_grts = spc_spec_totals_df_aut$GRTS

      # Get GRTS with species Man/Auto/Both
      both_grts = intersect(man_grts, aut_grts)
      man_only_grts = setdiff(man_grts, both_grts)
      aut_only_grts = setdiff(aut_grts, both_grts)
      # Get GRTS without species
      grts_with_spc      = unique(spc_spec_totals_df$GRTS)
      grts_without_spc   = setdiff(proj_grts, grts_with_spc)

      # Grab coordinates for the GRTS with data
      man_grts_with_spc_spdf = get_grts_shp(grts_ids = man_only_grts,
        project_id = project_id,
        project_df = project_df)
      aut_grts_with_spc_spdf = get_grts_shp(grts_ids = aut_only_grts,
        project_id = project_id,
        project_df = project_df)
      both_grts_with_spc_spdf = get_grts_shp(grts_ids = both_grts,
        project_id = project_id,
        project_df = project_df)
      grts_without_spc_spdf = get_grts_shp(grts_ids = grts_without_spc,
        project_id = project_id,
        project_df = project_df)

      all_grts_spdf = rbind(man_grts_with_spc_spdf, aut_grts_with_spc_spdf,
        both_grts_with_spc_spdf, grts_without_spc_spdf)
      full_extent = extent(all_grts_spdf)

      # Build the grts map overlayed by this species range
      #c ('#ff8400', '#337acc', '#23992f')) %>% # orange/blue/green
      m = leaflet() %>% addTiles() %>% addPolygons(data = spc_shp,
        label = spc, group = 'species_range')
      if (length(man_grts_with_spc_spdf) > 0){
        extent = extent(man_grts_with_spc_spdf)
        lng_ = extent@xmin + ((extent@xmax - extent@xmin)/2)
        lat_ = extent@ymin + ((extent@ymax - extent@ymin)/2)
        m = m %>% addPolygons(data = man_grts_with_spc_spdf, color = 'black',
          fillOpacity = 1, fillColor = '#23992f', weight=1, opacity=1)
      }
      if (length(aut_grts_with_spc_spdf) > 0){
        extent = extent(aut_grts_with_spc_spdf)
        lng_ = extent@xmin + ((extent@xmax - extent@xmin)/2)
        lat_ = extent@ymin + ((extent@ymax - extent@ymin)/2)
        m = m %>% addPolygons(data = aut_grts_with_spc_spdf, color = 'black',
          fillOpacity = 1, fillColor = '#337acc', weight=1, opacity=1)
      }
      if (length(both_grts_with_spc_spdf) > 0){
        extent = extent(both_grts_with_spc_spdf)
        lng_ = extent@xmin + ((extent@xmax - extent@xmin)/2)
        lat_ = extent@ymin + ((extent@ymax - extent@ymin)/2)
        m = m %>% addPolygons(data = both_grts_with_spc_spdf, color = 'black',
          fillOpacity = 1, fillColor = '#ff8400', weight=1, opacity=1)
      }
      if(length(grts_without_spc_spdf) > 0){
        extent = extent(grts_without_spc_spdf)
        lng_ = extent@xmin + ((extent@xmax - extent@xmin)/2)
        lat_ = extent@ymin + ((extent@ymax - extent@ymin)/2)
        m = m %>% addPolygons(data = grts_without_spc_spdf, color = 'black',
          fillOpacity = 0, fillColor = 'rgb(0,0,0,0)', weight=1, opacity=1)
      }

      m = m %>% fitBounds(full_extent@xmin, full_extent@ymin,
        full_extent@xmax, full_extent@ymax) %>%
        addMiniMap(toggleDisplay = F,
          zoomLevelFixed = 2,
          minimized = FALSE
        ) %>%
        htmlwidgets::onRender("
          function(el, t) {
          var myMap = this;

          var range = myMap.layerManager._byGroup.species_range;
          console.log('test', myMap.layerManager)
          console.log(range)
          console.log('3',range._latlngs);
          var range2 = new L.FeatureGroup();
          Object.keys(range).forEach(k => {
          if (range[k]._latlngs) {
          range[k]._latlngs.forEach(f => {
          var coords = [];
          f.forEach(c => {
          coords.push([c.lat, c.lng])
          })
          //range2.push(new L.Polygon(coords))
          range2.addLayer(new L.Polygon(coords))
          })
          console.log(range[k]._latlngs)
          }
          });
          console.log(range2);
          myMap.minimap.changeLayer(new L.LayerGroup([L.tileLayer.provider('Esri.NatGeoWorldMap'), range2]));
          }") %>%
        addLegend('topright', labels = c(paste0(spc, ' Automatic & Manual'),
          paste0(spc, ' Automatic Only'),
          paste0(spc, ' Manual Only'), paste0(spc, ' Not Found')),
          colors = c('#ff8400', '#337acc', '#23992f', 'rgb(0,0,0,0)'), opacity =1)

      # zoom_pt = rgeos::gCentroid(spc_shp)
      range_extent = extent(spc_shp)
      # Build species range map for this species
      # website for diff providers: http://leaflet-extras.github.io/leaflet-providers/preview/
      m_range = leaflet() %>% addTiles() %>%
        addPolygons(data = spc_shp, label = spc, group = 'species_range') %>%
        # setView(lng = zoom_pt@coords[,1], lat = zoom_pt@coords[,2], zoom = 3) %>%
        addLegend('topright',labels = paste0(spc, ' Species Range'),
          colors = c('blue'), opacity =1) %>%
        fitBounds(range_extent@xmin, range_extent@ymin,
          range_extent@xmax, range_extent@ymax)

      # Save out the two maps
      out_maps_dir = paste0(out_dir, '/temps/range_maps/')
      map_out_ = paste0(out_maps_dir, spc, '_grts.png')
      range_map_out_ = paste0(out_maps_dir, spc, '_range.png')

      if (save_bool){
        message (paste0('Saving out map for: ', spc))
        mapshot(m, file = map_out_,
          remove_controls = c("zoomControl", "layersControl", "homeButton"))
        mapshot(m_range, file = range_map_out_,
          remove_controls = c("zoomControl", "layersControl", "homeButton"))
      }

      # Species with range maps
      maps_grts_files = c(maps_grts_files, map_out_)
      maps_range_files = c(maps_range_files, range_map_out_)
      species_with_range = c(spc, species_with_range)
    }else {
      # No species range maps found for these species
      no_species_range = c(spc, no_species_range)
    }
  }
  return(list(maps_grts_files = maps_grts_files,
    maps_range_files = maps_range_files,
    species_with_range = species_with_range,
    no_species_range = no_species_range))
}


#' @title Get Stationary Acoustic Report examples
#'
#' @description Returns al ist of some preset stationary acoustic
#' report examples
#'
#' @export

get_sa_examples = function(){
  # description  = project_row_df$project_description
  sa_ex_description = "[EXAMPLE]: PURPOSE: Bat occupancy and abundance data in Colorado
  is less comprehensive than most other groups of mammals in the state. The purpose of
  a Bureau of Land Management-Royal Gorge Field Office (RGFO) acoustic monitoring project
  is to produce a statistically sound dataset that will provide a baseline for bat occupancy
  monitoring that may be utilized as a decision making tool for effective conservation.
  In addition, the project will participate in the continent-wide effort to create a
  coordinated bat population monitoring program at a regional and range wide scale that
  is able to provide inferences regarding changes in distribution and abundance of bat
  populations. OBJECTIVES: Establish a long-term monitoring program for bats across the
  RGFO; Utilize a monitoring protocol that will determine a baseline occupancy of bat species
  across the RGFO and may be used as an index to determine changes of occupancy in the future;
  Incorporate local data into the continent wide NABat dataset."
  # Set Methods in 3 sections
  sa_methods_1 = "Survey units were selected using the NABat master sampling frame, a grid-based
  system consisting of 10 x 10 km (100 km2) cells spanning Canada, the United States, and Mexico.
  The NABat master sample frame provides an ordered list of cells that is spatially balanced
  and randomized by utilizing the generalized random-tessellation stratified (GRTS) survey
  design algorithm. Using NABat's online cell selection tool, a subset of the master sampling
  frame was selected by defining the overall geographic scope of this project. Individual
  cells were then selected for survey based on their GRTS order and available resources. These
  100 km2 cells serve as the focal analytical unit for NABat analyses and are a biologically
  appropriate grain size given the nightly range of most bat species (Loeb et al. 2015). "
  sa_methods_2 = "Recording devices capable of detecting high-frequency bat echolocation calls
  were deployed at 2-4 sites within each GRTS cell selected for survey. Sites were chosen based
  on guidance provided in Loeb et al. (2015). Factors considered when selecting sites included
  land ownership, accessibility, minimizing clutter, elevation, and heterogeneity of habitats
  within the cell. Recording devices were deployed for 4 consecutive nights during the summer
  maternity season when bats are most active and most likely to be detected if present in the
  cell. Detectors were programmed to record automatically beginning 15 minutes prior to sunset
  and ending 15 minutes after sunrise. Microphones were elevated ~ 3 m from the ground and
  oriented in the direction of least clutter to maximize detection (Loeb et al. 2015). "
  sa_methods_3 = "Calls files were processed using commercially-available automated identification
  software. Prior to species identification, non-bat files were scrubbed using a noise filter.
  Next, the remaining files were identified to species using a regional classifier that only
  considers the species whose ranges intersect the defined region. Calls that could not be
  identified to species were labeled either NO ID or with a general category (LowF, HighF, 25k,
  40k, etc.). Due to overlap in the characteristics of some bat species' calls and the
  uncertainty associated with automated ID software, a subset of calls was manually vetted in
  accordance with Loeb et al. (2015). All call files identified as rare species were manually
  vetted, as were all calls from species not known to occur in the survey area. For non-rare
  species known to occur in the survey area, at least one call was manually vetted per point
  per night to confirm species presence within the survey cell and to estimate detection
  probability."
  # Set Summary in 2 sections
  sa_summary_1 = "[EXAMPLE]: Survey results will be reported to relevant state biologists,
  USFWS Region 4, and NABat. In 2019, survey efforts were expanded to include 15 new cells
  and collaborative efforts with Colorado Parks and Wildlife, USFWS, and Bat Conservation
  International.  "
  sa_summary_2 = "No statistically significant changes in species richness were detected between
  2018 and 2019, however, there was a significant decrease in overall activity rate between
  the two years. Moving forward, these data will help land managers determine priority areas
  for bat mitigation efforts and provide baseline data to examine habitat associations that
  may be important for protecting species of federal and state conservation concern. "

  # Lit Cited
  sa_lit_cited = "Loeb, S.C., T.J. Rodhouse, L.E. Ellison, C.L. Lausen, J.D. Reichard, K.M.
  Irvine, T.E. Ingersoll, J.T.H. Coleman, W.E. Thogmartin, J.R. Sauer, C.M. Francis, M.L.
  Bayless, T.R. Stanley, and D.H. Johnson. 2015. A plan for the North American Bat Monitoring
  Program (NABat). General Technical Reports SRS-208. Asheville, NC: U.S. Department of Agriculture
  Forest Service, Southern Research Station. 112 p."

  return(list(sa_ex_description = sa_ex_description,
    sa_methods_1 = sa_methods_1,
    sa_methods_2 = sa_methods_2,
    sa_methods_3 = sa_methods_3,
    sa_summary_1 = sa_summary_1,
    sa_summary_2 = sa_summary_2,
    sa_lit_cited = sa_lit_cited))
}




#' @title Get Stationary Acoustic Report Results
#'
#' @description Builds out text with results
#'
#' @param sa_bulk_df Dataframe Mobile acoustic bulk data. Can get from
#' get_ma_bulk_wavs() in nabatr
#' @param selected_year Integer Report year to use
#' @param species_df Dataframe species nabat lookup dataframe
#'
#' @export

get_sa_results = function(
  sa_bulk_df,
  selected_year,
  species_df){

  # Number of sites and grts cells
  number_of_sites = length(unique(sa_bulk_df$site_name))
  number_of_cells = length(unique(sa_bulk_df$grts_cell_id))
  proj_species_df = get_sa_species(sa_bulk_df, species_df, 'all','df')
  # Total number of bat calls (all recording wav files counted)
  # Total number of detector nights across all sites
  total_nights_df = sa_bulk_df %>% dplyr::select(survey_night_start, survey_night_end) %>%
    dplyr::distinct() %>% dplyr::rowwise() %>%
    dplyr::mutate(total_nights =
        as.integer(as.Date(survey_night_end) - as.Date(survey_night_start)) + 1)
  number_of_net_nights = sum(total_nights_df$total_nights)

  proj_species = get_sa_species(sa_bulk_df, species_df, 'all','vector')
  number_of_species_detected = length(proj_species)

  # Subset the stationary acoustic dataframe with only
  proj_species_ids = unique(proj_species_df$id)
  sa_bulk_df_bats_only = subset(sa_bulk_df, sa_bulk_df$auto_id %in% proj_species_ids |
      sa_bulk_df$manual_id %in% proj_species_ids)
  # Get the number of bat calls across project (excludes NOISE)
  number_of_bat_calls = length(unique(sa_bulk_df_bats_only$audio_recording_name))
  # Calculate some min, max, median, and averages across sites
  low_avg_per_night = min(plyr::count(sa_bulk_df_bats_only, 'site_name')$freq)
  high_avg_per_night = max(plyr::count(sa_bulk_df_bats_only, 'site_name')$freq)
  median_activity_rate = median(plyr::count(sa_bulk_df_bats_only, 'site_name')$freq)
  mean_activity_rate = round(mean(plyr::count(sa_bulk_df_bats_only, 'site_name')$freq),0)

  # Text for Results using Project Summary Data
  results_overview = paste0("A total of ", number_of_sites," sites in ", number_of_cells,
    " NABat GRTS cells were surveyed in ", selected_year," (Figure 1, Table 1). ",
    number_of_bat_calls," call files were recorded over ", number_of_net_nights,
    " detector nights, and ", number_of_species_detected,
    " species were detected (Figure 1, Table 2). Activity rate (average bat passes per night) ranged from ",
    low_avg_per_night," to ", high_avg_per_night,", with a median of ",
    median_activity_rate," and a mean of ", mean_activity_rate," (Figures 3, 4).")
  return(results_overview)
}




#' @title Build stationary acoustic table 1 for report
#'
#' @description Returns a table with GRTS, Species_Detected, and Method_of_Species_ID
#'
#' @param sa_bulk_df Dataframe Mobile acoustic bulk data. Can get from
#' get_ma_bulk_wavs() in nabatr
#' @param project_id Integer Project Id
#' @param project_df Dataframe NABat projects. Can get from
#' get_projects()
#' @param species_df Dataframe species nabat lookup dataframe
#' @param selected_year Integer Report year to use
#'
#' @export

build_sa_table_1 = function(
  sa_bulk_df,
  project_id,
  project_df,
  species_df,
  selected_year){

  # Create Table Description
  sa_descr_table_1 = paste0("Table 1. NABat GRTS cells surveyed in ",
    selected_year,
    ". Number of detector points, detector nights, and species detected are shown for each cell.")

  project_id_ = project_id
  grts_fname = as.character(subset(project_df,
    project_df$project_id == project_id_)$sample_frame_short)
  # Get grts_fname_df
  grts_fname_df = grts_lookup_df[grts_fname][[1]]

  all_grts = unique(sa_bulk_df$grts_cell_id)


  # Build Dataframe with grts and their center points
  grts_df = data.frame(GRTS_Cell = all_grts) %>%
    dplyr::left_join(grts_fname_df, by = c('GRTS_Cell'='GRTS_ID')) %>%
    dplyr::select(GRTS_Cell, center) %>% rowwise() %>%
    dplyr::rename('GRTS' = 'GRTS_Cell') %>%
    dplyr::mutate(y = as.numeric(strsplit(center, split=',')[[1]][1])) %>%
    dplyr::mutate(x = as.numeric(strsplit(center, split=',')[[1]][2]))

  # Calculate Detector nights based on survey_start and survey_end times for each Site
  all_grts_rows = sa_bulk_df %>% dplyr::group_by(grts_cell_id) %>%
    dplyr::select(grts_cell_id, site_name, survey_night_start, survey_night_end) %>%
    dplyr::distinct() %>%
    dplyr::mutate(Detector_Points = length(unique(site_name))) %>%
    dplyr::mutate(Site_Detector_Nights =
        as.integer(as.Date(survey_night_end) - as.Date(survey_night_start)) + 1) %>%
    dplyr::mutate(Detector_Nights = sum(Site_Detector_Nights)) %>%
    dplyr::select(grts_cell_id, Detector_Points, Detector_Nights) %>%
    dplyr::distinct() %>%
    dplyr::rename('GRTS' = grts_cell_id)

  # Calculate Unique species found at each GRTS
  species_counts = c()
  all_grts = unique(sa_bulk_df$grts_cell_id)
  for (grts in all_grts){
    grts_bulk_df = subset(sa_bulk_df, sa_bulk_df$grts_cell_id == grts)
    species_count = length(get_sa_species(grts_bulk_df, species_df, 'all','vector'))
    species_counts = c(species_counts, species_count)
  }
  all_grts_rows_add = data.frame(GRTS = all_grts, Species_Detected = species_counts)
  all_grts_df = all_grts_rows %>% dplyr::left_join(all_grts_rows_add, by='GRTS')

  # If in CONUS add State and County.  Otherwise exclude
  if (grts_fname == 'Continental US'){
    state_county = ll_to_county_state(dplyr::select(grts_df, x, y))
    grts_df['state_county'] = state_county

    grts_df_final = grts_df %>% rowwise() %>%
      dplyr::mutate(State = strsplit(state_county,',')[[1]][1]) %>%
      dplyr::mutate(State = if_else(is.na(State), 'Not Found', State)) %>%
      dplyr::mutate(County = .simpleCap(strsplit(state_county,',')[[1]][2])) %>%
      dplyr::mutate(County = if_else(County == 'NANA', 'Not Found', County)) %>%
      dplyr::select(-state_county, -x, -y, -center) %>%
      dplyr::left_join(all_grts_df, by = 'GRTS') %>%
      dplyr::arrange(State, County, Species_Detected)
  }else{
    grts_df_final = grts_df %>%
      dplyr::select(-x, -y, -center) %>%
      dplyr::left_join(all_grts_df, by = 'GRTS')
  }

  sa_ft1_names_list = list()
  for (name in names(grts_df_final)){
    sa_ft1_names_list[name] = gsub("_", " ", name)
  }

  sa_ft1 = flextable::flextable(grts_df_final)
  sa_ft1 = flextable::set_header_labels(sa_ft1, values = sa_ft1_names_list)
  sa_ft1 = flextable::height(sa_ft1, height =.7, part = 'header')
  sa_ft1 = flextable::width(sa_ft1, width = 1)
  sa_ft1 = flextable::fontsize(sa_ft1, size = 10, part = "all")

  return (list(table = sa_ft1, description = sa_descr_table_1))
}



#' @title Build stationary acoustic table 3 for report
#'
#' @description Returns a table with GRTS, Species_Detected,
#' and Method_of_Species_ID
#'
#' @param sa_bulk_df Dataframe Mobile acoustic bulk data. Can get from
#' get_ma_bulk_wavs() in nabatr
#' @param selected_year Integer Report year to use
#' @param species_df Dataframe species nabat lookup dataframe
#'
#' @export

build_sa_table_3 = function(
  sa_bulk_df,
  selected_year,
  species_df){

  # Create Table Description
  sa_descr_table_3 = paste0("Table 2. Bat species detected in each NABat GRTS cell surveyed, ",
    selected_year,
    ". Years with detections and method of species identification are shown for each species in each cell. ")

  all_grts = unique(sa_bulk_df$grts_cell_id)
  table_3_df = data.frame()
  for (grts in all_grts){
    grts_3_df= data.frame()
    # Get stationary acoustic bulk row data for this GRTS cell
    grts_sa_bulk_row = subset(sa_bulk_df, sa_bulk_df$grts_cell_id == grts)
    # Get species information for this grts
    proj_species_df = get_sa_species(grts_sa_bulk_row, species_df, 'all','df')
    proj_species = get_sa_species(grts_sa_bulk_row, species_df, 'all','vector')
    man_proj_species  = get_sa_species(grts_sa_bulk_row, species_df, 'man','vector')
    auto_proj_species = get_sa_species(grts_sa_bulk_row, species_df, 'auto','vector')
    # Find species at this grts for all, man, and auto types
    all_species_names = unique(subset(proj_species_df,
      proj_species_df$species_code %in% proj_species)$species)
    man_species_names = unique(subset(proj_species_df,
      proj_species_df$species_code %in% man_proj_species)$species)
    auto_species_names = unique(subset(proj_species_df,
      proj_species_df$species_code %in% auto_proj_species)$species)

    methods = c()
    for (species in all_species_names){
      if (species %in% man_species_names & species %in% auto_species_names){
        method = 'Auto, Manual'
      }else if(species %in% man_species_names){
        method = 'Manual'
      }else if(species %in% auto_species_names){
        method = 'Auto'
      }
      methods = c(methods, method)
    }
    grts_3_df = data.frame('GRTS' = rep(grts, length(all_species_names)),
      stringsAsFactors = FALSE) %>%
      dplyr::mutate(Species_Detected = all_species_names) %>%
      dplyr::mutate(Method_of_Species_ID = methods)

    table_3_df = rbind(grts_3_df, table_3_df)

  }

  sa_table_3 = table_3_df %>%
    dplyr::arrange(GRTS, Species_Detected)

  sa_ft3_names_list = list()
  for (name in names(sa_table_3)){
    sa_ft3_names_list[name] = gsub("_", " ", name)
  }
  sa_ft3 = flextable::flextable(sa_table_3)
  sa_ft3 = flextable::set_header_labels(sa_ft3, values = sa_ft3_names_list)
  sa_ft3 = flextable::height(sa_ft3, height =.5, part = 'header')
  sa_ft3 = flextable::width(sa_ft3, width =2)
  sa_ft3 = flextable::merge_v(sa_ft3, j = 'GRTS')
  sa_ft3 = flextable::fontsize(sa_ft3, size = 10, part = "all")
  sa_ft3 = flextable::italic(sa_ft3, j = 2)
  sa_ft3 = flextable::hline(sa_ft3, border = fp_border(color = "black"), part = "body")

  return (list(table = sa_ft3, description = sa_descr_table_3, df = sa_table_3))
}


#' @title Build stationary acoustic figure 1 for report
#'
#' @description Returns leaflet map
#'
#' @export

build_sa_figure_1 = function(
  sa_bulk_df,
  out_dir,
  project_df,
  project_id,
  survey_df,
  selected_year,
  save_bool = FALSE){

  # Create figure description
  sa_descr_fig1  = paste0("Figure 1. Map of all NABat GRTS cells surveyed in ",
    selected_year
    ," and detector points in each surveyed cell. ")
  sa_figure_1 =
    get_grts_leaflet_map(all_grts = unique(subset(survey_df,
      survey_df$year == selected_year)$grts_cell_id),
                        grts_with_data = unique(sa_bulk_df$grts_cell_id),
                        project_df = project_df,
                        project_id = project_id)

  # Save out map
  if (save_bool){
    map_out_ = paste0(out_dir, '/temps/intermediate_map.png')
    mapshot(sa_figure_1, file = map_out_)
  }else {
    map_out_ = NULL
  }
  return(list(figure = sa_figure_1, description = sa_descr_fig1, file = map_out_))
}



#' @title Build stationary acoustic figure 2 for report
#'
#' @description Returns 2 plots and 2 descriptions
#'
#' @param sa_bulk_df Dataframe Mobile acoustic bulk data. Can get from
#' get_ma_bulk_wavs() in nabatr
#' @param out_dir String Directory to save the temp directory with
#' @param species_df Dataframe species nabat lookup dataframe
#' all the maps into. Be sure not to end with '/'
#' @param selected_year Integer Report year to use
#' @param auto_species_grts_df_w Dataframe Species counts wide for
#' automatic species. Can get from get_species_counts_wide()
#' @param manual_species_grts_df_w Dataframe Species counts wide for
#' manual species. Can get from get_species_counts_wide()
#' @param save_bool Boolean Whether to save out maps or not
#'
#' @export

build_sa_figure_2 = function(
  sa_bulk_df,
  out_dir,
  species_df,
  selected_year,
  auto_species_grts_df_w,
  manual_species_grts_df_w,
  save_bool = FALSE){

  # Create figure descriptions
  sa_descr_fig2a = paste0("Figure 2a. ",
    selected_year,
    " bat activity rate (average number of bat passes per night) by species. Species with
    at least one manual identification per site are shown in blue. Species identified only
    by automated identification software are shown in orange and species identified only by
    manual identification software are shown in green.")
  sa_descr_fig2b = paste0("Figure 2b. ",
    selected_year,
    " bat activity rate (average number of bat passes per night using a logarithmic scale)
    by species. Species with at least one manual identification per site are shown in blue.
    Species identified only by automated identification software are shown in orange and
    species identified only by manual identification software are shown in green.")

  # Get all bat species
  proj_species_df = get_sa_species(sa_bulk_df, species_df, 'all','df')
  proj_species = get_sa_species(sa_bulk_df, species_df, 'all','vector')
  man_proj_species  = get_sa_species(sa_bulk_df, species_df, 'man','vector')
  auto_proj_species = get_sa_species(sa_bulk_df, species_df, 'auto','vector')


  all_grts = unique(sa_bulk_df$grts_cell_id)
  table_3_df = data.frame()
  for (grts in all_grts){
    grts_3_df= data.frame()
    # Get stationary acoustic bulk row data for this GRTS cell
    grts_sa_bulk_row = subset(sa_bulk_df, sa_bulk_df$grts_cell_id == grts)
    # Get species information for this grts
    proj_species_df = get_sa_species(grts_sa_bulk_row, species_df, 'all','df')
    proj_species = get_sa_species(grts_sa_bulk_row, species_df, 'all','vector')
    man_proj_species  = get_sa_species(grts_sa_bulk_row, species_df, 'man','vector')
    auto_proj_species = get_sa_species(grts_sa_bulk_row, species_df, 'auto','vector')
    # Find species at this grts for all, man, and auto types
    all_species_names = unique(subset(proj_species_df,
      proj_species_df$species_code %in% proj_species)$species_code)
    all_species_names = unique(subset(proj_species_df,
      proj_species_df$species_code %in% proj_species)$species_code)
    man_species_names = unique(subset(proj_species_df,
      proj_species_df$species_code %in% man_proj_species)$species_code)
    auto_species_names = unique(subset(proj_species_df,
      proj_species_df$species_code %in% auto_proj_species)$species_code)

    methods = c()
    for (species in all_species_names){
      if (species %in% man_species_names & species %in% auto_species_names){
        method = 'Auto, Manual'
      }else if(species %in% man_species_names){
        method = 'Manual'
      }else if(species %in% auto_species_names){
        method = 'Auto'
      }
      methods = c(methods, method)
    }
    grts_3_df = data.frame('GRTS' = rep(grts, length(all_species_names)),
      stringsAsFactors = FALSE) %>%
      dplyr::mutate(Species_Detected = all_species_names) %>%
      dplyr::mutate(Method_of_Species_ID = methods) %>%
      dplyr::mutate()

    table_3_df = rbind(grts_3_df, table_3_df)
  }

  all_bat_id_types = data.frame()
  for (bat_spc in proj_species){
    types = unique(subset(table_3_df,
      table_3_df$Species_Detected == subset(species_df,
        species_df$species_code == bat_spc)$species_code)$Method_of_Species_ID)

    if ('Auto, Manual' %in% types){
      this_type = 'At least one manual ID/site'
    }else if('Auto' %in% types & 'Manual' %in% types){
      this_type = 'At least one manual ID/site'
    }else if('Manual' %in% types){
      this_type = 'Manual ID only'
    }else if('Auto' %in% types){
      this_type = 'Auto ID only'
    }else {
      this_type = 'Not a Species'
    }

    if (this_type == 'At least one manual ID/site'){
      species_auto_count = subset(auto_species_grts_df_w,
        auto_species_grts_df_w$names == bat_spc)$species_totals
    }else if (this_type == 'Auto ID only'){
      species_auto_count = subset(auto_species_grts_df_w,
        auto_species_grts_df_w$names == bat_spc)$species_totals
    }else if (this_type == 'Manual ID only'){
      if (!is.null(manual_species_grts_df_w)){
        species_auto_count = subset(manual_species_grts_df_w,
          manual_species_grts_df_w$names == bat_spc)$species_totals
      }else {
        species_auto_count = 0
      }
    }else if(this_type == 'Not a Species'){
      species_auto_count = subset(auto_species_grts_df_w,
        auto_species_grts_df_w$names == bat_spc)$species_totals
    }

    bat_id_type_row = data.frame(species = bat_spc,
      bat_types = this_type,
      auto_count = species_auto_count,
      stringsAsFactors = FALSE)
    all_bat_id_types = rbind(all_bat_id_types, bat_id_type_row)
  }


  bat_id_type = all_bat_id_types$bat_types
  bat_id_color_df = data.frame(colors = c('#ff8400','#337acc', '#23992f'),
    types = c('At least one manual ID/site', 'Auto ID only', 'Manual ID only'),
    stringsAsFactors = FALSE)
  bat_id_colors = subset(bat_id_color_df, bat_id_color_df$types %in% bat_id_type)$colors
  bat_auto_counts = all_bat_id_types$auto_count / length(unique(sa_bulk_df$grts_cell_id))
  bat_species = all_bat_id_types$species

  # Setting aesthetics for words in plot
  l = list(family = "cambria", size = 22, color = "#6b6b6b")
  x_ = list(title = "Bat Species", titlefont = l)
  x_log = list(title = "Bat Species")
  y_ = list(title = "Average No. of Bat Passes",titlefont = l)
  y_log = list(title = "Average No. of Bat Passes(Log Scale)",titlefont = l, type = 'log')
  leg = list(family = "cambria", size = 16, color = "#6b6b6b")
  # Setting the margin for these plots
  m_fig_2     = list(t = 50, b = 30, l = 30, r = 15, pad = 0)
  m_fig_2_log = list(t = 50, b = 40, l = 45, r = 15, pad = 0)

  # fig 2a
  sa_fig2_p = plot_ly(x = bat_species, y = as.integer(bat_auto_counts), type = 'bar',
    # width = 850, height = 650,
    marker = list(line = list(color = 'black', width = .5)),
    color = bat_id_type, colors = bat_id_colors) %>%
    layout(xaxis = x_, yaxis = y_,
      margin = m_fig_2,
      title = 'Average Bat Activity Rate',
      font = leg, showlegend = TRUE, autosize=TRUE, bargap = .6,
      legend = list(x = .2, y = 1.05, orientation = 'h', font = leg))
  # fig 2b
  sa_fig2_p_log = plot_ly(x = bat_species, y = as.integer(bat_auto_counts), type = 'bar',
    # width = 850, height = 650,
    marker = list(line = list(color = 'black', width = .5)),
    color = bat_id_type, colors = bat_id_colors) %>%
    layout(xaxis = x_log, yaxis = y_log,
      margin = m_fig_2_log,
      title = 'Average Bat Activity Rate using a Logarithmic Scale',
      font = leg, showlegend = TRUE, autosize=TRUE, bargap = .6,
      legend = list(x = .2, y = 1.05, orientation = 'h', font = leg))

  if (save_bool){
    fig2a_f = paste0(out_dir, "/temps/fig2a.png")
    fig2b_f = paste0(out_dir, "/temps/fig2b.png")
    plotly::export(sa_fig2_p, file = fig2a_f)
    plotly::export(sa_fig2_p_log, file = fig2b_f)
  }else{
    fig2a_f = NULL
    fig2b_f = NULL
  }

  return (list(figure_a = sa_fig2_p, figure_b = sa_fig2_p_log,
               description_a = sa_descr_fig2a,
               description_b = sa_descr_fig2b,
               file_a = fig2a_f,
               file_b = fig2b_f))
}




#' @title Build stationary acoustic figure 4 for report
#'
#' @description Returns 1 plot with grts and the number of bat
#' passes they had
#'
#' @export

build_sa_figure_4 = function(
  sa_bulk_df,
  out_dir,
  species_df,
  selected_year,
  save_bool = FALSE){

  # Build description
  sa_descr_fig4 = paste0("Figure 4. ",selected_year,
    " bat activity rate (average number of bat passes per night) by NABat GRTS cell")
  # Get fig data
  proj_species_df = get_sa_species(sa_bulk_df, species_df, 'all','df')
  proj_species_w_noid = get_sa_species(sa_bulk_df, species_df, 'all','vector')
  proj_species_ids = unique(subset(proj_species_df,
    proj_species_df$species_code %in% proj_species_w_noid)$id)

  species_only_df = subset(sa_bulk_df,
    sa_bulk_df$manual_id %in% proj_species_ids | sa_bulk_df$auto_id %in% proj_species_ids)
  proj_grts = unique(species_only_df$grts_cell_id)

  fig4_data = as.data.frame(table(species_only_df$grts_cell_id),
    stringsAsFactors = FALSE) %>%
    dplyr::rename('GRTS' = Var1, 'values' = Freq)

  # Fig styles
  l = list(family = "cambria", size = 22, color = "#6b6b6b")
  leg = list(family = "cambria", size = 16, color = "#6b6b6b")
  x_ = list(title = "NABat GRTS Cell", titlefont = leg)
  y_ = list(title = "Average No. of Bat Passes",titlefont = l)
  m_fig_4 = list(t = 50, b = 20, l = 20, r = 10, pad = 0)

  # Build Fig
  sa_fig4_p = plot_ly(x = fig4_data$GRTS, y = fig4_data$values, type = 'bar',
    # width = 850, height = 650,
    marker = list(line = list(color = 'black', width = .5)),
    color = '#337acc', colors = c('#337acc')) %>%
    layout(margin = m_fig_4, font = leg, xaxis = x_, yaxis = y_,
      showlegend = F, autosize= T, bargap = .6,
      title = 'Average Bat Calls at each GRTS',
      legend = list(x = .2, y = 1.05, orientation = 'h', font = leg))
  if(save_bool){
    sa_fig4_f = paste0(out_dir, "/temps/fig4.png")
    plotly::export(sa_fig4_p, file = sa_fig4_f)
  }else{
    sa_fig4_f = NULL
  }

  return(list(figure = sa_fig4_p, description = sa_descr_fig4, file = sa_fig4_f))
}



#' @title Find if sa_bulk_df has manual or auto data present
#'
#' @description Returns 1 plot with grts and the number of bat
#' passes they had
#'
#' @export

does_sa_type_exist = function(
  sa_bulk_df,
  species_df,
  type){

  proj_species_df = get_sa_species(sa_bulk_df, species_df, 'all','df')
  detection_types = unique(proj_species_df$detection_type)
  # Set whether the stationary acoustic data has automatic or manual data in it
  if (type == 'automatic' | type == 'auto'){
    auto_data_bool = FALSE
    if ('automatic' %in% detection_types){
      auto_data_bool = TRUE
    }
    return(auto_data_bool)
  }
  if (type == 'manual' | type == 'man'){
    man_data_bool = FALSE
    if ('manual' %in% detection_types){
      man_data_bool = TRUE
    }
    return(man_data_bool)
  }
}





