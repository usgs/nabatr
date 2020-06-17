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
# Created: 2020-2-15
#############################################################################


#' @title Get Mobile Acoustic Report Results
#'
#' @description Builds out text with results
#'
#' @export

get_ma_results = function(
  ma_bulk_df,
  species_df,
  year){

  # Is number of routes the Site ID? not the number of days they recorded across the route?
  num_routes = length(unique(ma_bulk_df$location_name))
  num_cells = length(unique(ma_bulk_df$grts_cell_id))
  project_year = year

  auto_ids = subset(ma_bulk_df, !is.na(ma_bulk_df$auto_id))$auto_id
  manual_ids = subset(ma_bulk_df, !is.na(ma_bulk_df$manual_id))$manual_id
  num_bat_calls = length(auto_ids)

  # Clean time fields and add observed_night for wav file
  ma_bulk_df = clean_time_fields(ma_bulk_df)
  ma_bulk_df = add_observed_nights(ma_bulk_df)

  # This should be the one we use and not the nights_per_event (event splits them into different GRTS?)
  nights_per_transect = ma_bulk_df %>%
    dplyr::select(location_name, observed_night) %>%
    dplyr::group_by(location_name, observed_night) %>%
    dplyr::distinct()
  num_detector_nights = dim(nights_per_transect)[1]

  # Get species at this project
  project_species = (data.frame(id = unique(auto_ids), stringsAsFactors = FALSE) %>%
      dplyr::left_join(species_df, by = 'id'))
  species_table = project_species %>% dplyr::select(species_code, species, common_name, bat_call)

  species_found = subset(project_species, project_species$bat_call)
  species_detected_wav = subset(ma_bulk_df, ma_bulk_df$auto_id %in% species_found$id)

  num_species_detected = dim(species_found)[1]

  # Tally dataframe by
  location_acitivty_df = species_detected_wav %>% dplyr::group_by(location_name) %>% dplyr::tally(name = 'count')
  # Subset bat calls by
  lowest_activity  = min(location_acitivty_df$count)
  highest_activity = max(location_acitivty_df$count)
  median_activity  = stats::median(location_acitivty_df$count)
  mean_activity    = mean(location_acitivty_df$count)

  ma_results = paste0("A total of ",num_routes," mobile transect routes were surveyed in ",num_cells,
    " NABat GRTS cells in ",project_year," (Figure 1, Table 1). ",num_bat_calls," call files were recorded over ",
    num_detector_nights," detector nights, and ",num_species_detected,
    " species were detected (Figure 1, Table 2). Activity rate (average bat passes per route) ranged from ",
    lowest_activity," to ",highest_activity,", with a median of ",median_activity," and a mean of ",
    mean_activity," (Figures 3, 4).")
}



#' @title Get Mobile Acoustic Report examples
#'
#' @description
#' Returns al ist of some preset mobile acoustic report examples
#'
#' @export

get_ma_examples = function(){
  # description  = project_row_df$project_description
  ma_ex_description = "[EXAMPLE]: PURPOSE: Bat occupancy and abundance data in Colorado is less comprehensive than most other groups of mammals in the state. The purpose of a Bureau of Land Management-Royal Gorge Field Office (RGFO) acoustic monitoring project is to produce a statistically sound dataset that will provide a baseline for bat occupancy monitoring that may be utilized as a decision making tool for effective conservation. In addition, the project will participate in the continent-wide effort to create a coordinated bat population monitoring program at a regional and range wide scale that is able to provide inferences regarding changes in distribution and abundance of bat populations. OBJECTIVES: Establish a long-term monitoring program for bats across the RGFO; Utilize a monitoring protocol that will determine a baseline occupancy of bat species across the RGFO and may be used as an index to determine changes of occupancy in the future; Incorporate local data into the continent wide NABat dataset."
  # Set Methods in 3 sections
  ma_methods_1 = "Survey units were selected using the NABat master sampling frame, a grid-based system consisting of 10 x 10 km (100 km2) cells spanning Canada, the United States, and Mexico. The NABat master sample frame provides an ordered list of cells that's both spatially balanced and randomized by utilizing the generalized random-tessellation stratified (GRTS) survey design algorithm. Using NABat's online cell selection tool, a subset of the master sampling frame was selected by defining the overall geographic scope of this project. Individual cells were then selected for survey based on their GRTS order. These 100 km2 cells serve as the focal analytical unit for NABat analyses and are a biologically appropriate grain size given the nightly range of most bat species."
  ma_methods_2 = "Surveys were conducted on mobile transect routes 25-48 km in length using audio recording devices capable of detecting high-frequency bat echolocation calls. Routes were chosen based on guidance presented in Loeb et al. (2015). Factors considered when selecting routes included safety, road surface, habitat heterogeneity, and ability to travel continuously without stops. Directional microphones capable of recording high-frequency echolocation calls were mounted on the roof of the survey vehicle and oriented straight up. During the survey, vehicles traveled continuously at ~ 32 km/h and remained at least 100 m from previously traveled roadways in order to avoid recording the same bat twice, facilitating the estimation of abundance from transect data. GPS units were used to georeference each bat call file, and transect routes were also recorded. Routes were surveyed during the summer maternity season when bats are most active and mostly likely to be detected if present. Surveys were conducted twice each season, during the same week when possible, in order to estimate detection probability."
  ma_methods_3 = "Calls files were processed using commercially-available automated identification software. Prior to species identification, non-bat files were scrubbed using a noise filter. Next, the remaining files were identified to species using a regional classifier that only considers the species whose ranges intersect the defined region. Calls that could not be identified to species were labeled either NO ID or with a general category (LowF, HighF, 25k, 40k, etc.). Due to overlap in the characteristics of some bat species' calls and the uncertainty associated with automated ID software, a subset of calls was manually vetted in accordance with Loeb et al. (2015). All call files identified as rare species were manually vetted, as were all calls from species not known to occur in the survey area. For non-rare species known to occur in the survey area, at least one call was manually vetted per point per night to confirm species presence within the survey cell and to estimate detection probability."
  # Set Summary in 2 sections
  ma_summary_1 = "[EXAMPLE]: Survey results will be reported to relevant state biologists, USFWS Region 4, and NABat. In 2019, survey efforts were expanded to include 15 new cells and collaborative efforts with Colorado Parks and Wildlife, USFWS, and Bat Conservation International."
  ma_summary_2 = "No statistically significant changes in species richness were detected between 2018 and 2019, however, there was a significant decrease in overall activity rate between the two years. Moving forward, these data will help land managers determine priority areas for bat mitigation efforts and provide baseline data to examine habitat associations that may be important for protecting species of federal and state conservation concern"

  # Lit Cited
  ma_lit_cited = "Loeb, S.C., T.J. Rodhouse, L.E. Ellison, C.L. Lausen, J.D. Reichard, K.M. Irvine, T.E. Ingersoll, J.T.H. Coleman, W.E. Thogmartin, J.R. Sauer, C.M. Francis, M.L. Bayless, T.R. Stanley, and D.H. Johnson. 2015. A plan for the North American Bat Monitoring Program (NABat). General Technical Reports SRS-208. Asheville, NC: U.S. Department of Agriculture Forest Service, Southern Research Station. 112 p."

  return(list(ma_ex_description = ma_ex_description,
    ma_methods_1 = ma_methods_1,
    ma_methods_2 = ma_methods_2,
    ma_methods_3 = ma_methods_3,
    ma_summary_1 = ma_summary_1,
    ma_lit_cited = ma_lit_cited))
}



#' @title Build Mobile Acoustic Report Table 1
#'
#' @description
#' Returns a Table and its description. Table with NABat GRTS cells surveyed and
#' Number of unique mobile transect routes, detector nights,
#' and species detected are also shown for each cell.
#'
#' @param ma_bulk_df Dataframe create from either get_ma_bulk_wavs()
#' @param species_df Dataframe species_df
#' @param project_id Integer project id
#' @param project_df Dataframe project_df
#' @param year Integer Project year with mobile acoustic data
#'
#' @export

build_ma_table_1 = function(
  ma_bulk_df,
  project_id,
  project_df,
  species_df,
  year = NULL){

  # Create table description
  ma_descr_tbl_1 = paste0("Table 1. NABat GRTS cells surveyed in ",
    year,
    ". Number of unique mobile transect routes, detector nights, and species detected are also shown for each cell.")

  auto_ids = subset(ma_bulk_df, !is.na(ma_bulk_df$auto_id))$auto_id
  manual_ids = subset(ma_bulk_df, !is.na(ma_bulk_df$manual_id))$manual_id

  project_species = (data.frame(id = unique(auto_ids), stringsAsFactors = FALSE) %>%
      dplyr::left_join(species_df, by = 'id'))
  species_table = project_species %>% dplyr::select(species_code, species, common_name, bat_call)

  species_found = subset(project_species, project_species$bat_call)
  species_detected_wav = subset(ma_bulk_df, ma_bulk_df$auto_id %in% species_found$id)

  grts = unique(ma_bulk_df$grts_cell_id)
  # Calculate transect routes per grts
  grts_cell_by_transect = ma_bulk_df %>% dplyr::select(grts_cell_id, location_name) %>%
    dplyr::group_by(grts_cell_id) %>% dplyr::distinct()
  transect_routes_df = as.data.frame(table(grts_cell_id = grts_cell_by_transect$grts_cell_id), name = 'freq', stringsAsFactors = FALSE) %>%
    dplyr::rename('Transect Routes' = Freq)%>%
    dplyr::mutate(grts_cell_id = as.integer(grts_cell_id))

  # Calculate Detector nights at each grts
  grts_cell_by_detector_night = ma_bulk_df %>% dplyr::select(grts_cell_id, observed_night) %>% dplyr::distinct()
  detector_nights_df = as.data.frame(table(grts_cell_id = grts_cell_by_detector_night$grts_cell_id), stringsAsFactors = FALSE) %>%
    dplyr::rename('Detector Nights' = Freq)%>%
    dplyr::mutate(grts_cell_id = as.integer(grts_cell_id))

  # Calculate # of species detected at each grts
  species_detected_grts = species_detected_wav %>% dplyr::select(grts_cell_id, auto_id) %>%
    dplyr::group_by(grts_cell_id) %>% dplyr::distinct() %>%
    dplyr::tally(name = 'Auto Species Detected') %>%
    dplyr::mutate(grts_cell_id = as.integer(grts_cell_id))

  ma_table_1 = transect_routes_df %>% dplyr::left_join(detector_nights_df, by = 'grts_cell_id') %>%
    dplyr::left_join(species_detected_grts, by = 'grts_cell_id') %>%
    dplyr::rename('GRTS' = grts_cell_id)

  # Now add Counties if they exist
  project_id_ = project_id
  grts_fname = as.character(subset(project_df, project_df$project_id == project_id_)$sample_frame_short)
  # Combine the spatial information (states and counties) with the detector info and species detected
  # Get grts_fname_df
  grts_fname_df = grts_lookup_df[grts_fname][[1]]

  # Build Dataframe with grts and their center points
  grts_df = data.frame(GRTS_Cell = grts) %>% dplyr::left_join(grts_fname_df, by = c('GRTS_Cell'='GRTS_ID')) %>%
    dplyr::select(GRTS_Cell, center) %>% rowwise() %>%
    dplyr::rename('GRTS' = 'GRTS_Cell') %>%
    dplyr::mutate(y = as.numeric(strsplit(center, split=',')[[1]][1])) %>%
    dplyr::mutate(x = as.numeric(strsplit(center, split=',')[[1]][2]))


  # If in CONUS add State and County.  Otherwise exclude
  if (grts_fname == 'Continental US'){
    state_county = ll_to_county_state(dplyr::select(grts_df, x, y))
    grts_df['state_county'] = state_county
    state_df = data.frame(State = state.name, state_abr = state.abb, stringsAsFactors = FALSE)

    grts_df_final = grts_df %>% rowwise() %>%
      dplyr::mutate(State = strsplit(state_county,',')[[1]][1]) %>%
      dplyr::mutate(County = .simpleCap(strsplit(state_county,',')[[1]][2])) %>%
      dplyr::left_join(state_df, by = 'State') %>%
      dplyr::select(-state_county, -x, -y, -center, -State) %>%
      dplyr::left_join(ma_table_1, by = 'GRTS') %>%
      dplyr::rename('GRTS Cell' = GRTS, 'State' = state_abr) %>%
      dplyr::arrange(State, County)
    ma_ft1 = flextable::flextable(grts_df_final)
    ma_ft1 = flextable::width(ma_ft1, width = 1)
    ma_ft1 = flextable::width(ma_ft1, width = .5, j = 'State')
  }else{
    grts_df_final = grts_df %>%
      dplyr::select(-x, -y, -center) %>%
      dplyr::left_join(ma_table_1) %>%
      dplyr::rename('GRTS Cell' = GRTS)
    ma_ft1 = flextable::flextable(grts_df_final)
    ma_ft1 = flextable::width(ma_ft1, width = 1)
    ma_ft1 = flextable::width(ma_ft1, width = 1.5, j = c('Transect Routes', 'Detector Nights', 'Auto Species Detected'))
  }
  ma_ft1 = flextable::set_header_labels(ma_ft1, values = names(grts_df_final))
  ma_ft1 = flextable::height(ma_ft1, height =.5, part = 'header')
  ma_ft1 = flextable::fontsize(ma_ft1, size = 10, part = "all")

  return (list(table = ma_ft1, description = ma_descr_tbl_1))
}



#' @title Build Mobile Acoustic Report Table 2
#'
#' @description
#' Bat species table for this input ma_bulk_df
#'
#' @param ma_bulk_df Dataframe create from either get_ma_bulk_wavs()
#' @param species_df Dataframe species dataframe
#' @param year Integer (optional) Project year with mobile acoustic data
#'
#' @export

build_ma_table_2 = function(
  ma_bulk_df,
  species_df,
  year = NULL){

  # Build Description
  ma_descr_tbl_2 = paste0("Table 2. Bat species detected in ",
    year,
    ". Common name, geographic range, and number of transect routes with detections are displayed for all species.")

  auto_ids = subset(ma_bulk_df, !is.na(ma_bulk_df$auto_id))$auto_id
  manual_ids = subset(ma_bulk_df, !is.na(ma_bulk_df$manual_id))$manual_id

  nights_per_transect = ma_bulk_df %>%
    dplyr::select(location_name, observed_night) %>%
    dplyr::group_by(location_name, observed_night) %>%
    dplyr::distinct()

  project_species = (data.frame(id = unique(auto_ids), stringsAsFactors = FALSE) %>%
      dplyr::left_join(species_df, by = 'id'))
  species_table = project_species %>% dplyr::select(species_code, species, common_name, bat_call)

  species_found = subset(project_species, project_species$bat_call)
  species_detected_wav = subset(ma_bulk_df, ma_bulk_df$auto_id %in% species_found$id)

  int_ma_tbl2 = species_detected_wav %>% dplyr::left_join(species_found, by = c('auto_id' = 'id')) %>%
    dplyr::select(location_name, auto_id, species, common_name) %>% distinct() %>%
    dplyr::select(species) %>% dplyr::arrange(species) %>%
    subset(!species =='NoID')
  num_transect_routes = as.data.frame(table(int_ma_tbl2), stringsAsFactors = FALSE)$Freq

  ma_tbl2 = species_detected_wav %>% dplyr::left_join(species_found, by = c('auto_id' = 'id')) %>%
    dplyr::select(species, common_name) %>%
    dplyr::distinct() %>%
    subset(!species =='NoID' ) %>%
    dplyr::rename('Species' = species, 'Common Name' = common_name) %>%
    dplyr::arrange(Species) %>%
    dplyr::mutate(year = num_transect_routes)
  names(ma_tbl2)[names(ma_tbl2) == "year"] = paste0("# of Transect Routes with Detections ", year)

  ma_ft2 = flextable::flextable(ma_tbl2)
  ma_ft2 = flextable::set_header_labels(ma_ft2, values = names(ma_tbl2))
  ma_ft2 = flextable::height(ma_ft2, height =.5, part = 'header')
  ma_ft2 = flextable::width(ma_ft2, width = 2, j = names(ma_tbl2))
  ma_ft2 = flextable::fontsize(ma_ft2, size = 10, part = "all")
  ma_ft2 = italic(ma_ft2, italic = TRUE, part = "body", j = 'Species')

  return (list(table = ma_ft2, description = ma_descr_tbl_2))
}



#' @title Build Mobile Acoustic Report Table 3
#'
#' @description
#' Potentially returns 2 tables and their descriptions.  Table 3A is
#' for the auto detected species table and table 3B is for the manual
#' detected species table.
#'
#' @param ma_bulk_df Dataframe create from either get_ma_bulk_wavs()
#' @param species_df Dataframe Species dataframe
#' @param nightly_observed_list (optional) List from running get_observed_nights()
#' @param year Integer (optional) Project year with mobile acoustic data
#'
#' @export

build_ma_table_3 = function(
  ma_bulk_df,
  nightly_observed_list,
  species_df,
  year = NULL){

  # Create descriptions for the 2 tables
  ma_descr_tbl_3b = paste0("Table 3b. Number of manual calls from each detected species in all GRTS cells surveyed by mobile transect in ",year,".")
  ma_descr_tbl_3a = paste0("Table 3a. Number of automatic calls from each detected species in all GRTS cells surveyed by mobile transect in ",year,".")

  # nightly_observed_list = get_observed_nights(ma_bulk_df)
  auto_nights_df = nightly_observed_list$auto_nightly_df
  manual_nights_df  = nightly_observed_list$manual_nightly_df
  # Individual Manual or Auto
  manual_species_totals_l = get_species_counts_long(manual_nights_df, filter=TRUE) %>% dplyr::select(-c(type,project_id,year))
  auto_species_totals_l   = get_species_counts_long(auto_nights_df, filter=TRUE)  %>% dplyr::select(-c(type,project_id,year))

  table_3_list = list(table_a = NULL, table_b = NULL,
    description_a = ma_descr_tbl_3a, description_b = ma_descr_tbl_3b)

  # 3a
  if (length(names(auto_species_totals_l)) == 1){
    auto_species_bool = FALSE
  }else{
    auto_species_bool = TRUE
    ma_ft3a = flextable::flextable(auto_species_totals_l)
    ma_ft3a = flextable::set_header_labels(ma_ft3a, values = names(auto_species_totals_l))
    ma_ft3a = flextable::height(ma_ft3a, height =.5, part = 'header')
    ma_ft3a = flextable::bold(ma_ft3a, part = 'header')
    ma_ft3a = flextable::width(ma_ft3a, width = .6)
    ma_ft3a = flextable::fontsize(ma_ft3a, size = 8, part = "all")
    ma_ft3a = flextable::border(ma_ft3a, border.top = fp_border(color = 'grey', width = 1))
    table_3_list$table_a = ma_ft3a
  }
  # 3b
  if (length(names(manual_species_totals_l)) == 1){
    manual_species_bool = FALSE
  }else{
    manual_species_bool = TRUE
    ma_ft3b = flextable::flextable(manual_species_totals_l)
    ma_ft3b = flextable::set_header_labels(ma_ft3b, values = names(manual_species_totals_l))
    ma_ft3b = flextable::height(ma_ft3b, height =.5, part = 'header')
    ma_ft3b = flextable::bold(ma_ft3b, part = 'header')
    ma_ft3b = flextable::width(ma_ft3b, width = .6)
    ma_ft3b = flextable::fontsize(ma_ft3b, size = 8, part = "all")
    ma_ft3b = flextable::border(ma_ft3b, border.top = fp_border(color = 'grey', width = 1))
    table_3_list$table_b = ma_ft3b
  }
  return (table_3_list)
}


#' @title Build Mobile Acoustic Report Figure 1
#'
#' @description
#'  Creates a leaflet Map for mobile acoustic data
#'
#' @param ma_bulk_df Dataframe create from either get_ma_bulk_wavs()
#' @param project_id Integer project id
#' @param project_df Dataframe project_df
#' @param year Integer (optional) Project year with mobile acoustic data
#'
#' @export

build_ma_figure_1 = function(
  ma_bulk_df,
  project_id,
  project_df,
  year = NULL){

  # Build Description
  ma_descr_fig1  = paste0("Figure 1. Map of all NABat GRTS cells surveyed in ",year," and detector points in each surveyed cell. ")

  all_grts = unique(ma_bulk_df$grts_cell_id)

  project_id_ = project_id
  grts_fname = as.character(subset(project_df, project_df$project_id == project_id_)$sample_frame_short)
  # Get grts_fname_df
  grts_fname_df = grts_lookup_df[grts_fname][[1]]
  # Create grts_fname_df
  grts_df = data.frame(GRTS_ID = all_grts) %>% dplyr::left_join(grts_fname_df, by = c('GRTS_ID'), type = "left")

  # Creating map with an Imagery layer
  m = leaflet() %>% addTiles()
  # Loop through all all_grts, create a polygon for each, and add to the leaflet map m
  count = 0
  for (grts_cell in all_grts){
    color_ = 'green'
    color_2 = 'green'
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
  m = m %>% addLegend('bottomright',labels = c('NABat GRTS Cell', 'Mobile Transect'), colors = c('#198a00', 'blue'), opacity =1)

  # Add the lines from the acoustic_mobile events
  mobile_ids = unique(ma_bulk_df$mobile_acoustic_values_id)

  for (mobile_id in mobile_ids){
    mobile_df = subset(ma_bulk_df, ma_bulk_df$mobile_acoustic_values_id == mobile_id)
    transect_geom = TRUE
    if(length(unique(mobile_df$latitude)) == 1 & is.na(unique(mobile_df$latitude)[1])){
      transect_geom = FALSE
    }else{
      lats = mobile_df$latitude
      lons = mobile_df$longitude
    }

    if(transect_geom){
      # Create polyline
      l = cbind(lons, lats)
      li = Line(l)
      lin = Lines(list(li), ID = "a")
      line = SpatialLines(list(lin))

      ps = matrix(as.numeric(unlist(data.frame(lons = lons, lats = lats, id = mobile_id))),nrow=nrow(data.frame(lats = lats, lons = lons, id=mobile_id)))
      p = SpatialPoints(ps, proj4string=CRS(as.character(NA)))


      m = m %>% addCircleMarkers(data = p, color = 'blue', weight = 1, radius = .1, fillOpacity = 1, label = mobile_id)
      # m = m %>% addPolylines(data = line, color = 'red', opacity = 2, weight = .8)
    }
  }
  return (list(map = m, description = ma_descr_fig1))
}



#' @title Build Mobile Acoustic Report Figure 2
#'
#' @description
#' Creates a Number of bat passes for each species plot
#'
#' @param ma_bulk_df Dataframe create from either get_ma_bulk_wavs()
#' @param species_df Dataframe Species Dataframe
#' @param year Integer (optional) Project year with mobile acoustic data
#'
#' @export


build_ma_figure_2 = function(
  ma_bulk_df,
  species_df,
  year = NULL){

  # Build descriptions
  ma_descr_fig2a = paste0("Figure 2a. ",year," bat activity rate (average number of bat passes per night) by species. Species with at least one manual identification per site are shown in blue. Species identified only by automated identification software are shown in orange and species identified only by manual identification software are shown in green.")
  ma_descr_fig2b = paste0("Figure 2b. ",year," bat activity rate (average number of bat passes per night using a logarithmic scale) by species. Species with at least one manual identification per site are shown in blue. Species identified only by automated identification software are shown in orange and species identified only by manual identification software are shown in green.")
  auto_ids = subset(ma_bulk_df, !is.na(ma_bulk_df$auto_id))$auto_id
  manual_ids = subset(ma_bulk_df, !is.na(ma_bulk_df$manual_id))$manual_id

  project_species_auto = (data.frame(id = auto_ids, stringsAsFactors = FALSE) %>%
      dplyr::left_join(species_df, by = 'id')) %>% subset(bat_call) %>%
    dplyr::select(species_code)
  auto_species_counts = as.data.frame(table(project_species_auto), stringsAsFactors = FALSE) %>% dplyr::rename('auto_freq' = Freq)
  auto_species = unique(project_species_auto$species_code)

  project_species_manual = (data.frame(id = manual_ids, stringsAsFactors = FALSE) %>%
      dplyr::left_join(species_df, by = 'id')) %>% subset(bat_call) %>%
    dplyr::select(species_code)
  man_species_counts = as.data.frame(table(project_species_manual), stringsAsFactors = FALSE) %>% dplyr::rename('man_freq' = Freq)
  manual_specices = unique(project_species_manual$species_code)


  all_species_names = unique(c(auto_species, manual_specices))
  methods = c()
  for (species in all_species_names){
    if (species %in% manual_specices & species %in% auto_species){
      method = 'Auto, Manual'
    }else if(species %in% manual_specices){
      method = 'Manual'
    }else if(species %in% auto_species){
      method = 'Auto'
    }
    methods = c(methods, method)
  }

  if (is.null(methods)){
    methods = rep('None', length(all_species_names))
  }

  # Setting whether or not manual species is TRUE or false
  man_ids = unique(ma_bulk_df$manual_id[!is.na(ma_bulk_df$manual_id)])

  if(length(man_ids) > 0){
    species_method = data.frame(species = all_species_names, method = methods, stringsAsFactors = FALSE) %>%
      dplyr::left_join(auto_species_counts, by = c("species" = "project_species_auto")) %>%
      dplyr::left_join(man_species_counts, by = c("species" = "project_species_manual")) %>%
      dplyr::mutate(auto_freq = ifelse(is.na(auto_freq), 0, auto_freq)) %>%
      dplyr::mutate(man_freq = ifelse(is.na(man_freq), 0, man_freq)) %>%
      dplyr::group_by(species) %>%
      dplyr::mutate(total_freq = auto_freq + man_freq)

    fig2_methods_df = species_method %>%
      dplyr::mutate(method_exp = NA) %>%
      dplyr::mutate(method_exp = case_when(method %in% 'Auto, Manual' ~ 'At least one manual ID/site',
        method %in% 'Auto' ~ 'Auto ID only',
        method %in% 'Manual' ~ 'Manual ID only'))
  }else{
    species_method = data.frame(species = all_species_names, method = methods, stringsAsFactors = FALSE) %>%
      dplyr::left_join(auto_species_counts, by = c("species" = "project_species_auto")) %>%
      dplyr::mutate(auto_freq = ifelse(is.na(auto_freq), 0, auto_freq)) %>%
      dplyr::group_by(species) %>%
      dplyr::mutate(total_freq = auto_freq)

    fig2_methods_df = species_method %>%
      dplyr::mutate(method_exp = NA) %>%
      dplyr::mutate(method_exp = case_when(method %in% 'Auto, Manual' ~ 'At least one manual ID/site',
        method %in% 'Auto' ~ 'Auto ID only',
        method %in% 'Manual' ~ 'Manual ID only'))
  }

  # Set fonts for Plots
  f = list(family = "cambria", size = 24, color = "#6b6b6b")
  l = list(family = "cambria", size = 22, color = "#6b6b6b")
  ll = list(family = "cambria", size = 16, color = "black")
  leg = list(family = "cambria", size = 16, color = "#6b6b6b")
  # ti = list(title = "Bat Activity rate", titlefont = f)
  x_ = list(title = "Bat Species", titlefont = l)
  x_log = list(title = "Bat Species")
  y = list(title = "Average No. of Bat Passes",titlefont = l)
  y_log = list(title = "Average No. of Bat Passes(Log Scale)",titlefont = l, type = 'log')
  # Setting the margin for these plots
  m_fig_2     = list(t = 50, b = 30, l = 30, r = 15, pad = 0)
  m_fig_2_log = list(t = 50, b = 40, l = 45, r = 15, pad = 0)


  bat_id_type = fig2_methods_df$method_exp
  bat_id_color_df = data.frame(colors = c('#ff8400','#337acc', '#23992f'),
    types = c('At least one manual ID/site', 'Auto ID only', 'Manual ID only'), stringsAsFactors = FALSE)
  bat_id_colors = subset(bat_id_color_df, bat_id_color_df$types %in% bat_id_type)$colors

  # fig 2a
  fig2a_p = plot_ly(x = all_species_names, y = fig2_methods_df$total_freq, type = 'bar',
    # width = 850, height = 650,
    marker = list(line = list(color = 'black', width = .5)),
    color = fig2_methods_df$method_exp, colors = bat_id_colors) %>%
    layout(xaxis = x_, yaxis = y,
      margin = m_fig_2,
      # title = list(x = .1, y = 1.4, text = 'Average Bat Activity Rate', font = f),
      title = 'Average Bat Activity Rate',
      font = leg, showlegend = TRUE, autosize=TRUE, bargap = .6,
      legend = list(x = .2, y = 1.05, orientation = 'h', font = leg))
  # fig 2b
  fig2b_p = plot_ly(x = all_species_names, y = fig2_methods_df$total_freq, type = 'bar',
    # width = 850, height = 650,
    marker = list(line = list(color = 'black', width = .5)),
    color = fig2_methods_df$method_exp, colors = bat_id_colors) %>%
    layout(xaxis = x_log, yaxis = y_log,
      margin = m_fig_2_log,
      # title = list(x = .1, y = 1.1, text = 'Average Bat Activity Rate using a Logarithmic Scale', font = f),
      title = 'Average Bat Activity Rate using a Logarithmic Scale',
      font = leg, showlegend = TRUE, autosize=TRUE, bargap = .6,
      legend = list(x = .2, y = 1.05, orientation = 'h', font = leg))


  return (list(figure_a = fig2a_p, figure_b = fig2b_p,
    description_a = ma_descr_fig2a, description_b = ma_descr_fig2b))
}



#' @title Build Mobile Acoustic Report Figure 3
#'
#' @description
#'
#' @param ma_bulk_df Dataframe create from either get_ma_bulk_wavs()
#' @param species_df Dataframe species_df
#' @param year Integer (optional) Project year with mobile acoustic data
#'
#' @export
#'

build_ma_figure_3 = function(
  ma_bulk_df,
  species_df,
  year = NULL){

  # Build description
  ma_descr_fig3 = paste0("Figure 3. ",
    year,
    " bat activity rate (average number of bat passes per survey) recorded in each NABat GRTS cell.")

  auto_ids = subset(ma_bulk_df, !is.na(ma_bulk_df$auto_id))$auto_id

  project_species_auto = (data.frame(id = auto_ids, stringsAsFactors = FALSE) %>%
      dplyr::left_join(species_df, by = 'id')) %>% subset(bat_call) %>%
    dplyr::select(species_code)

  all_grts = unique(ma_bulk_df$grts_cell_id)

  # Calculate bat calls per grts cell
  grts_calls_df = as.data.frame(table((ma_bulk_df %>% dplyr::select(grts_cell_id, auto_id) %>%
      dplyr::left_join(species_df, by = c('auto_id' = 'id')) %>% subset(bat_call) %>%
      dplyr::select(grts_cell_id))$grts_cell_id), stringsAsFactors = FALSE) %>%
    dplyr::rename('grts_cell_id' = Var1, 'bat_calls' = Freq)

  leg = list(family = "cambria", size = 16, color = "#6b6b6b")
  l = list(family = "cambria", size = 22, color = "#6b6b6b")
  x = list(title = "NABat GRTS Cell", titlefont = leg)
  y = list(title = "Average No. of Bat Passes",titlefont = l, font=list(family = "cambria", size = 10, color = "#6b6b6b"))
  leg = list(family = "cambria", size = 16, color = "#6b6b6b")
  m_fig_3 = list(t = 50, b = 20, l = 20, r = 10, pad = 0)


  fig3_p = plot_ly(x = grts_calls_df$grts_cell_id[1:30], y = grts_calls_df$bat_call[1:30], type = 'bar',
    # width = 850, height = 650,
    marker = list(line = list(color = 'black', width = .5)),
    color = '#337acc', colors = c('#337acc')) %>%
    layout(margin = m_fig_3, font = leg, xaxis = x, yaxis = y, showlegend = F, autosize=T, bargap = .6,
      title = 'Average Bat Calls at each GRTS',
      legend = list(x = .2, y = 1.05, orientation = 'h', font = leg))

  return (list(figure = fig3_p, description = ma_descr_fig3))
}












