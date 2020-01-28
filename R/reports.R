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
#' @import rprojroot
#' @import kableExtra
#' @import plotly
#' @import stringr
#'
#' @description
#' Using the outputs from get_projects(), get_project_surveys(), get_acoustic_bulk_wavs(),
#' and get_observed_nights() this function will create a report .html file to an output_dir.
#' This function can run with these outputs or without.
#' @param token String token created from get_nabat_gql_token function
#' @param project_id String or Integer project id from NABat ex: 105 or '105'
#' @param output_dir String output directory to save report .html file ex: /path/to/directory
#' @param file_name (optional) String output file name ex: 'my_report.html' ex: my_report.html
#' @param survey_df (optional) Dataframe from running get_project_surveys()
#' @param acoustic_bulk_df (optional) Dataframe from running get_acoustic_bulk_wavs()
#' @param manual_nights_df (optional) Dataframe from running get_observed_nights()
#' @param auto_nights_df (optional) Dataframe from running get_observed_nights()
#' @param nightly_plots_type (optional) String 'grts' | 'sites'
#' @param nightly_observed_list (optional) List from running get_observed_nights()
#' @param num_plots (optional) Integer for number of plots. Ex: any number from 0 to 60
#'
#' @keywords bats, NABat, GQL
#' @examples
#'
#' \dontrun{
#' get_acoustic_stationary_report(token      = 'generated-nabat-gql-token',
#'                                output_dir = '/path/to/your/output/directory',
#'                                file_name  = 'report.html',
#'                                project_id = 'number or string of a number')
#' }
#'
#' @export
#'
get_acoustic_stationary_report = function(token,
                                          project_id,
                                          output_dir,
                                          output_type = 'html',
                                          project_name = NULL,
                                          project_description = NULL,
                                          file_name = 'report.html',
                                          nightly_plots_type = 'grts',
                                          survey_df = NULL,
                                          acoustic_bulk_df = NULL,
                                          nightly_observed_list = NULL,
                                          num_plots = NULL){

  template  = system.file("templates", "acoustic_stationary_report.Rmd", package = "nabatr")
  nabat_png = system.file("templates", "nabat_logo.png", package = "nabatr")

  # Check to see if output_dir exists
  if (dir.exists(output_dir)){
    message(template)
    message(nabat_png)

    # Get Project dataframe
    project_df = get_projects(token    = token)
    this_project_id = project_id
    project_name = subset(project_df, project_df$project_id == this_project_id)$project_name

    # Get survey dataframe
    if (is.null(survey_df)){
      survey_df = get_project_surveys(
        token      = token,
        project_id = project_id)
    }

    # Get stationary acoustic bulk upload format dataframe
    if (is.null(acoustic_bulk_df)){
      acoustic_bulk_df = get_acoustic_bulk_wavs(token      = token,
        survey_df  = survey_df,
        project_id = project_id)
    }

    # Get Acoustic stationary acoustic bulk dataframe
    if(is.null(nightly_observed_list)){
      nightly_observed_list = get_observed_nights(acoustic_bulk_df)
    }

    manual_nights_df      = nightly_observed_list$manual_nightly_df
    auto_nights_df        = nightly_observed_list$auto_nightly_df

    if (dim(manual_nights_df)[1] == 0  && dim(auto_nights_df)[1] == 0){
      message('Error, this project has no data to build a report with')
      return('Failed')
    }

    # Build maps using grts found in survey_df and auto_nights_df
    grts_map = get_grts_leaflet_map(all_grts       = unique(survey_df$grts_cell_id),
                                    grts_with_data = unique(auto_nights_df$GRTS))

    # MANUAL
    manual_species_totals_l  = get_species_counts_long(manual_nights_df)
    manual_species_totals_w  = get_species_counts_wide(manual_nights_df) # List of 2 dfs
    manual_species_grts_df_w = manual_species_totals_w$species_grts_df
    # AUTOMATIC
    auto_species_totals_l  = get_species_counts_long(auto_nights_df)
    auto_species_totals_w  = get_species_counts_wide(auto_nights_df) # List of 2 dfs
    auto_species_grts_df_w = auto_species_totals_w$species_grts_df

    # Re-order dataframe by site_id, then observed_night
    ordered_grts_df_auto = auto_nights_df[order(auto_nights_df$site_id, auto_nights_df$observed_night),]
    ordered_grts_df_auto = dplyr::select(ordered_grts_df_auto, -c('25k', 'NoID', 'HighF'))
    ordered_grts_df_auto$type = 'automatic'

    ordered_grts_df_man = manual_nights_df[order(manual_nights_df$site_id, manual_nights_df$observed_night),]
    ordered_grts_df_man = dplyr::select(ordered_grts_df_man, -c('25k', 'NoID', 'HighF'))
    ordered_grts_df_man$type  = 'manual'

    combined_data = rbind(ordered_grts_df_auto, ordered_grts_df_man)
    ordered_combined_data_ = combined_data[order(combined_data$site_id, combined_data$observed_night),]
    row.names(ordered_combined_data_) = NULL
    # Add factor to GRTS to retain order for building out plots in report
    ordered_combined_data_$GRTS = factor(ordered_combined_data_$GRTS, levels = unique(ordered_combined_data_$GRTS))
    ordered_combined_data_$site_id = factor(ordered_combined_data_$site_id, levels = unique(ordered_combined_data_$site_id))


    # Create the list of dataframes to use for plotting
    if (nightly_plots_type == 'grts'){
      # By GRTS
      split_list = ordered_combined_data_ %>% split(ordered_combined_data_$GRTS)
      if (is.null(num_plots)){
        num_plots = length(split_list)}
      plot_list = lapply(split_list[1],  build_grts_plot, type = 'grts')
    }

    if (nightly_plots_type == 'sites'){
      # By sites
      split_list = ordered_combined_data_ %>% split(ordered_combined_data_$site_id)
      if (is.null(num_plots)){
        num_plots = length(split_list)}
      plot_list = lapply(split_list[1],  build_grts_plot, type = 'sites')
    }


    # Specifiy template in data directory
    message(paste0("Checking report template location: ", template))

    if (output_type == 'html'){
      rmarkdown::render(input       = template,
                        output_file = file_name,
                        output_dir  = output_dir,
                        output_format = "html_document")
    } else if (output_type == 'pdf'){
      rmarkdown::render(input       = template,
                        output_file = file_name,
                        output_dir  = output_dir,
                        output_format = "pdf_document")
    }


    # Return the location of the downloaded report
    return(template)
  } else{
    message('Failed to find output directory: ', output_dir)
  }
}



#' @title Build Report document in .docx file for Acoustic Stationary Project Data
#'
#' @description Using the outputs from get_projects(), get_project_surveys(), get_acoustic_bulk_wavs(),
#' and get_observed_nights() this function will create a report .docx file to an out_dir.
#'
#' @import mapview
#' @import officer
#' @import magrittr
#' @import maps
#' @import maptools
#' @import sp
#' @import flextable
#'
#' @param out_dir String output directory to save report .html file ex: /path/to/directory
#' @param file_name String output file name ex: paste0('doc_report_',project_id_,'_',Sys.Date(),'.docx')
#' @param project_df Dataframe from running get_projects()
#' @param project_id Integer project id from NABat ex: 105
#' @param auto_nights_df Dataframe from running get_observed_nights()
#' @param manual_nights_df Dataframe from running get_observed_nights()
#' @param cover_photo String path to a .png file
#' @param map (optional) output from get_grts_leaflet_map()
#' @param manual_species_grts_df_w Dataframe from running get_species_counts_wide()
#' @param auto_species_grts_df_w Dataframe from running get_species_counts_wide()
#' @param auto_species_totals_l Dataframe from running get_species_counts_long()
#' @param manual_species_totals_l Dataframe from running get_species_counts_long()
#' @param acoustic_bulk_df Dataframe from running get_observed_nights()
#' @param date Date current time in a month/day/Year format ex: format(Sys.time(), "%B %d, %Y")
#'
#' \dontrun{
#' doc_ = build_ac_doc(out_dir = '/path/to/output/dir',
#'                     file_name  = paste0('doc_report_',project_id_,'_',Sys.Date(),'.docx'),
#'                     project_df = project_df_,
#'                     project_id = project_id_,
#'                     auto_nights_df = auto_nights_df_,
#'                     manual_nights_df = manual_nights_df_,
#'                     cover_photo = '/path/to/a/cover/photo.png',
#'                     map = grts_map,
#'                     manual_species_grts_df_w = manual_species_grts_df_w_,
#'                     auto_species_grts_df_w = auto_species_grts_df_w_,
#'                     auto_species_totals_l = auto_species_totals_l_,
#'                     manual_species_totals_l = manual_species_totals_l_,
#'                     date = format(Sys.time(), "%B %d, %Y"),
#'                     acoustic_bulk_df = acoustic_bulk_df_)
#' }
#'
#' @export
#'

build_ac_doc = function(out_dir,
  file_name,
  manual_species_grts_df_w,
  auto_species_grts_df_w,
  project_df,
  project_id,
  acoustic_bulk_df,
  manual_species_totals_l,
  auto_species_totals_l,
  auto_nights_df,
  manual_nights_df,
  cover_photo = NULL,
  date = format(Sys.time(), "%B %d, %Y"),
  map = NULL){

  print ('Enter Report Function')

  if (dir.exists(paste0(out_dir, '/temps/'))==FALSE){
    dir.create(paste0(out_dir, '/temps/'))
  }

  print ('Set Variables')
  logo_img_ = system.file("templates", "nabat_logo.png", package = "nabatr")
  proj_id = project_id
  project_row_df = subset(project_df, project_df$project_id == proj_id)
  title        = project_row_df$project_name
  by           = project_row_df$owner_email
  organization = project_row_df$organization
  # description  = project_row_df$project_description
  description = "Ex. PURPOSE: Bat occupancy and abundance data in Colorado is less comprehensive than most other groups of mammals in the state. The purpose of a Bureau of Land Management-Royal Gorge Field Office (RGFO) acoustic monitoring project is to produce a statistically sound dataset that will provide a baseline for bat occupancy monitoring that may be utilized as a decision making tool for effective conservation. In addition, the project will participate in the continent-wide effort to create a coordinated bat population monitoring program at a regional and range wide scale that is able to provide inferences regarding changes in distribution and abundance of bat populations. OBJECTIVES: Establish a long-term monitoring program for bats across the RGFO; Utilize a monitoring protocol that will determine a baseline occupancy of bat species across the RGFO and may be used as an index to determine changes of occupancy in the future; Incorporate local data into the continent wide NABat dataset.]"
  # Set Methods in 3 sections
  methods_1 = "Survey units were selected using the NABat master sampling frame, a grid-based system consisting of 10 x 10 km (100 km2) cells spanning Canada, the United States, and Mexico. The NABat master sample frame provides an ordered list of cells that is spatially balanced and randomized by utilizing the generalized random-tessellation stratified (GRTS) survey design algorithm. Using NABat's online cell selection tool, a subset of the master sampling frame was selected by defining the overall geographic scope of this project. Individual cells were then selected for survey based on their GRTS order and available resources. These 100 km2 cells serve as the focal analytical unit for NABat analyses and are a biologically appropriate grain size given the nightly range of most bat species (Loeb et al. 2015). "
  methods_2 = "Recording devices capable of detecting high-frequency bat echolocation calls were deployed at 2-4 sites within each GRTS cell selected for survey. Sites were chosen based on guidance provided in Loeb et al. (2015). Factors considered when selecting sites included land ownership, accessibility, minimizing clutter, elevation, and heterogeneity of habitats within the cell. Recording devices were deployed for 4 consecutive nights during the summer maternity season when bats are most active and most likely to be detected if present in the cell. Detectors were programmed to record automatically beginning 15 minutes prior to sunset and ending 15 minutes after sunrise. Microphones were elevated ~ 3 m from the ground and oriented in the direction of least clutter to maximize detection (Loeb et al. 2015). "
  methods_3 = "Calls files were processed using commercially-available automated identification software. Prior to species identification, non-bat files were scrubbed using a noise filter. Next, the remaining files were identified to species using a regional classifier that only considers the species whose ranges intersect the defined region. Calls that could not be identified to species were labeled either NO ID or with a general category (LowF, HighF, 25k, 40k, etc.). Due to overlap in the characteristics of some bat species' calls and the uncertainty associated with automated ID software, a subset of calls was manually vetted in accordance with Loeb et al. (2015). All call files identified as rare species were manually vetted, as were all calls from species not known to occur in the survey area. For non-rare species known to occur in the survey area, at least one call was manually vetted per point per night to confirm species presence within the survey cell and to estimate detection probability."
  # Set Summary in 2 sections
  summary_1 = "Ex. Survey results will be reported to relevant state biologists, USFWS Region 4, and NABat. In 2019, survey efforts were expanded to include 15 new cells and collaborative efforts with Colorado Parks and Wildlife, USFWS, and Bat Conservation International.  "
  summary_2 = "No statistically significant changes in species richness were detected between 2018 and 2019, however, there was a significant decrease in overall activity rate between the two years. Moving forward, these data will help land managers determine priority areas for bat mitigation efforts and provide baseline data to examine habitat associations that may be important for protecting species of federal and state conservation concern. "

  # Lit Cited
  lit_cited = "Loeb, S.C., T.J. Rodhouse, L.E. Ellison, C.L. Lausen, J.D. Reichard, K.M. Irvine, T.E. Ingersoll, J.T.H. Coleman, W.E. Thogmartin, J.R. Sauer, C.M. Francis, M.L. Bayless, T.R. Stanley, and D.H. Johnson. 2015. A plan for the North American Bat Monitoring Program (NABat). General Technical Reports SRS-208. Asheville, NC: U.S. Department of Agriculture Forest Service, Southern Research Station. 112 p."

  # Set variables for the results text created below
  # Site and Cell Counts
  number_of_sites = length(unique(acoustic_bulk_df$site_name))
  number_of_cells = length(unique(acoustic_bulk_df$grts_cell_id))
  # Selected Years - should be 1
  selected_year = unique(format(as.Date(acoustic_bulk_df$recording_time), '%Y'))[1]
  # Total number of bat calls (all recording wav files counted)
  number_of_bat_calls = length(acoustic_bulk_df$audio_recording_name)
  # Total number of net nights across all sites
  net_nights_df = acoustic_bulk_df %>% dplyr::mutate(site_date_nights = paste0(acoustic_bulk_df$site_name, '___', as.Date(acoustic_bulk_df$recording_time)))
  number_of_net_nights = length(unique(net_nights_df$site_date_nights))

  # If the manual_species_grts_df_w is not null
  if (!is.null(manual_species_grts_df_w)){
    # All unique species found for project across both Automatic and manual Ids
    manual_species = subset(manual_species_grts_df_w$names,
      manual_species_grts_df_w$names != 'site_totals' & manual_species_grts_df_w$names != 'NoID' & manual_species_grts_df_w$names != '25k')
  }else{
    manual_species = c('')
  }
  auto_species = subset(auto_species_grts_df_w$names,
    auto_species_grts_df_w$names != 'site_totals' & auto_species_grts_df_w$names != 'NoID' & auto_species_grts_df_w$names != '25k')
  # All species between auto and manual species
  all_species = unique(c(auto_species, setdiff(auto_species, manual_species)))
  number_of_species_detected = length(all_species)

  print ('Calculate min, max, median, mean for sites')
  # Calculate some min, max, median, and averages across sites
  low_avg_per_night = min(plyr::count(acoustic_bulk_df, 'site_name')$freq)
  high_avg_per_night = max(plyr::count(acoustic_bulk_df, 'site_name')$freq)
  median_activity_rate = median(plyr::count(acoustic_bulk_df, 'site_name')$freq)
  mean_activity_rate = round(mean(plyr::count(acoustic_bulk_df, 'site_name')$freq),0)

  # Text for Results using Project Summary Data
  results_overview = paste0("A total of ", number_of_sites," sites in ", number_of_cells," NABat GRTS cells were surveyed in ",
    selected_year," (Figure 1, Table 1). ", number_of_bat_calls," call files were recorded over ",
    number_of_net_nights," net nights, and ", number_of_species_detected,
    " species were detected (Figure 1, Table 2). Activity rate (average bat passes per night) ranged from ",
    low_avg_per_night," to ", high_avg_per_night,", with a median of ", median_activity_rate," and a mean of ",
    mean_activity_rate," (Figures 3, 4).")

  descr_fig1 = paste0("Figure 1. Map of all NABat GRTS cells surveyed in ",selected_year ," and detector points in each surveyed cell. ")

  # Extract the names for species that have counts > 0
  get_species_names = function(df){
    this_df = df
    row_species = names(this_df[,colSums(this_df) > 0])
    return (row_species)
  }

  sites = unique(acoustic_bulk_df$site_name)
  all_GRTS = unique(acoustic_bulk_df$grts_cell_id)

  all_rows_auto = data.frame()
  all_rows_man = data.frame()
  all_grts_rows = data.frame()
  table3_df = data.frame()
  final_all_species = c()

  print ('Building dataframes for all GRTS cells')
  names_length_man = length(names(manual_species_totals_l))
  for (grts in all_GRTS){
    # MANUAL
    if (names_length_man == 4){
      man_species_names = c('')
      grts_species_man  = c('')
    }else{
      this_row_man = subset(manual_species_totals_l, manual_species_totals_l$GRTS == grts)
      # Exclude NoID from species calculation
      grts_species_man = get_species_names(this_row_man %>% dplyr::select(-type, -project_id, -GRTS, -year))
      species_length_man = length(grts_species_man)
      this_row_man['Species_Detected'] = species_length_man
      this_row_man['all_species'] = paste(grts_species_man, collapse='_')
      all_rows_man = rbind(all_rows_man, this_row_man)

      # MANUAL names
      man_species_names = as.character(subset(pkg.env$bats_df, pkg.env$bats_df$species_code %in% grts_species_man)$species)
      man_species_names = man_species_names[man_species_names != ""]
    }


    # AUTO
    this_row_auto = subset(auto_species_totals_l, auto_species_totals_l$GRTS == grts)
    # Exclude NoID from species calculation
    grts_species_auto = get_species_names(this_row_auto %>% dplyr::select(-type, -project_id, -GRTS, -year))
    species_length_auto = length(grts_species_auto)
    this_row_auto['Species_Detected'] = species_length_auto
    this_row_auto['all_species'] = paste(grts_species_auto, collapse='_')
    all_rows_auto = rbind(all_rows_auto, this_row_auto)

    # AUTO names
    auto_species_names = as.character(subset(pkg.env$bats_df, pkg.env$bats_df$species_code %in% grts_species_auto)$species)

    all_species_names = unique(c(man_species_names, auto_species_names))
    all_species_names = all_species_names[all_species_names != ""]

    # Build Method of species ID
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

    if (is.null(methods)){
      methods = rep('None', length(all_species_names))
    }

    table3_row_df = data.frame('GRTS' = rep(grts, length(all_species_names)), stringsAsFactors = FALSE) %>%
      dplyr::mutate(Species_Detected = all_species_names) %>%
      dplyr::mutate(Method_of_Species_ID = methods)

    table3_df = rbind(table3_df, table3_row_df)

    # Combine both species lists and grab unique to use both AUTO and MAN counts
    grts_all_species = unique(c(grts_species_man, grts_species_auto))
    grts_all_species = grts_all_species[grts_all_species != ""]
    grts_species = grts_all_species[grts_all_species != '25k' & grts_all_species != 'NoID']
    number_species_grts = length(grts_species)

    # Get detector points in each grts cell
    this_row_nights_df = subset(auto_nights_df, auto_nights_df$GRTS == grts)
    detector_nights = dim(this_row_nights_df)[1]
    detector_points = length(unique(this_row_nights_df$site_name))

    # Get detector nights
    this_grts_row = this_row_auto %>% dplyr::select(GRTS) %>%
      dplyr::mutate(Detector_Points = detector_points) %>%
      dplyr::mutate(Detector_Nights = detector_nights) %>%
      dplyr::mutate(Species_Detected = number_species_grts)
    all_grts_rows = rbind(all_grts_rows, this_grts_row)

    # Combine species from previous GRTS cell
    final_all_species = c(final_all_species, grts_all_species)
  } # End GRTS loop

  final_all_species = unique(final_all_species)

  row.names(all_rows_auto) = NULL
  row.names(all_rows_man) = NULL
  row.names(all_grts_rows) = NULL


  print ('Build dataframe with center points')

  grts_fname = get_grts_frame_name(project_df, project_id)
  grts_fname_df = pkg.env$grts_df[grts_fname][[1]]

  # Build Dataframe with grts and their center points
  grts_df = data.frame(GRTS_Cell = all_GRTS) %>% dplyr::left_join(grts_fname_df, by = c('GRTS_Cell'='GRTS_ID')) %>%
    dplyr::select(GRTS_Cell, center) %>% rowwise() %>%
    dplyr::rename('GRTS' = 'GRTS_Cell') %>%
    dplyr::mutate(y = as.numeric(strsplit(center, split=',')[[1]][1])) %>%
    dplyr::mutate(x = as.numeric(strsplit(center, split=',')[[1]][2]))


  # Combine the spatial information (states and counties) with the detector info and species detected
  .simpleCap = function(x) {
    s = strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
      sep = "", collapse = " ")
  }

  ll_to_county_state = function(points_df) {
    # type = 'state' | 'county'
    # Prepare SpatialPolygons object with one SpatialPolygon
    # per state (plus DC, minus HI & AK)
    this_map =  map('county' , fill=TRUE, col="transparent", plot=FALSE)
    # Upper case to first letter in states or counties
    IDs = sapply(sapply(strsplit(this_map$names, ":"), function(x) x[1]), .simpleCap)
    names(IDs) = NULL
    states_sp = map2SpatialPolygons(this_map, IDs=IDs,proj4string=CRS("+proj=longlat +datum=WGS84"))
    # Convert points_df to a SpatialPoints object
    points_sp = SpatialPoints(points_df, proj4string=CRS("+proj=longlat +datum=WGS84"))
    # Use 'over' to get _indices_ of the Polygons object containing each point
    indices = over(points_sp, states_sp)
    # Return the state or county names of the Polygons object containing each point
    stateNames = sapply(states_sp@polygons, function(x) x@ID)
    stateNames[indices]
  }

  # If in CONUS add State and County.  Otherwise exclude
  if (grts_fname == 'CONUS'){
    print ('Build grts_df_final')
    state_county = ll_to_county_state(dplyr::select(grts_df, x, y))
    grts_df['state_county'] = state_county

    grts_df_final = grts_df %>% rowwise() %>%
      dplyr::mutate(State = strsplit(state_county,',')[[1]][1]) %>%
      dplyr::mutate(County = .simpleCap(strsplit(state_county,',')[[1]][2])) %>%
      dplyr::select(-state_county, -x, -y, -center) %>%
      dplyr::left_join(all_grts_rows) %>%
      dplyr::arrange(State, County, Species_Detected)
  }else{
    grts_df_final = grts_df %>%
      dplyr::select(-x, -y, -center) %>%
      dplyr::left_join(all_grts_rows)
  }

  # Build flex tables for table 1 and 3
  descr_table1 = paste0("Table 1. NABat GRTS cells surveyed in ",selected_year,". Number of detector points, detector nights, and species detected are shown for each cell.")
  descr_table3 = paste0("Table 2. Bat species detected in each NABat GRTS cell surveyed, ",selected_year,". Years with detections and method of species identification are shown for each species in each cell. ")

  print ('Build flextable 1')
  # Table 1
  ft1 = flextable::flextable(grts_df_final, col_keys = names(grts_df_final))
  ft1 = flextable::height(ft1, height =.7, part = 'header')
  ft1 = flextable::width(ft1, width = 1)
  ft1 = flextable::fontsize(ft1, size = 10, part = "all")
  # Table 3
  print ('Build flextable 3')
  ft3 = flextable::flextable(table3_df, col_keys = names(table3_df))
  ft3 = flextable::height(ft3, height =.5, part = 'header')
  ft3 = flextable::width(ft3, width =2)
  ft3 = flextable::merge_v(ft3, j = 'GRTS')
  ft3 = flextable::fontsize(ft3, size = 10, part = "all")
  ft3 = flextable::italic(ft3, j = 2)
  ft3 = flextable::hline(ft3, border = fp_border(width = .75, color = "black"), part = "body")

  print ('Save out map')
  # Figure 1
  # Save out leaflet map as a png using mapview
  if (is.null(map)){
    m = leaflet() %>% addTiles() %>% addMarkers(lat=40, lng=-105) %>% setView(lat=40,lng=-105,zoom=6)
    map_out_ = paste0(out_dir, '/temps/intermediate_map.png')
    mapshot(m, file = map_out_)
  } else{
    m = map
    map_out_ = paste0(out_dir, '/temps/intermediate_map.png')
    mapshot(m, file = map_out_)
  }

  descr_fig1  = paste0("Figure 1. Map of all NABat GRTS cells surveyed in ",selected_year," and detector points in each surveyed cell. ")
  descr_fig2a = paste0("Figure 2a. ",selected_year," bat activity rate (average number of bat passes per night) by species. Species with at least one manual identification per site are shown in blue. Species identified only by automated identification software are shown in orange and species identified only by manual identification software are shown in green.")
  descr_fig2b = paste0("Figure 2b. ",selected_year," bat activity rate (average number of bat passes per night using a logarithmic scale) by species. Species with at least one manual identification per site are shown in blue. Species identified only by automated identification software are shown in orange and species identified only by manual identification software are shown in green.")
  descr_fig3 = paste0("Figure 3. ",selected_year," automatic detections across project.")

  print ('Get all bat species')
  # Get all bat species
  bat_species = final_all_species[final_all_species != '25k' & final_all_species != 'NoID' & final_all_species != 'HighF']

  print ('Build bat types df')
  all_bat_id_types = data.frame()
  # Get a bat species bat_id_type -- 'Manual ID only'|'Auto ID only'|'At least one manual ID/site'
  for (bat_spc in bat_species){
    types = unique(subset(table3_df, table3_df$Species_Detected == subset(pkg.env$bats_df, pkg.env$bats_df$species_code == bat_spc)$species)$Method_of_Species_ID)
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
      species_auto_count = subset(auto_species_grts_df_w, auto_species_grts_df_w$names == bat_spc)$species_totals
    }else if (this_type == 'Auto ID only'){
      species_auto_count = subset(auto_species_grts_df_w, auto_species_grts_df_w$names == bat_spc)$species_totals
    }else if (this_type == 'Manual ID only'){
      if (!is.null(manual_species_grts_df_w)){
        species_auto_count = subset(manual_species_grts_df_w, manual_species_grts_df_w$names == bat_spc)$species_totals
      }else {
        species_auto_count = 0
      }
    }else if(this_type == 'Not a Species'){
      species_auto_count = subset(auto_species_grts_df_w, auto_species_grts_df_w$names == bat_spc)$species_totals
    }
    if (this_type == 'Not a Species'){
      all_bat_id_types = all_bat_id_types
    }else {
      bat_id_type_row = data.frame(species = bat_spc, bat_types = this_type, auto_count = species_auto_count, stringsAsFactors = FALSE)
      all_bat_id_types = rbind(all_bat_id_types, bat_id_type_row)
    }
  }

  bat_id_type = all_bat_id_types$bat_types
  bat_auto_counts = all_bat_id_types$auto_count / length(grts_df_final$GRTS)
  bat_species = all_bat_id_types$species
  print ('Build plotly fig2')
  f = list(family = "cambria", size = 24, color = "#6b6b6b")
  l = list(family = "cambria", size = 22, color = "#6b6b6b")
  leg = list(family = "cambria", size = 16, color = "#6b6b6b")
  # ti = list(title = "Bat Activity rate", titlefont = f)
  x = list(title = "", titlefont = leg)
  y = list(title = "Average No. of Bat Passes",titlefont = l)
  y_log = list(title = "Average No. of Bat Passes(Log Scale)",titlefont = l, type = 'log')
  m = list(t = 70)

  fig2_p_base = plot_ly(x = bat_species, y = bat_auto_counts, type = 'bar',
    width = 850, height = 650,
    marker = list(line = list(color = 'black', width = .5)),
    color = bat_id_type, colors = c('#ff8400', '#337acc', '#23992f')) %>% # orange/blue/green
    layout(margin = m, font = leg, xaxis = x, yaxis = y, showlegend = TRUE, autosize=F, bargap = .6,
      legend = list(x = .2, y = 1.05, orientation = 'h', font = leg))

  # fig 2a
  fig2_p = fig2_p_base %>% layout(title = list(x = .1, y = 1.4, text = 'Average Bat Activity Rate', font = f))
  # fig 2b
  fig2_p_log = fig2_p_base %>% layout(yaxis = y_log, title = list(x = .1, y = 1.1, text = 'Average Bat Activity Rate using a Logarithmic Scale', font = f))


  print ('Save out plotly fig2')
  # Export to a file to be used to upload into the .docx
  fig2a_f = paste0(out_dir, "/temps/fig2a.png")
  plotly::export(fig2_p, file = fig2a_f)
  # Export to a file to be used to upload into the .docx
  fig2b_f = paste0(out_dir, "/temps/fig2b.png")
  plotly::export(fig2_p_log, file = fig2b_f)

  print ('Building plotly fig3')
  species_counts_df = auto_species_grts_df_w %>% subset(names != 'grts_totals') %>% dplyr::select(names, species_totals)
  col_ = colorRampPalette(c('black', '#337acc'))(length(species_counts_df$names))

  # fig 3
  pie_species = plot_ly(values = species_counts_df$species_totals, type = 'pie', width='100%',
    labels = species_counts_df$names,
    showlegend=FALSE,
    marker = list(colors = col_,line = list(color = 'black', width = .5)),
    height = 1000,
    textinfo = 'label+value') %>%
    layout(title = list(x = .1, y = .9, text = 'Automatic Detection Counts', font = f))
  print ('Save out plotly fig3')
  fig3_f = paste0(out_dir, "/temps/fig3.png")
  plotly::export(pie_species, file = fig3_f)


  print ('Set bold and par style')
  # Font for title
  bold_face = shortcuts$fp_bold(font.size = 16)
  par_style = fp_par(text.align = "center")

  print ('Begin .docx build')
  doc = read_docx() %>%
    # Add title/header
    # 'Normal', 'heading 1', 'heading 2', 'heading 3', 'centered', 'graphic title', 'table title', 'toc 1', 'toc 2', 'Balloon Text'
    body_add_img(src = logo_img_, width = 2, height = .75, style= 'centered', pos = 'before') %>%
    body_add_par(value = "", style = "centered") %>%
    body_add_par(value = "", style = "centered") %>%
    body_add_par(value = "", style = "centered") %>%
    body_add_fpar(fpar(ftext(title, prop = bold_face), fp_p = par_style ), style = 'centered') %>%
    # body_add_par(value = title, style = "graphic title") %>%
    body_add_par(value = "", style = "centered") %>%
    body_add_par(value = paste0('By ', by), style = "centered") %>%
    body_add_par(value = organization, style = "centered") %>%
    body_add_par(value = date, style = "centered") %>%

    # Add Map
    body_add_par(value = "", style = "centered") %>%
    body_add_par(value = "", style = "centered") %>%
    # body_add_par(value = "GRTS Map", style = "table title") %>%
    body_add_par(value = "", style = "centered") %>%
    body_add_par(value = "", style = "centered") %>%
    body_add_img(src = cover_photo, width = 6, height = 4, style= 'centered') %>%

    # Add summary data for project and GRTS cells
    body_add_par(value = "", style = "centered") %>%

    body_add_break() %>%

    # Project Description
    body_add_par(value = "Project Description", style = "heading 1") %>%
    body_add_par(value = "", style = "centered") %>%
    body_add_par(value = description, style = "Normal") %>%

    body_add_break() %>%

    # Methods
    body_add_par(value = "Methods", style = "heading 1") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = "Site Selection", style = "heading 2") %>%
    body_add_par(value = methods_1, style = "Normal") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = "Data Collection", style = "heading 2") %>%
    body_add_par(value = methods_2, style = "Normal") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = "Call Processing/Species Identification", style = "heading 2") %>%
    body_add_par(value = methods_3, style = "Normal") %>%

    body_add_break() %>%

    # Results
    body_add_par(value = "Results", style = "heading 1") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = results_overview, style = "Normal") %>%

    body_add_break() %>%

    # Summary
    body_add_par(value = "Summary", style = "heading 1") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = summary_1, style = "Normal") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = summary_2, style = "Normal") %>%

    body_add_break() %>%

    # Literature Cited
    body_add_par(value = "Literature Cited", style = "heading 1") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = lit_cited, style = "Normal") %>%

    body_add_break() %>%

    # Table 1
    body_add_par(value = descr_table1, style = "Normal") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_flextable(ft1, align='left') %>%
    body_add_par(value = "", style = "Normal") %>%

    body_add_break() %>%

    # Table 3
    body_add_par(value = descr_table3, style = "Normal") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_flextable(ft3, align='left') %>%

    body_add_break() %>%

    # Figure 1
    body_add_par(value = descr_fig1, style = "Normal") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_img(src = map_out_, width = 6, height = 4, style= 'centered') %>%
    body_add_par(value = "", style = "Normal") %>%

    body_add_break() %>%

    # Figure 2a
    body_add_par(value = descr_fig2a, style = "Normal") %>%
    slip_in_img(src = fig2a_f, width = 6.5, height = 5) %>%

    body_add_break() %>%

    # Figure 2b
    body_add_par(value = descr_fig2b, style = "Normal") %>%
    slip_in_img(src = fig2b_f, width = 6.5, height = 5) %>%

    body_add_break() %>%

    # Figure 3
    body_add_par(value = descr_fig3, style = "Normal") %>%
    slip_in_img(src = fig3_f, width = 6.5, height = 7)

  return(doc)
}
