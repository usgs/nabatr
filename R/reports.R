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
                                    project_id     = project_id,
                                    project_df     = project_df,
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
#' @param manual_species_grts_df_w Dataframe manual species df wide get_species_counts_wide()
#' @param auto_species_grts_df_w Dataframe auto species df wide get_species_counts_wide()
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
#'                     manual_species_grts_df_w,
#'                     auto_species_grts_df_w,
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
  map = NULL,
  range_maps = TRUE){

  print ('Enter Report Function')

  # Setup temps directory to store intermediate files
  if (dir.exists(paste0(out_dir, '/temps/'))==FALSE){
    dir.create(paste0(out_dir, '/temps/'))
  }
  if (dir.exists(paste0(out_dir, '/temps/range_maps/'))==FALSE){
    dir.create(paste0(out_dir, '/temps/range_maps/'))
  }

  # Get all species long
  all_species_totals_l_l = get_all_species_counts_long(auto_nights_df, manual_nights_df, fil = TRUE)

  # Select a species to grab it's range from the shapefile
  selected_species = names(all_species_totals_l_l %>% dplyr::select(-c('GRTS', 'type', 'project_id', 'year', 'NoID')))
  all_grts_with_data = unique(all_species_totals_l_l$GRTS)
  num_all_grts_with_data = length(all_grts_with_data)

  # Read in species ranges
  range_file = '/data/bat_species_ranges/'
  if (file.exists(range_file)){
    species_shp = readOGR(range_file)[,1:4]
  }else{
    species_shp = pkg.env$species_ranges
  }

  print (species_shp)
  # Set CRS to WGS
  proj4string(species_shp) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

  # Build species dataframe to merge with NABat's species lookup table
  species_shp_names_df = data.frame(species = as.character(unique(species_shp$SCI_NAME)),
    value = rep(1, length(unique(species_shp$SCI_NAME))), stringsAsFactors = FALSE)
  # Grab the nabatr package environment for NABat's species lookup table
  species_lookup_df = pkg.env$bats_df

  # Merge the two together
  species_range_df = species_lookup_df %>%
    dplyr::left_join(species_shp_names_df, by = c('species'='species')) %>% subset(value == 1) %>%
    dplyr::select(-value)

  if (range_maps){
    no_species_range = c()
    species_with_range = c()
    maps_grts_files = c()
    maps_range_files = c()
    # If the species exists in the ranges shapefile than build it otherwise add it to a no_species_range vector
    for (spc in selected_species){
      if (spc %in% species_range_df$species_code){
        # Grab species range
        spc_row = subset(species_range_df, species_range_df$species_code == spc)
        spc_shp = subset(species_shp, species_shp$SCI_NAME == spc_row$species)

        # Grab GRTS from data -- Both Auto and Manual species
        spc_spec_totals_df = subset(all_species_totals_l_l, all_species_totals_l_l[spc] > 0)
        spc_spec_totals_df_aut = subset(all_species_totals_l_l, all_species_totals_l_l[spc] > 0 & all_species_totals_l_l$type == 'auto')
        spc_spec_totals_df_man = subset(all_species_totals_l_l, all_species_totals_l_l[spc] > 0 & all_species_totals_l_l$type == 'manual')

        man_grts = spc_spec_totals_df_man$GRTS
        aut_grts = spc_spec_totals_df_aut$GRTS

        # Get GRTS with species Man/Auto/Both
        both_grts = intersect(man_grts, aut_grts)
        man_only_grts = setdiff(man_grts, both_grts)
        aut_only_grts = setdiff(aut_grts, both_grts)
        # Get GRTS without species
        grts_with_spc      = unique(spc_spec_totals_df$GRTS)
        grts_without_spc   = setdiff(all_grts_with_data, grts_with_spc)

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

        all_grts_spdf = rbind(man_grts_with_spc_spdf, aut_grts_with_spc_spdf, both_grts_with_spc_spdf, grts_without_spc_spdf)
        full_extent = extent(all_grts_spdf)

        # Build the grts map overlayed by this species range
        #c ('#ff8400', '#337acc', '#23992f')) %>% # orange/blue/green
        m = leaflet() %>% addTiles() %>% addPolygons(data = spc_shp, label = spc, group = 'species_range')
        if (length(man_grts_with_spc_spdf) > 0){
          extent = extent(man_grts_with_spc_spdf)
          lng_ = extent@xmin + ((extent@xmax - extent@xmin)/2)
          lat_ = extent@ymin + ((extent@ymax - extent@ymin)/2)
          m = m %>% addPolygons(data = man_grts_with_spc_spdf, color = 'black', fillOpacity = 1, fillColor = '#23992f', weight=1, opacity=1)
        }
        if (length(aut_grts_with_spc_spdf) > 0){
          extent = extent(aut_grts_with_spc_spdf)
          lng_ = extent@xmin + ((extent@xmax - extent@xmin)/2)
          lat_ = extent@ymin + ((extent@ymax - extent@ymin)/2)
          m = m %>% addPolygons(data = aut_grts_with_spc_spdf, color = 'black', fillOpacity = 1, fillColor = '#337acc', weight=1, opacity=1)
        }
        if (length(both_grts_with_spc_spdf) > 0){
          extent = extent(both_grts_with_spc_spdf)
          lng_ = extent@xmin + ((extent@xmax - extent@xmin)/2)
          lat_ = extent@ymin + ((extent@ymax - extent@ymin)/2)
          m = m %>% addPolygons(data = both_grts_with_spc_spdf, color = 'black', fillOpacity = 1, fillColor = '#ff8400', weight=1, opacity=1)
        }
        if(length(grts_without_spc_spdf) > 0){
          extent = extent(grts_without_spc_spdf)
          lng_ = extent@xmin + ((extent@xmax - extent@xmin)/2)
          lat_ = extent@ymin + ((extent@ymax - extent@ymin)/2)
          m = m %>% addPolygons(data = grts_without_spc_spdf, color = 'black', fillOpacity = 0, fillColor = 'rgb(0,0,0,0)', weight=1, opacity=1)
        }

        print ('Adding Minimap')
        m = m %>% fitBounds(full_extent@xmin, full_extent@ymin, full_extent@xmax, full_extent@ymax) %>%
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
          addLegend('bottomright', labels = c(paste0(spc, ' Automatic & Manual'), paste0(spc, ' Automatic Only'),
            paste0(spc, ' Manual Only'), paste0(spc, ' Not Found')), colors = c('#ff8400', '#337acc', '#23992f', 'rgb(0,0,0,0)'), opacity =1)

        print ('Getting a zoom point to setView for rangemap')
        # zoom_pt = rgeos::gCentroid(spc_shp)
        range_extent = extent(spc_shp)
        # Build species range map for this species
        # website for diff providers: http://leaflet-extras.github.io/leaflet-providers/preview/
        print ('Creating range map with leaflet')
        m_range = leaflet() %>% addTiles() %>%
          addPolygons(data = spc_shp, label = spc, group = 'species_range') %>%
          # setView(lng = zoom_pt@coords[,1], lat = zoom_pt@coords[,2], zoom = 3) %>%
          addLegend('bottomright',labels = paste0(spc, ' Species Range'), colors = c('blue'), opacity =1) %>%
          fitBounds(range_extent@xmin, range_extent@ymin, range_extent@xmax, range_extent@ymax)

        print ('Saving out map')
        # Save out the two maps
        out_maps_dir = paste0(out_dir, '/temps/range_maps/')
        map_out_ = paste0(out_maps_dir, spc, '_grts.png')
        mapshot(m, file = map_out_, remove_controls = c("zoomControl", "layersControl", "homeButton"))
        range_map_out_ = paste0(out_maps_dir, spc, '_range.png')
        mapshot(m_range, file = range_map_out_, remove_controls = c("zoomControl", "layersControl", "homeButton"))

        # species with range maps
        maps_grts_files = c(maps_grts_files, map_out_)
        maps_range_files = c(maps_range_files, range_map_out_)
        species_with_range = c(spc, species_with_range)
      }else {
        # No species range maps found for these species
        no_species_range = c(spc, no_species_range)
      }
    }
  }


  print ('Set Variables')
  logo_img_ = system.file("templates", "nabat_logo.png", package = "nabatr")
  circle_logo_ = system.file('templates', 'NABat_Circle_color.jpg', package = 'nabatr')
  proj_id = project_id
  project_row_df = subset(project_df, project_df$project_id == proj_id)
  title        = project_row_df$project_name
  # by           = project_row_df$owner_email
  organization = project_row_df$organization
  this_project_description = project_row_df$project_description

  # description  = project_row_df$project_description
  description = "[EXAMPLE]: PURPOSE: Bat occupancy and abundance data in Colorado is less comprehensive than most other groups of mammals in the state. The purpose of a Bureau of Land Management-Royal Gorge Field Office (RGFO) acoustic monitoring project is to produce a statistically sound dataset that will provide a baseline for bat occupancy monitoring that may be utilized as a decision making tool for effective conservation. In addition, the project will participate in the continent-wide effort to create a coordinated bat population monitoring program at a regional and range wide scale that is able to provide inferences regarding changes in distribution and abundance of bat populations. OBJECTIVES: Establish a long-term monitoring program for bats across the RGFO; Utilize a monitoring protocol that will determine a baseline occupancy of bat species across the RGFO and may be used as an index to determine changes of occupancy in the future; Incorporate local data into the continent wide NABat dataset."
  # Set Methods in 3 sections
  methods_1 = "Survey units were selected using the NABat master sampling frame, a grid-based system consisting of 10 x 10 km (100 km2) cells spanning Canada, the United States, and Mexico. The NABat master sample frame provides an ordered list of cells that is spatially balanced and randomized by utilizing the generalized random-tessellation stratified (GRTS) survey design algorithm. Using NABat's online cell selection tool, a subset of the master sampling frame was selected by defining the overall geographic scope of this project. Individual cells were then selected for survey based on their GRTS order and available resources. These 100 km2 cells serve as the focal analytical unit for NABat analyses and are a biologically appropriate grain size given the nightly range of most bat species (Loeb et al. 2015). "
  methods_2 = "Recording devices capable of detecting high-frequency bat echolocation calls were deployed at 2-4 sites within each GRTS cell selected for survey. Sites were chosen based on guidance provided in Loeb et al. (2015). Factors considered when selecting sites included land ownership, accessibility, minimizing clutter, elevation, and heterogeneity of habitats within the cell. Recording devices were deployed for 4 consecutive nights during the summer maternity season when bats are most active and most likely to be detected if present in the cell. Detectors were programmed to record automatically beginning 15 minutes prior to sunset and ending 15 minutes after sunrise. Microphones were elevated ~ 3 m from the ground and oriented in the direction of least clutter to maximize detection (Loeb et al. 2015). "
  methods_3 = "Calls files were processed using commercially-available automated identification software. Prior to species identification, non-bat files were scrubbed using a noise filter. Next, the remaining files were identified to species using a regional classifier that only considers the species whose ranges intersect the defined region. Calls that could not be identified to species were labeled either NO ID or with a general category (LowF, HighF, 25k, 40k, etc.). Due to overlap in the characteristics of some bat species' calls and the uncertainty associated with automated ID software, a subset of calls was manually vetted in accordance with Loeb et al. (2015). All call files identified as rare species were manually vetted, as were all calls from species not known to occur in the survey area. For non-rare species known to occur in the survey area, at least one call was manually vetted per point per night to confirm species presence within the survey cell and to estimate detection probability."
  # Set Summary in 2 sections
  summary_1 = "[EXAMPLE]: Survey results will be reported to relevant state biologists, USFWS Region 4, and NABat. In 2019, survey efforts were expanded to include 15 new cells and collaborative efforts with Colorado Parks and Wildlife, USFWS, and Bat Conservation International.  "
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
  # Total number of detector nights across all sites
  number_of_net_nights = dim(auto_nights_df)[1]

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
    number_of_net_nights," detector nights, and ", number_of_species_detected,
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
      man_species_names = man_species_names[!is.na(man_species_names)]
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
    auto_species_names = auto_species_names[auto_species_names != ""]
    auto_species_names = auto_species_names[!is.na(auto_species_names)]

    all_species_names = unique(c(man_species_names, auto_species_names))


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

  project_id_ = project_id
  grts_fname = as.character(subset(project_df, project_df$project_id == project_id_)$sample_frame_short)
  # Get grts_fname_df
  grts_fname_df = grts_lookup_df[grts_fname][[1]]

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
  if (grts_fname == 'Continental US'){
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

  # Table 1
  print ('Build flextable 1')
  ft1_names_list = list()
  for (name in names(grts_df_final)){
    ft1_names_list[name] = gsub("_", " ", name)
  }
  print (ft1_names_list)
  ft1 = flextable::flextable(grts_df_final)
  ft1 = flextable::set_header_labels(ft1, values = ft1_names_list)
  ft1 = flextable::height(ft1, height =.7, part = 'header')
  ft1 = flextable::width(ft1, width = 1)
  ft1 = flextable::fontsize(ft1, size = 10, part = "all")
  # Table 3
  print ('Build flextable 3')
  ft3_names_list = list()
  for (name in names(table3_df)){
    ft3_names_list[name] = gsub("_", " ", name)
  }
  print (ft3_names_list)
  ft3 = flextable::flextable(table3_df)
  ft3 = flextable::set_header_labels(ft3, values = ft3_names_list)
  ft3 = flextable::height(ft3, height =.5, part = 'header')
  ft3 = flextable::width(ft3, width =2)
  ft3 = flextable::merge_v(ft3, j = 'GRTS')
  ft3 = flextable::fontsize(ft3, size = 10, part = "all")
  ft3 = flextable::italic(ft3, j = 2)
  ft3 = flextable::hline(ft3, border = fp_border(color = "black"), part = "body")

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
  descr_fig4 = paste0("Figure 4. ",selected_year," bat activity rate (average number of bat passes per night) by NABat GRTS cell")

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
  bat_id_color_df = data.frame(colors = c('#ff8400','#337acc', '#23992f'),
    types = c('At least one manual ID/site', 'Auto ID only', 'Manual ID only'), stringsAsFactors = FALSE)
  bat_id_colors = subset(bat_id_color_df, bat_id_color_df$types %in% bat_id_type)$colors
  bat_auto_counts = all_bat_id_types$auto_count / length(grts_df_final$GRTS)
  bat_species = all_bat_id_types$species

  print ('Build plotly fig2')
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
  m_fig_4     = list(t = 50, b = 20, l = 20, r = 10, pad = 0)
  # m = list(t = 60, b = 50, l = 50, r = 15, pad = 25)

  # fig 2a
  fig2_p = plot_ly(x = bat_species, y = bat_auto_counts, type = 'bar',
    width = 850, height = 650,
    marker = list(line = list(color = 'black', width = .5)),
    color = bat_id_type, colors = bat_id_colors) %>%
    layout(xaxis = x_, yaxis = y,
      margin = m_fig_2,
      # title = list(x = .1, y = 1.4, text = 'Average Bat Activity Rate', font = f),
      title = 'Average Bat Activity Rate',
      font = leg, showlegend = TRUE, autosize=FALSE, bargap = .6,
      legend = list(x = .2, y = 1.05, orientation = 'h', font = leg))
  # fig 2b
  fig2_p_log = plot_ly(x = bat_species, y = bat_auto_counts, type = 'bar',
    width = 850, height = 650,
    marker = list(line = list(color = 'black', width = .5)),
    color = bat_id_type, colors = bat_id_colors) %>%
    layout(xaxis = x_log, yaxis = y_log,
      margin = m_fig_2_log,
      # title = list(x = .1, y = 1.1, text = 'Average Bat Activity Rate using a Logarithmic Scale', font = f),
      title = 'Average Bat Activity Rate using a Logarithmic Scale',
      font = leg, showlegend = TRUE, autosize=FALSE, bargap = .6,
      legend = list(x = .2, y = 1.05, orientation = 'h', font = leg))

  print ('Save out plotly fig2')
  # Export to a file to be used to upload into the .docx
  fig2a_f = paste0(out_dir, "/temps/fig2a.png")
  plotly::export(fig2_p, file = fig2a_f)
  # Export to a file to be used to upload into the .docx
  fig2b_f = paste0(out_dir, "/temps/fig2b.png")
  plotly::export(fig2_p_log, file = fig2b_f)


  print ('Building plotly fig3')
  fig3_counts = subset(auto_species_grts_df_w, auto_species_grts_df_w$names != 'grts_totals')$species_totals
  fig3_species = subset(auto_species_grts_df_w, auto_species_grts_df_w$names != 'grts_totals')$names
  fig3_data_df = data.frame(species = fig3_species, counts = fig3_counts, stringsAsFactors = FALSE) %>% subset(species != 'NoID')
  # col_ = colorRampPalette(c('red', '#337acc','yellow'))(length(fig3_data_df$species))
  col_ = rainbow(n = length(fig3_data_df$species))

  # fig 3
  # pie_species = plot_ly(values = fig3_data_df$counts, type = 'pie', width='100%',
  #   labels = fig3_data_df$species,
  #   showlegend=FALSE,
  #   marker = list(colors = col_,line = list(color = 'black', width = .5)),
  #   height = 1000,
  #   textinfo = 'label+value') %>%
  #   layout(title = list(x = .1, y = .9, text = 'Automatic Detection Counts', font = f), font = ll)
  # print ('Save out plotly fig3')
  # fig3_f = paste0(out_dir, "/temps/fig3.png")
  # plotly::export(pie_species, file = fig3_f)


  # Fig 4
  x = list(title = "NABat GRTS Cell", titlefont = leg)
  y = list(title = "Average No. of Bat Passes",titlefont = l)
  if ('NoID' %in% auto_species_grts_df_w$names){
    fig_4_grts = names(auto_species_grts_df_w %>% dplyr::select(-species_totals, -names))
    int_species_counts = subset(auto_species_grts_df_w, auto_species_grts_df_w$names != 'NoID' & auto_species_grts_df_w$names != 'grts_totals' )
    these_species = int_species_counts$names
    species_counts = int_species_counts %>% dplyr::select(-species_totals, -names)
    fig_4_sums = as.integer(colSums(species_counts))

  }else {
    fig_4_grts = names(auto_species_grts_df_w %>% dplyr::select(-species_totals, -names))
    int_species_counts = subset(auto_species_grts_df_w, auto_species_grts_df_w$names != 'grts_totals' )
    these_species = int_species_counts$names
    species_counts = int_species_counts %>% dplyr::select(-species_totals, -names)
    fig_4_sums = as.integer(colSums(species_counts))
  }

  fig4_p = plot_ly(x = fig_4_grts, y = fig_4_sums, type = 'bar',
    width = 850, height = 650,
    marker = list(line = list(color = 'black', width = .5)),
    color = '#337acc', colors = c('#337acc')) %>%
    layout(margin = m_fig_4, font = leg, xaxis = x, yaxis = y, showlegend = F, autosize=F, bargap = .6,
      title = 'Average Bat Calls at each GRTS',
      legend = list(x = .2, y = 1.05, orientation = 'h', font = leg))
  print ('Save out plotly fig4')
  # Export to a file to be used to upload into the .docx
  fig4_f = paste0(out_dir, "/temps/fig4.png")
  plotly::export(fig4_p, file = fig4_f)

  print ('Set bold and par style')
  # Font for title
  bold_face_map = shortcuts$fp_bold(font.size = 12)
  bold_face = shortcuts$fp_bold(font.size = 16)
  date_font = fp_text(color = 'black', font.family = 'Cambria', font.size = 12)
  par_style = fp_par(text.align = "center")
  par_style_left = fp_par(text.align = "left")
  example_font = fp_text(color = "#bfbfbf", font.size = 12, bold = FALSE,
    italic = FALSE, underlined = FALSE, font.family = "Cambria",
    vertical.align = "baseline", shading.color = "transparent")
  black_bracket = fp_text(color = "black", font.size = 12, font.family = "Cambria")
  fig6_font = fp_text(color = "black", font.size = 10, font.family = "Cambria")

  print ('Begin .docx build')
  doc = read_docx() %>%
    # Add title/header
    # 'Normal', 'heading 1', '  heading 2', 'heading 3', 'centered', 'graphic title', 'table title', 'toc 1', 'toc 2', 'Balloon Text'
    # body_add_img(src = logo_img_, width = 2, height = .75, style= 'centered', pos = 'before') %>%
    body_add_fpar(fpar(ftext('Stationary Acoustic Report', prop = bold_face), fp_p = par_style ), style = 'centered') %>%
    body_add_fpar(fpar(ftext(paste0(selected_year, ' Data'), prop = date_font), fp_p = par_style ), style = 'centered') %>%
    body_add_par(value = "", style = "centered") %>%
    body_add_fpar(fpar(ftext(title, prop = bold_face), fp_p = par_style ), style = 'centered') %>%
    # body_add_par(value = title, style = "graphic title") %>%
    body_add_par(value = "", style = "centered") %>%
    # body_add_par(value = paste0('By ', by), style = "centered") %>%
    body_add_par(value = organization, style = "centered") %>%
    body_add_par(value = date, style = "centered") %>%

    body_add_par(value = "", style = "centered") %>%
    body_add_par(value = "", style = "centered") %>%
    body_add_par(value = "", style = "centered") %>%
    body_add_par(value = "", style = "centered") %>%

    body_add_img(src = circle_logo_, width = 2.5, height = 2.5, style= 'centered') %>%


    # Add summary data for project and GRTS cells
    body_add_par(value = "", style = "centered") %>%

    body_add_break() %>%

    # Project Description
    body_add_par(value = "Project Description", style = "heading 1") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = this_project_description, style = "Normal") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_fpar(fpar(ftext(description, prop = example_font)), style = 'Normal') %>%

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
    body_add_fpar(fpar(ftext(summary_1, prop = example_font)), style = 'Normal') %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_fpar(fpar(ftext(summary_2, prop = example_font)), style = 'Normal') %>%

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
    body_add_img(src = map_out_, width = 5.7, height = 4, style= 'centered') %>%
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

    # # Figure 3
    # body_add_par(value = descr_fig3, style = "Normal") %>%
    # slip_in_img(src = fig3_f, width = 6.5, height = 7) %>%
    #
    # body_add_break() %>%

    # Figure 4
    body_add_par(value = descr_fig4, style = "Normal") %>%
    slip_in_img(src = fig4_f, width = 6.5, height = 5) %>%
    body_add_break()


    # Add species range maps
  if (range_maps){
    map_c = 0
    letters_ = myLetters(length(maps_range_files))
    for (range_m in maps_range_files){
      map_c = map_c + 1
      grts_m = maps_grts_files[map_c]
      spc_range_name = str_split(str_split(sub('\\.png$', '', range_m), 'range_maps/')[[1]][2], '_range')[[1]][1]
      spc_grts_name = str_split(str_split(sub('\\.png$', '', grts_m), 'range_maps/')[[1]][2], '_grts')[[1]][1]
      descr_fig5 = paste0("Figure 5",letters_[map_c],". Species range map for ",spc_range_name)
      descr_fig6 = paste0("Figure 6",letters_[map_c],". NABat GRTS map with the species range map overlayed(",spc_range_name,").  Green GRTS cells represent the presence of ",spc_range_name," found using only Manual detection.  Blue GRTS cells represent the presence of ",spc_range_name," found using only Automatic detection.  Orange GRTS cells represent the presence of ",spc_range_name," found using both Automatic and Manual detection.  Transparent GRTS cells represent no detections found for ",spc_range_name,".")
      # Add the maps to the doc
      doc = doc %>%
        body_add_fpar(fpar(ftext(paste0('Species:  ',spc_range_name), prop = bold_face_map), fp_p = par_style ), style = 'Normal') %>%
        body_add_fpar(fpar(ftext(descr_fig5, prop = fig6_font)), style = 'Normal') %>%
        slip_in_img(src = range_m, width = 5.7, height = 4) %>%
        body_add_par(value = "", style = "Normal") %>%
        body_add_fpar(fpar(ftext(descr_fig6, prop = fig6_font)), style = 'Normal') %>%
        body_add_par(value = "", style = "Normal") %>%
        slip_in_img(src = grts_m, width = 5.7, height = 4) %>%
        body_add_break()
    }
  }
  return(doc)
}



#' @title Build Report document in .docx file for Colony Count Data
#'
#' @description Using the outputs from get_projects(), get_project_surveys(), get_colony_bulk_counts(),
#' and () this function will create a report .docx file to an out_dir.
#'
#' @import mapview
#' @import officer
#' @import magrittr
#' @import maps
#' @import maptools
#' @import sp
#' @import flextable
#' @import lubridate
#' @import ggplot2

#' @export
#'

build_col_doc = function(out_dir,
                         file_name,
                         project_df,
                         project_id,
                         colony_bulk_df,
                         survey_table,
                         cover_photo = NULL,
                         date = format(Sys.time(), "%B %d, %Y")){

  print ('Enter Report Function')

  if (dir.exists(paste0(out_dir, '/temps/'))==FALSE){
    dir.create(paste0(out_dir, '/temps/'))
  }

  print ('Set Variables')
  logo_img_ = system.file("templates", "nabat_logo.png", package = "nabatr")
  circle_logo_ = system.file('templates', 'NABat_Circle_color.jpg', package = 'nabatr')
  proj_id = project_id
  project_row_df = subset(project_df, project_df$project_id == proj_id)
  title        = project_row_df$project_name
  # by           = project_row_df$owner_email
  organization = project_row_df$organization
  this_project_description = project_row_df$project_description

  # description  = project_row_df$project_description
  description = "[EXAMPLE]:  "

  # Methods
  methods = "[EXAMPLE]: Survey sites were chosen based on previous knowledge of winter hibernacula in the region, historical monitoring efforts, and suitability criteria outlined in Loeb et al. (2015). Because detection probability of hibernating bats is highly variable within seasons, surveys were conducted between late January and early March to maximize detection (Loeb et al. 2015). Abundance was estimated using visual counts and accompanying digital photographs. Multiple observers conducted counts in each section of the hibernacula to facilitate the estimation of detection probability and to validate species identifications."

  # Results

  # Remove NA values for winter year and species
  colony_bulk_df = colony_bulk_df %>% tidyr::drop_na(wyear, species)

  ## Set variables to be printed in results section
  spp <- unique(colony_bulk_df$species)
  species_sampled <- paste(length(spp), " species ", "(", paste(spp, collapse = ", "), ")", sep = "")
  number_of_sites <- length(unique(colony_bulk_df$site_name))
  range_winter_years <- paste(min(colony_bulk_df$wyear, na.rm = TRUE), "to", max(colony_bulk_df$wyear, na.rm = TRUE))
  number_of_grts <- length(unique(colony_bulk_df$grts_id))

  results_overview = paste0("Winter colonies for ", species_sampled, " were counted at ", number_of_sites, " sites from ", range_winter_years, ", and across ", number_of_grts, " grid cells (Table 1).")

  # Table 1. Summary survey table

  survey_table <- colony_bulk_df %>%
    dplyr::group_by(wyear, species) %>%
    dplyr::summarise(number_of_sites = length(unique(site_name))) %>%
    tidyr::spread(species, number_of_sites) %>%
    dplyr::rename(`Winter Year` = wyear)
  # Remove the spaces in the field names (breaks on website/docker)
  names(survey_table) = gsub(" ", "_", names(survey_table))

  descr_table1 = paste0("Table 1. Summary of winter colony count surveys. Number of sites surveyed for species by winter year")

  print ('Build Table 1')
  ft1 = flextable::flextable(survey_table, col_keys = names(survey_table))
  ft1 = flextable::height(ft1, height =.7, part = 'header')
  ft1 = flextable::width(ft1, width = 1)
  ft1 = flextable::fontsize(ft1, size = 10, part = "all")

  # Figure 1

  print ('Build Figure 1')

  descr_fig1 = paste0("Figure 1. Winter colony counts of bats by site and species")

  p <- colony_bulk_df %>%
    ggplot(aes(x = as.integer(wyear), y = count, color = site_name)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_line() +
    scale_y_log10() +
    facet_wrap(~species, scales = "free") +
    xlab("") +
    ylab("Count of bats") +
    theme_bw() +
    theme(panel.grid = element_blank(), strip.text = element_text(face = "italic"),
          legend.position = "bottom")


  fig1_dir <- paste0(out_dir, "/fig1.png")
  ggsave(p, filename = fig1_dir)

  # Lit Cited
  lit_cited = "Loeb, S.C., T.J. Rodhouse, L.E. Ellison, C.L. Lausen, J.D. Reichard, K.M. Irvine, T.E. Ingersoll, J.T.H. Coleman, W.E. Thogmartin, J.R. Sauer, C.M. Francis, M.L. Bayless, T.R. Stanley, and D.H. Johnson. 2015. A plan for the North American Bat Monitoring Program (NABat). General Technical Reports SRS-208. Asheville, NC: U.S. Department of Agriculture Forest Service, Southern Research Station. 112 p."

  # Remove files
  if (file.exists(paste0(out_dir, '/', file_name))){
    print (paste0('Removing ', paste0(out_dir, '/', file_name)))
    file.remove(paste0(out_dir, '/', file_name))
  }

  # Font for title
  bold_face = shortcuts$fp_bold(font.size = 16)
  par_style = fp_par(text.align = "center")
  example_font = fp_text(color = "#bfbfbf", font.size = 12, bold = FALSE,
    italic = FALSE, underlined = FALSE, font.family = "Cambria",
    vertical.align = "baseline", shading.color = "transparent")

  print ('Begin .docx build')
  doc = read_docx() %>%
    # Add title/header
    # 'Normal', 'heading 1', 'heading 2', 'heading 3', 'centered', 'graphic title', 'table title', 'toc 1', 'toc 2', 'Balloon Text'
    body_add_par(value = "", style = "centered") %>%
    body_add_fpar(fpar(ftext(title, prop = bold_face), fp_p = par_style ), style = 'centered') %>%
    # body_add_par(value = title, style = "graphic title") %>%
    body_add_par(value = "", style = "centered") %>%
    # body_add_par(value = paste0('By ', by), style = "centered") %>%
    body_add_par(value = organization, style = "centered") %>%
    body_add_par(value = date, style = "centered") %>%
    body_add_par(value = "", style = "centered") %>%
    body_add_par(value = "", style = "centered") %>%
    body_add_par(value = "", style = "centered") %>%
    body_add_par(value = "", style = "centered") %>%

    body_add_img(src = circle_logo_, width = 2.5, height = 2.5, style= 'centered') %>%

    body_add_break() %>%

    # Project Description
    body_add_par(value = "Project Description", style = "heading 1") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = this_project_description, style = "Normal") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_fpar(fpar(ftext(description, prop = example_font)), style = 'Normal') %>%

    body_add_break() %>%

    # Methods
    body_add_par(value = "Methods", style = "heading 1") %>%
    body_add_par(value = "", style = "Normal") %>%

    body_add_break() %>%

    # Results
    body_add_par(value = "Results", style = "heading 1") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = results_overview, style = "Normal") %>%

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

    # Figure 1
    body_add_par(value = descr_fig1, style = "Normal") %>%
    slip_in_img(src = fig1_dir, width = 6.5, height = 5) %>%

    body_add_break() %>%

    # Literature Cited
    body_add_par(value = "Literature Cited", style = "heading 1") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = lit_cited, style = "Normal")

  return(doc)
}


