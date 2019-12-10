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
#' @param username String your NABat username from https://sciencebase.usgs.gov/nabat/#/home
#' @param project_id String or Integer project id from NABat ex: 105 or '105'
#' @param output_dir String output directory to save report .html file ex: /path/to/directory
#' @param file_name (optional) String output file name ex: 'my_report.html' ex: my_report.html
#' @param survey_df (optional) Dataframe from running get_project_surveys()
#' @param acoustic_bulk_df (optional) Dataframe from running get_acoustic_bulk_wavs()
#' @param manual_nights_df (optional) Dataframe from running get_observed_nights()
#' @param auto_nights_df (optional) Dataframe from running get_observed_nights()
#' @keywords bats, NABat, GQL
#' @examples
#'
#' \dontrun{
#' get_acoustic_stationary_report(token      = 'generated-nabat-gql-token',
#'                                username   = 'NABat_Username',
#'                                output_dir = '/path/to/your/output/directory',
#'                                file_name  = 'report.html',
#'                                project_id = 'number or string of a number')
#' }
#'
#' @export
#'
get_acoustic_stationary_report = function(token,
                                          username,
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
    project_df = get_projects(token    = token,
                              username = username)
    this_project_id = project_id
    project_name = subset(project_df, project_df$project_id == this_project_id)$project_name

    # Get survey dataframe
    if (is.null(survey_df)){
      survey_df = get_project_surveys(username   = username,
        token      = token,
        project_id = project_id)
    }

    # Get stationary acoustic bulk upload format dataframe
    if (is.null(acoustic_bulk_df)){
      acoustic_bulk_df = get_acoustic_bulk_wavs(token      = token,
        username   = username,
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
#' @import mapview
#' @import officer
#' @import rJava
#' @import magrittr
#'
#'
#' @export
#'

build_ac_doc = function(out_dir,
  file_name,
  manual_species_grts_df_w,
  auto_species_grts_df_w,
  project_df,
  project_id,
  cover_photo = NULL,
  date = format(Sys.time(), "%B %d, %Y"),
  map = NULL){

  logo_img_ = system.file("templates", "nabat_logo.png", package = "nabatr")

  proj_id = project_id
  project_row_df = subset(project_df, project_df$project_id == proj_id)
  print (project_row_df)

  title        = project_row_df$project_name
  by           = project_row_df$owner_email
  organization = project_row_df$organization
  description  = project_row_df$project_description

  # Save out leaflet map as a png using mapview
  if (is.null(map)){
    m = leaflet() %>% addTiles() %>% addMarkers(lat=40, lng=-105) %>% setView(lat=40,lng=-105,zoom=6)
    map_out_ = paste0(out_dir, '/intermediate_map.png')
    mapshot(m, file = map_out_)
  } else{
    m = map
    map_out_ = paste0(out_dir, '/intermediate_map.png')
    mapshot(m, file = map_out_)
  }

  # Set Methods in 3 sections
  methods_1 = "Survey units were selected using the NABat master sampling frame, a grid-based system consisting of 10 x 10 km (100 km2) cells spanning Canada, the United States, and Mexico. The NABat master sample frame provides an ordered list of cells that is spatially balanced and randomized by utilizing the generalized random-tessellation stratified (GRTS) survey design algorithm. Using NABat's online cell selection tool, a subset of the master sampling frame was selected by defining the overall geographic scope of this project. Individual cells were then selected for survey based on their GRTS order. These 100 km2 cells serve as the focal analytical unit for NABat analyses and are a biologically appropriate grain size given the nightly range of most bat species (Loeb et al. 2015). "
  methods_2 ="Recording devices capable of detecting high-frequency bat echolocation calls were placed at 2-4 sites within each GRTS cell selected for survey. Sites were chosen based on guidance provided in Loeb et al. (2015). Factors considered when selecting sites included land ownership, accessibility, minimizing clutter, elevation, and heterogeneity of habitats within the cell. Recording devices were deployed for 4 consecutive nights during the summer maternity season, when bats are most active and most likely to be detected if present in the cell. Detectors were programmed to automatically begin recording 15 minutes prior to sunset and end recording 15 minutes after sunrise. Microphones were elevated ~ 3 m from the ground and oriented in the direction of least clutter to maximize detection (Loeb et al. (2015)). "
  methods_3 ="Calls files were processed using commercially-available automated identification software. Prior to species identification, non-bat files were scrubbed using a noise filter. Next, the remaining files were identified to species using a regional classifier that only considers the species whose ranges intersect the defined region. Calls that could not be identified to species were labeled either NO ID or with a general category (LowF, HighF, 25k, 40k, etc.). Due to overlap in the characteristics of some bat species' calls and the uncertainty associated with automated ID software, a subset of calls was manually vetted in accordance with Loeb et al. (2015). All call files identified as rare species were manually vetted, as were all calls from species not known to occur in the survey area. For non-rare species known to occur in the survey area, at least one call was manually vetted per point per night to confirm species presence within the survey cell and to estimate detection probability."

  # Set Summary in 2 sections
  summary_1 = "Ex. Survey efforts will be reported to relevant state biologists, USFWS Region 4, and NABat. In 2019, survey efforts were expanded to include 15 new cells and collaborative efforts with Colorado Parks and Wildlife, USFWS, and Bat Conservation International."
  summary_2 = "No statistically significant changes in species richness were detected between 2018 and 2019, however, there was a significant decrease in overall activity rate between the two years. Moving forward, these data will help land managers determine priority areas for bat mitigation efforts and provide baseline data to examine habitat associations that may be important for protecting species of federal and state conservation concern."

  # Lit Cited
  lit_cited = "Loeb, S.C., T.J. Rodhouse, L.E. Ellison, C.L. Lausen, J.D. Reichard, K.M. Irvine, T.E. Ingersoll, J.T.H. Coleman, W.E. Thogmartin, J.R. Sauer, C.M. Francis, M.L. Bayless, T.R. Stanley, and D.H. Johnson. 2015. A plan for the North American Bat Monitoring Program (NABat). General Technical Reports SRS-208. Asheville, NC: U.S. Department of Agriculture Forest Service, Southern Research Station. 112 p."


  # Build a plot of Manual species
  ax = list(title = 'Manual Species Detection',
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE)

  df = manual_species_grts_df_w %>% subset(names != 'grts_totals' & names != 'NoID' & names != '25k') %>% dplyr::select('names','species_totals')
  df = df[order(df$species_totals, df$species_totals),]
  df$names = factor(df$names, levels = unique(df$names)[order(df$species_totals, decreasing = TRUE)])
  row.names(df) = NULL
  n = dim(df)[1]
  color_palette = colorRampPalette(c("#29a4ff", "#ffd25c", "#8a443d"))(n)
  pm = plot_ly(x = df$names, y = df$species_totals, showlegend = FALSE, colors = color_palette, title = 'Manual Species Detection',
    text = df$species_totals, textposition = 'outside',
    textfont=list(family="sans serif",size=12,color="darkgrey")) %>%
    layout(yaxis = ax)

  plot_man_w_bar = paste0(out_dir, "/plot_man_w_bar.png")
  plotly::export(pm, file = plot_man_w_bar)

  #  Build a plot of Auto species
  ax = list(title = 'Auto Species Detection',
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE)

  df = auto_species_grts_df_w %>% subset(names != 'grts_totals' & names != 'NoID' & names != '25k') %>% dplyr::select('names','species_totals')
  df = df[order(df$species_totals, df$species_totals),]
  df$names = factor(df$names, levels = unique(df$names)[order(df$species_totals, decreasing = TRUE)])
  row.names(df) = NULL
  n = dim(df)[1]
  color_palette = colorRampPalette(c("#29a4ff", "#ffd25c", "#8a443d"))(n)
  pm = plot_ly(x = df$names, y = df$species_totals, showlegend = FALSE, colors = color_palette, title = 'Auto Species Detection',
    text = df$species_totals, textposition = 'outside',
    textfont=list(family="sans serif",size=12,color="darkgrey")) %>%
    layout(yaxis = ax)

  plot_auto_w_bar = paste0(out_dir, "/plot_auto_w_bar.png")
  plotly::export(pm, file = plot_auto_w_bar)

  # Set variables for the results text created below
  number_of_sites = ""
  number_of_cells = ""
  selected_year = ""
  number_of_bat_calls = ""
  number_of_net_nights = ""
  number_of_species_detected = ""
  low_avg_per_night = ""
  high_avg_per_night = ""
  median_activity_rate = ""
  mean_activity_rate = ""


  # Text for Results using Project Summary Data
  results_overview = paste0("A total of ", number_of_sites," sites in ", number_of_cells," NABat GRTS cells were surveyed in ", selected_year," (Figure 1, Table 1). ", number_of_bat_calls," call files were recorded over ", number_of_net_nights," net nights, and ", number_of_species_detected," species were detected (Figure 1, Table 2). Activity rate (average bat passes per night) ranged from ", low_avg_per_night," to ", high_avg_per_night,", with a median of ", median_activity_rate," and a mean of ", mean_activity_rate," (Figures 3, 4).")



  # Remove files
  if (file.exists(paste0(out_dir, '/', file_name))){
    print (paste0('Removing ', paste0(out_dir, '/', file_name)))
    file.remove(paste0(out_dir, '/', file_name))
  }

  # Font for title
  bold_face = shortcuts$fp_bold(font.size = 16)
  par_style = fp_par(text.align = "center")


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

    # slip_in_img(src = map_out_, style = "strong",
    # width = 6, height = 4, pos = "after") %>%

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
    body_add_par(value = methods_1, style = "Normal") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = methods_2, style = "Normal") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = methods_3, style = "Normal") %>%
    # body_add_par(value = "[TODO] - placeholder for text", style = "Normal") %>%
    # # Grid all selection
    # body_add_par(value = "Grid all selection", style = "heading 2") %>%
    # body_add_par(value = "[TODO] - placeholder for text", style = "Normal") %>%
    # # Detector Deployment
    # body_add_par(value = "Detector Deployment", style = "heading 2") %>%
    # body_add_par(value = "[TODO] - placeholder for text", style = "Normal") %>%
    # # Data Collection and Processing
    # body_add_par(value = "Data Collection and Processing", style = "heading 2") %>%
    # body_add_par(value = "[TODO] - placeholder for text", style = "Normal") %>%

  body_add_break() %>%

    # Results and Discussion
    body_add_par(value = "Results and Discussion", style = "heading 1") %>%
    # Sampling
    body_add_par(value = "Sampling", style = "heading 2") %>%
    body_add_par(value = "[TODO] - placeholder for text", style = "Normal") %>%
    # Bat Species Presence
    body_add_par(value = "Bat Species Presence", style = "heading 2") %>%
    body_add_par(value = "[TODO] - placeholder for text", style = "Normal") %>%
    slip_in_img(src = plot_man_w_bar, width = 6, height = 4) %>%
    slip_in_img(src = plot_auto_w_bar, width = 6, height = 4) %>%

    # Activity Rate
    body_add_par(value = "Activity Rate", style = "heading 2") %>%
    body_add_par(value = "[TODO] - placeholder for text", style = "Normal") %>%
    # Regional Coordination
    body_add_par(value = "Regional Coordination", style = "heading 2") %>%
    body_add_par(value = "[TODO] - placeholder for text", style = "Normal") %>%

    body_add_break() %>%

    # Summary
    body_add_par(value = "Summary", style = "heading 1") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = summary_1, style = "Normal") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = summary_1, style = "Normal") %>%

    body_add_break() %>%

    # Literature Cited
    body_add_par(value = "Literature Cited", style = "heading 1") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = lit_cited, style = "Normal") %>%

    print(doc, target = paste0(out_dir, '/', file_name))

  file.remove(plot_auto_w_bar)
  file.remove(plot_man_w_bar)
}
