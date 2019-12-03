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
                        output_file = paste0(output_dir, '/', file_name),
                        output_format = "html_document")
    } else if (output_type == 'pdf'){
      rmarkdown::render(input       = template,
                        output_file = paste0(output_dir, '/', file_name),
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

build_ac_doc = function(out_dir_, file_name_, proj_sum_, manual_species_grts_df_w, auto_species_grts_df_w,
  title_ = "Implementing NABat on National Wildlife Refuges in the Pacific Northwest Region (R1): Results from 2016 - 2018",
  by_    = "Jenny Barnett",
  member_role_org_ = "Zone Inventory and Monitoring Biologist",
  date_  = format(Sys.time(), "%B %d, %Y"),
  map = NULL){

  logo_img_ = system.file("templates", "nabat_logo.png", package = "nabatr")
  # Save out leaflet map as a png using mapview
  if (is.null(map)){
    m = leaflet() %>% addTiles() %>% addMarkers(lat=40, lng=-105) %>% setView(lat=40,lng=-105,zoom=6)
    map_out_ = paste0(out_dir_, '/intermediate_map.png')
    mapshot(m, file = map_out_)
  } else{
    m = map
    map_out_ = paste0(out_dir_, '/intermediate_map.png')
    mapshot(m, file = map_out_)
  }


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

  plot_man_w_bar = paste0(out_dir_, "/plot_man_w_bar.png")
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

  plot_auto_w_bar = paste0(out_dir_, "/plot_auto_w_bar.png")
  plotly::export(pm, file = plot_auto_w_bar)


  # Remove files
  if (file.exists(paste0(out_dir_, '/', file_name_))){
    print (paste0('Removing ', paste0(out_dir_, '/', file_name_)))
    file.remove(paste0(out_dir_, '/', file_name_))
  }

  # Font for title
  bold_face = shortcuts$fp_bold(font.size = 16)
  par_style = fp_par(text.align = "center")


  doc = read_docx() %>%

    # Add title/header
    # 'Normal', 'heading 1', 'heading 2', 'heading 3', 'centered', 'graphic title', 'table title', 'toc 1', 'toc 2', 'Balloon Text'
    slip_in_img(src = logo_img_, width = 2, height = .75) %>%
    body_add_par(value = "", style = "centered") %>%
    body_add_fpar(fpar( ftext(title_, prop = bold_face), fp_p = par_style ), style = 'centered') %>%
    # body_add_par(value = title_, style = "graphic title") %>%
    body_add_par(value = "", style = "centered") %>%
    body_add_par(value = paste0('By ', by_), style = "centered") %>%
    body_add_par(value = member_role_org_, style = "centered") %>%
    body_add_par(value = date_, style = "centered") %>%

    # Add Map
    body_add_par(value = "", style = "centered") %>%
    body_add_par(value = "", style = "centered") %>%
    body_add_par(value = "GRTS Map", style = "table title") %>%
    body_add_par(value = "", style = "centered") %>%
    body_add_par(value = "", style = "centered") %>%
    slip_in_img(src = map_out_, style = "strong",
      width = 6, height = 4, pos = "after") %>%

    # Add summary data for project and GRTS cells
    body_add_par(value = "", style = "centered") %>%
    body_add_par(value = proj_sum_, style = "Normal") %>%

    body_add_break() %>%

    # Project Description
    body_add_par(value = "Project Description", style = "heading 1") %>%
    body_add_par(value = "", style = "centered") %>%
    body_add_par(value = "[TODO] - placeholder for text", style = "Normal") %>%

    body_add_break() %>%

    # Methods
    body_add_par(value = "Methods", style = "heading 1") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = "[TODO] - placeholder for text", style = "Normal") %>%
    # Grid all selection
    body_add_par(value = "Grid all selection", style = "heading 2") %>%
    body_add_par(value = "[TODO] - placeholder for text", style = "Normal") %>%
    # Detector Deployment
    body_add_par(value = "Detector Deployment", style = "heading 2") %>%
    body_add_par(value = "[TODO] - placeholder for text", style = "Normal") %>%

    # Data Collection and Processing
    body_add_par(value = "Data Collection and Processing", style = "heading 2") %>%
    body_add_par(value = "[TODO] - placeholder for text", style = "Normal") %>%

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
    body_add_par(value = "[TODO] - placeholder for text", style = "Normal") %>%

    body_add_break() %>%

    # Literature Cited
    body_add_par(value = "Literature Cited", style = "heading 1") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = "[TODO] - placeholder for text", style = "Normal") %>%

    print(doc, target = paste0(out_dir_, '/', file_name_))


  file.remove(plot_auto_w_bar)
  file.remove(plot_man_w_bar)
}

