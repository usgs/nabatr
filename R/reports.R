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
#' Using the outputs from get_projects(), get_sa_project_summary(), get_acoustic_bulk_wavs(),
#' and get_observed_nights() this function will create a report .html file to an output_dir.
#' This function can run with these outputs or without.
#' @param token String token created from get_nabat_gql_token function
#' @param project_id String or Integer project id from NABat ex: 105 or '105'
#' @param output_dir String output directory to save report .html file ex: /path/to/directory
#' @param file_name (optional) String output file name ex: 'my_report.html' ex: my_report.html
#' @param survey_df (optional) Dataframe from running get_sa_project_summary()
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
get_sa_html_report = function(token,
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
    project_df = get_projects(token = token)
    this_project_id = project_id
    project_name = subset(project_df, project_df$project_id == this_project_id)$project_name

    # Get survey dataframe
    if (is.null(survey_df)){
      survey_df = get_sa_project_summary(
        token      = token,
        project_id = project_id,
        project_df = project_df)
    }

    # Get stationary acoustic bulk upload format dataframe
    if (is.null(acoustic_bulk_df)){
      acoustic_bulk_df = get_sa_bulk_wavs(token      = token,
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



#' @title Build Stationary Acoustic Report document in .docx file
#'
#' @description Using the outputs from get_projects(), get_sa_project_summary(), get_as_bulk_wavs(),
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
#' doc_ = build_sa_doc(out_dir = '/path/to/output/dir',
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

build_sa_doc =  function(out_dir,
  file_name,
  project_df,
  project_id,
  sa_bulk_df,
  sa_survey_df,
  species_df,
  selected_year,
  nightly_observed_list = NULL,
  date = format(Sys.time(), "%B %d, %Y"),
  range_maps = TRUE){


  message ('Enter Stationary Report Function')
  # Clean the data
  sa_bulk_df = sa_bulk_df %>% clean_time_fields() %>%add_observed_nights()
  # Add selected year as 1st year
  message ('Setup for temp directories')
  if (is.null(selected_year)){
    selected_year = unique(format(as.Date(sa_bulk_df$recording_time), '%Y'))[1]
  }
  # Setup temps directory to store intermediate files
  if (dir.exists(paste0(out_dir, '/temps/'))==FALSE){
    dir.create(paste0(out_dir, '/temps/'))
  }
  if (dir.exists(paste0(out_dir, '/temps/range_maps/'))==FALSE){
    dir.create(paste0(out_dir, '/temps/range_maps/'))
  }

  message ('Getting nightly data')
  # If nightly_observed_list isn't put in through variable
  if (is.null(nightly_observed_list)){
    nightly_observed_list =  get_observed_nights(sa_bulk_df)
  }
  # Getting Manipulated data for species counts at GRTS cells
  auto_nights_df = nightly_observed_list$auto_nightly_df
  manual_nights_df  = nightly_observed_list$manual_nightly_df
  all_species_totals_l_l = get_all_species_counts_long(auto_nights_df, manual_nights_df, fil = TRUE)
  manual_species_grts_df_w   = get_species_counts_wide(manual_nights_df)$species_grts_df
  auto_species_grts_df_w   = get_species_counts_wide(auto_nights_df)$species_grts_df
  manual_species_totals_l = get_species_counts_long(manual_nights_df, filter=TRUE)
  auto_species_totals_l   = get_species_counts_long(auto_nights_df, filter=TRUE)

  message ('Setting project specific variables')
  # Set Variables
  sa_logo_img_ = system.file("templates", "nabat_logo.png", package = "nabatr")
  sa_circle_logo_ = system.file('templates', 'NABat_Circle_color.jpg', package = 'nabatr')
  sa_proj_id = project_id
  sa_project_row_df = subset(project_df, project_df$project_id == sa_proj_id)
  sa_title        = sa_project_row_df$project_name
  sa_organization = sa_project_row_df$organization
  sa_this_project_description = sa_project_row_df$project_description

  message ('Getting example text')
  # Get the example text to put into the report
  sa_examples = get_sa_examples()

  message ('Build range maps')
  # build range maps
  maps_data = get_sa_range_maps(sa_bulk_df, project_df, project_id, all_species_totals_l_l, species_df, out_dir, range_maps)

  message ('Build results text')
  # Build the results text for Stationary acoustic report
  sa_results = get_sa_results(sa_bulk_df, selected_year, species_df)

  message ('Build table 1')
  # sa_table_1
  sa_table_1 = build_sa_table_1(sa_bulk_df, project_id, project_df, species_df, selected_year)

  message ('Build table 3')
  # sa table 3
  sa_table_3 = build_sa_table_3(sa_bulk_df, selected_year, species_df)

  message ('Build figure 1')
  # Figure 1 (map)
  save_figures = TRUE
  sa_figure_1 = build_sa_figure_1(sa_bulk_df, out_dir, project_df, project_id, sa_survey_df, selected_year, save_figures)

  message('Build figure 2')
  # Figure 2a/2b
  sa_figure_2 = build_sa_figure_2(sa_bulk_df, out_dir, species_df, selected_year,
    sa_table_3, auto_species_grts_df_w, manual_species_grts_df_w, save_figures)

  message('Build figure 4')
  # Figure 4
  sa_figure_4 = build_sa_figure_4(sa_bulk_df, out_dir, species_df, selected_year, save_figures)


  message('Build final.docx document')
  ##### BUILD DOCUMENT
  # Fonts
  bold_face_map = shortcuts$fp_bold(font.size = 12)
  bold_face = shortcuts$fp_bold(font.size = 16)
  date_font = fp_text(color = 'black', font.family = 'Cambria', font.size = 12)
  par_style = fp_par(text.align = "center")
  example_font = fp_text(color = "#bfbfbf", font.size = 12, bold = FALSE,
    italic = FALSE, underlined = FALSE, font.family = "Cambria",
    vertical.align = "baseline", shading.color = "transparent")
  fig6_font = fp_text(color = "black", font.size = 10, font.family = "Cambria")

  message ('Begin .docx build')
  sa_doc = read_docx() %>%
    body_add_fpar(fpar(ftext('Stationary Acoustic Report', prop = bold_face), fp_p = par_style ), style = 'centered') %>%
    body_add_fpar(fpar(ftext(paste0(selected_year, ' Data'), prop = date_font), fp_p = par_style ), style = 'centered') %>%
    body_add_par(value = "", style = "centered") %>%
    body_add_fpar(fpar(ftext(sa_title, prop = bold_face), fp_p = par_style ), style = 'centered') %>%
    body_add_par(value = "", style = "centered") %>%
    body_add_par(value = sa_organization, style = "centered") %>%
    body_add_par(value = date, style = "centered") %>%

    body_add_par(value = "", style = "centered") %>%
    body_add_par(value = "", style = "centered") %>%
    body_add_par(value = "", style = "centered") %>%
    body_add_par(value = "", style = "centered") %>%

    body_add_img(src = sa_circle_logo_, width = 2.5, height = 2.5, style= 'centered') %>%


    # Add summary data for project and GRTS cells
    body_add_par(value = "", style = "centered") %>%

    body_add_break() %>%

    # Project Description
    body_add_par(value = "Project Description", style = "heading 1") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = sa_this_project_description, style = "Normal") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_fpar(fpar(ftext(sa_examples$sa_ex_description, prop = example_font)), style = 'Normal') %>%

    body_add_break() %>%

    # Methods
    body_add_par(value = "Methods", style = "heading 1") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = "Site Selection", style = "heading 2") %>%
    body_add_par(value = sa_examples$sa_methods_1, style = "Normal") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = "Data Collection", style = "heading 2") %>%
    body_add_par(value = sa_examples$sa_methods_2, style = "Normal") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = "Call Processing/Species Identification", style = "heading 2") %>%
    body_add_par(value = sa_examples$sa_methods_3, style = "Normal") %>%

    body_add_break() %>%

    # Results
    body_add_par(value = "Results", style = "heading 1") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = sa_results, style = "Normal") %>%

    body_add_break() %>%

    # Summary
    body_add_par(value = "Summary", style = "heading 1") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_fpar(fpar(ftext(sa_examples$sa_summary_1, prop = example_font)), style = 'Normal') %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_fpar(fpar(ftext(sa_examples$sa_summary_2, prop = example_font)), style = 'Normal') %>%

    body_add_break() %>%

    # Literature Cited
    body_add_par(value = "Literature Cited", style = "heading 1") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = sa_examples$sa_lit_cited, style = "Normal") %>%

    body_add_break() %>%

    # Table 1
    body_add_par(value = sa_table_1$description, style = "Normal") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_flextable(sa_table_1$table, align='left') %>%
    body_add_par(value = "", style = "Normal") %>%

    body_add_break() %>%

    # Table 3
    body_add_par(value = sa_table_3$description, style = "Normal") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_flextable(sa_table_3$table, align='left') %>%

    body_add_break() %>%

    # Figure 1
    body_add_par(value = sa_figure_1$description, style = "Normal") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_img(src = sa_figure_1$file, width = 5.7, height = 4, style= 'centered') %>%
    body_add_par(value = "", style = "Normal") %>%

    body_add_break() %>%

    # Figure 2a
    body_add_par(value = sa_figure_2$description_a, style = "Normal") %>%
    slip_in_img(src = sa_figure_2$file_a, width = 6.5, height = 5) %>%

    body_add_break() %>%

    # Figure 2b
    body_add_par(value = sa_figure_2$description_b, style = "Normal") %>%
    slip_in_img(src = sa_figure_2$file_b, width = 6.5, height = 5) %>%

    body_add_break() %>%

    # Figure 4
    body_add_par(value = sa_figure_4$description, style = "Normal") %>%
    slip_in_img(src = sa_figure_4$file, width = 6.5, height = 5) %>%
    body_add_break()

  message('Adding species range maps to .docx')
  # Add species range maps
  if (range_maps){
    map_c = 0
    letters_ = myLetters(length(maps_data$maps_range_files))
    for (range_m in maps_data$maps_range_files){
      map_c = map_c + 1
      grts_m = maps_data$maps_grts_files[map_c]
      spc_range_name = str_split(str_split(sub('\\.png$', '', range_m), 'range_maps/')[[1]][2], '_range')[[1]][1]
      spc_grts_name = str_split(str_split(sub('\\.png$', '', grts_m), 'range_maps/')[[1]][2], '_grts')[[1]][1]
      descr_fig5 = paste0("Figure 5",letters_[map_c],". Species range map for ",spc_range_name)
      descr_fig6 = paste0("Figure 6",letters_[map_c],". NABat GRTS map with the species range map overlayed(",spc_range_name,").  Green GRTS cells represent the presence of ",spc_range_name," found using only Manual detection.  Blue GRTS cells represent the presence of ",spc_range_name," found using only Automatic detection.  Orange GRTS cells represent the presence of ",spc_range_name," found using both Automatic and Manual detection.  Transparent GRTS cells represent no detections found for ",spc_range_name,".")
      # Add the maps to the doc
      sa_doc = sa_doc %>%
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
  return (sa_doc)
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




#' @title Build Mobile Acoustic Report document in .docx file
#'
#' @description Using the outputs from get_projects(), get_ma_project_summary(), get_ma_bulk_wavs(),
#' and get_observed_nights() this function will create a report .docx file to an out_dir.
#'
#' @import mapview
#' @import officer
#' @import magrittr
#' @import maps
#' @import maptools
#' @import sp
#' @import flextable
#' @import leaflet
#' @import rmarkdown
#' @import htmlwidgets
#' @import htmltools
#'
#' @export
#'

build_ma_doc = function(out_dir,
                        file_name,
                        project_df,
                        project_id,
                        ma_bulk_df,
                        species_df,
                        year,
                        nightly_observed_list,
                        date = format(Sys.time(), "%B %d, %Y")){

  # Setup temps directory to store intermediate files
  if (dir.exists(paste0(out_dir, '/temps/'))==FALSE){
    dir.create(paste0(out_dir, '/temps/'))
  }
  if (dir.exists(paste0(out_dir, '/temps/range_maps/'))==FALSE){
    dir.create(paste0(out_dir, '/temps/range_maps/'))
  }

  logo_img_ = system.file("templates", "nabat_logo.png", package = "nabatr")
  circle_logo_ = system.file('templates', 'NABat_Circle_color.jpg', package = 'nabatr')
  proj_id = project_id
  project_row_df = subset(project_df, project_df$project_id == proj_id)
  ma_title        = project_row_df$project_name
  ma_organization = project_row_df$organization
  ma_description = project_row_df$project_description

  print ('build results')
  # Build results text
  ma_results = get_ma_results(ma_bulk_df, species_df, year)

  print ('build examples')
  # get example text for mobile acoustic report
  ma_examples = get_ma_examples()

  print ('build table 1')
  # Build table 1
  ma_table_1 = build_ma_table_1(ma_bulk_df, project_id, project_df, species_df, year)

  print ('build table 2')
  # Build table 2
  ma_table_2 = build_ma_table_2(ma_bulk_df, species_df, year)

  print ('build table 3')
  # Build table 3
  ma_table_3 = build_ma_table_3(ma_bulk_df, nightly_observed_list, species_df, year)

  print ('build figure 1')
  # Build figure 1
  ma_figure_1 = build_ma_figure_1(ma_bulk_df, project_id, project_df, year)
  # Save out map to import into officer word doc builder later
  map_out_ = paste0(out_dir, '/temps/intermediate_map.png')
  print ('Saving map out')
  mapshot(ma_figure_1$map, file = map_out_)

  print ('build figure 2')
  # Build figure 2a/2b
  ma_figure_2 = build_ma_figure_2(ma_bulk_df, species_df, year)

  # Export to a file to be used to upload into the .docx
  fig2a_f = paste0(out_dir, "/temps/fig2a.png")
  plotly::export(ma_figure_2$figure_a, file = fig2a_f)
  # Export to a file to be used to upload into the .docx
  fig2b_f = paste0(out_dir, "/temps/fig2b.png")
  plotly::export(ma_figure_2$figure_b, file = fig2b_f)

  print ('build figure 3')
  # Build Figure 3
  ma_figure_3 = build_ma_figure_3(ma_bulk_df, species_df, year)

  # Export to a file to be used to upload into the .docx
  fig3_f = paste0(out_dir, "/temps/fig4.png")
  plotly::export(ma_figure_3$figure, file = fig3_f)


  bold_face = shortcuts$fp_bold(font.size = 16)
  date_font = fp_text(color = 'black', font.family = 'Cambria', font.size = 12)
  par_style = fp_par(text.align = "center")
  example_font = fp_text(color = "#bfbfbf", font.size = 12, bold = FALSE,
    italic = FALSE, underlined = FALSE, font.family = "Cambria",
    vertical.align = "baseline", shading.color = "transparent")

  print ('build doc')
  ma_doc = read_docx() %>%
    body_add_fpar(fpar(ftext('Mobile Acoustic Report', prop = bold_face), fp_p = par_style ), style = 'centered') %>%
    body_add_fpar(fpar(ftext(paste0(year, ' Data'), prop = date_font), fp_p = par_style ), style = 'centered') %>%
    body_add_par(value = "", style = "centered") %>%
    body_add_fpar(fpar(ftext(ma_title, prop = bold_face), fp_p = par_style ), style = 'centered') %>%
    # body_add_par(value = title, style = "graphic title") %>%
    body_add_par(value = "", style = "centered") %>%
    # body_add_par(value = paste0('By ', by), style = "centered") %>%
    body_add_par(value = ma_organization, style = "centered") %>%
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
    body_add_par(value = ma_description, style = "Normal") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_fpar(fpar(ftext(ma_examples$ma_ex_description, prop = example_font)), style = 'Normal') %>%

    body_add_break() %>%

    # Methods
    body_add_par(value = "Methods", style = "heading 1") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = "Site Selection", style = "heading 2") %>%
    body_add_par(value = ma_examples$ma_methods_1, style = "Normal") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = "Data Collection", style = "heading 2") %>%
    body_add_par(value = ma_examples$ma_methods_2, style = "Normal") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = "Call Processing/Species Identification", style = "heading 2") %>%
    body_add_par(value = ma_examples$ma_methods_3, style = "Normal") %>%

    body_add_break() %>%

    # Results
    body_add_par(value = "Results", style = "heading 1") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = ma_results, style = "Normal") %>%

    body_add_break() %>%

    # Summary
    body_add_par(value = "Summary", style = "heading 1") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_fpar(fpar(ftext(ma_examples$ma_summary_1, prop = example_font)), style = 'Normal') %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_fpar(fpar(ftext(ma_examples$ma_summary_2, prop = example_font)), style = 'Normal') %>%

    body_add_break() %>%

    # Literature Cited
    body_add_par(value = "Literature Cited", style = "heading 1") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = ma_examples$ma_lit_cited, style = "Normal") %>%

    body_add_break() %>%

    # Table 1
    body_add_par(value = ma_table_1$description, style = "Normal") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_flextable(ma_table_1$table, align='left') %>%
    body_add_par(value = "", style = "Normal") %>%

    body_add_break() %>%

    # Table 2
    body_add_par(value = ma_table_2$description, style = "Normal") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_flextable(ma_table_2$table, align='left') %>%
    body_add_par(value = "", style = "Normal") %>%

    body_add_break() %>%
    body_end_section_continuous()

  # Add table 3a/b
  auto_ids = unique(ma_bulk_df$auto_id[!is.na(ma_bulk_df$auto_id)])
  if (length(auto_ids) > 0){
    ma_doc = ma_doc %>%
      # Table 3a
      body_add_par(value = ma_table_3$description_a, style = "Normal") %>%
      body_add_par(value = "", style = "Normal") %>%
      body_add_flextable(ma_table_3$table_a, align='left') %>%
      body_add_break()
  }
  man_ids = unique(ma_bulk_df$manual_id[!is.na(ma_bulk_df$manual_id)])
  if (length(man_ids) > 0){
    ma_doc = ma_doc %>%
      # Table 3b
      body_add_par(value = ma_table_3$description_b, style = "Normal") %>%
      body_add_par(value = "", style = "Normal") %>%
      body_add_flextable(ma_table_3$table_b, align='left') %>%
      body_add_break()
  }

  ma_doc = ma_doc %>%

    # Figure 1
    body_add_par(value = ma_figure_1$description, style = "Normal") %>%
    # body_add_img(src = map_out_, width = 5.7, height = 4, style= 'centered') %>%
    body_add_img(src = map_out_, width = 8, height = 6, style= 'centered')  %>%

    # Figure 2a
    body_add_par(value = ma_figure_2$description_a, style = "Normal") %>%
    slip_in_img(src = fig2a_f, width = 7, height = 5) %>%

    body_add_break() %>%

    # Figure 2b
    body_add_par(value = ma_figure_2$description_b, style = "Normal") %>%
    slip_in_img(src = fig2b_f, width = 7, height = 5) %>%

    body_add_break() %>%

    # Figure 3
    body_add_par(value = ma_figure_3$description, style = "Normal") %>%
    slip_in_img(src = fig3_f, width = 7, height = 5) %>%
    body_end_section_landscape()

  return (ma_doc)
}







































