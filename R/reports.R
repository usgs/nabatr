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
                                          project_name = NULL,
                                          project_description = NULL,
                                          file_name = 'report.html',
                                          nightly_plots_type = 'grts',
                                          survey_df = NULL,
                                          acoustic_bulk_df = NULL,
                                          nightly_observed_list = NULL){

  template  = system.file("templates", "acoustic_stationary_report.Rmd", package = "nabatr")
  nabat_png = system.file("templates", "nabat_logo.png", package = "nabatr")

  # Check to see if output_dir exists
  if (dir.exists(output_dir)){
    message(template)
    message(nabat_png)

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
      num_plots = length(split_list)
      plot_list = lapply(split_list[1],  build_grts_plot, type = 'grts')
    }

    if (nightly_plots_type == 'sites'){
      # By sites
      split_list = ordered_combined_data_ %>% split(ordered_combined_data_$site_id)
      num_plots = length(split_list)
      plot_list = lapply(split_list[1],  build_grts_plot, type = 'sites')
    }


    # Specifiy template in data directory
    message(paste0("Checking report template location: ", template))
    rmarkdown::render(input       = template,
                      output_file = paste0(output_dir, '/', file_name))
    # Return the location of the downloaded report
    return(template)
  } else{
    message('Failed to find output directory: ', output_dir)
  }
}

