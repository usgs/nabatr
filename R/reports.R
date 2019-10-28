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
#' @import kable
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
                                          file_name = 'report.html',
                                          survey_df = NULL,
                                          acoustic_bulk_df = NULL,
                                          manual_nights_df = NULL,
                                          auto_nights_df = NULL){

  template  = system.file("templates", "acoustic_stationary_report.Rmd", package = "nabatr")
  nabat_png = system.file("templates", "nabat_logo.png", package = "nabatr")

  message(template)
  message(nabat_png)

  # Get survey dataframe
  survey_df = get_project_surveys(username   = username,
                                  token      = token,
                                  project_id = project_id)

  # Get stationary acoustic bulk upload format dataframe
  acoustic_bulk_df = get_acoustic_bulk_wavs(token      = token,
                                            username   = username,
                                            survey_df  = survey_df,
                                            project_id = project_id)

  # Get Acoustic stationary acoustic bulk dataframe
  nightly_observed_list = get_observed_nights(acoustic_bulk_df)
  manual_nights_df      = nightly_observed_list$auto_nightly_df
  auto_nights_df        = nightly_observed_list$manual_nightly_df

  if (dim(manual_nights_df)[1] == 0  && dim(auto_nights_df)[1] == 0){
    message('Error, this project has no data to build a report with')
    return('Failed')
  }

  # Build leaflet map
  grts_map = get_grts_leaflet_map(all_grts       = unique(survey_df$grts_cell_id),
                                  grts_with_data = unique(auto_nights_df$GRTS))

  # Specifiy template in data directory
  message(paste0("Checking report template location: ", template))
  rmarkdown::render(input       = template,
                    output_file = paste0(output_dir, '/', file_name))
  # Return the location of the downloaded report
  return(template)
}

