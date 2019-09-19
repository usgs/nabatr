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

#' @title Observed nightly observations for NABat stationary acoustic data
#'
#' @description Creates
#'
#' @param acoustic_bulk_df Dataframe acoustic bulk dataframe from output of get_acoustic_bulk()
#' @keywords species, bats, NABat
#' @examples
#'
#' \dontrun{
#' nightly_observed_list = nabatr::get_observed_nights(acoustic_bulk_df)
#' manual_nights_df = nightly_observed_list$auto_nightly_df
#' auto_nights_df   = nightly_observed_list$manual_nightly_df
#' }
#'
#' @export
#'
get_observed_nights = function(acoustic_bulk_df){
  species_df  = bats_df
  species     = species_df$species_code
  surveys_    = unique(acoustic_bulk_df$survey_id)

  auto_project_data = data.frame()
  manual_project_data = data.frame()
  for (survey_ in surveys_){
    ex_grts_df = subset(acoustic_bulk_df, acoustic_bulk_df$survey_id == survey_)
    ex_grts_df[,c("grts_cell_id", "survey_start_time", "recording_time", "survey_end_time", "survey_id", "location_name",
                  "audio_recording_name", "auto_id", "manual_id")]

    GRTS_id = unique(ex_grts_df$grts_cell_id)

    print (unique(ex_grts_df$auto_id))

    # Add columns auto_species and manual_species
    species_df_ = species_df[, c("id", "species_code")]
    # Auto column
    names(species_df_)[names(species_df_) == 'id'] = 'auto_id'
    ex_grts_df = plyr::join(ex_grts_df, species_df_, by = c('auto_id'), type = "left")
    names(ex_grts_df)[names(ex_grts_df) == 'species_code'] = 'auto_species'
    # Manual column
    names(species_df_)[names(species_df_) == 'auto_id'] = 'manual_id'
    ex_grts_df = plyr::join(ex_grts_df, species_df_, by = c('manual_id'), type = "left")
    names(ex_grts_df)[names(ex_grts_df) == 'species_code'] = 'manual_species'

    # Replace Unconfirmed with NoID in Auto Id field
    ex_grts_df$auto_species[is.na(ex_grts_df$auto_species)] = "NoID"
    ex_grts_df$manual_species[is.na(ex_grts_df$manual_species)] = "NoID"
    ex_grts_df$auto_id[is.na(ex_grts_df$auto_id)] = "NoID"
    ex_grts_df$manual_id[is.na(ex_grts_df$manual_id)] = "NoID"

    # Remove rows with no wave files
    ex_grts_df = subset(ex_grts_df, !is.na(ex_grts_df$recording_time))

    # Edit dates
    ex_grts_df$survey_start_time = as.POSIXct(ex_grts_df$survey_start_time, tryFormats = c('%Y-%m-%dT%H:%M:%S', '%m/%d/%y %H:%M','%Y-%m-%dT%H:%M:%S+%Z'))
    ex_grts_df$survey_end_time = as.POSIXct(ex_grts_df$survey_end_time, tryFormats = c('%Y-%m-%dT%H:%M:%S', '%m/%d/%y %H:%M','%Y-%m-%dT%H:%M:%S+%Z'))
    ex_grts_df$recording_time = as.Date(ex_grts_df$recording_time)
    survey_dates = unique(ex_grts_df$recording_time)

    for (x in c(1:length(survey_dates))){
      date = survey_dates[x]
      if (length(date) != 0){
        night_data = subset(ex_grts_df, ex_grts_df$recording_time == date)
        locations = unique(night_data$location_name)
        manual_night_row = data.frame(GRTS = GRTS_id)
        auto_night_row   = data.frame(GRTS = GRTS_id)
        # Adding number of species at each night and adding it to the row
        for (location in locations){
          night_data_at_location = subset(night_data, night_data$location_name == location)
          manual_night_row$site_id = paste0(GRTS_id,'_',location)
          manual_night_row$site_name = location
          manual_night_row$observed_night = date
          auto_night_row$site_id = paste0(GRTS_id,'_',location)
          auto_night_row$site_name = location
          auto_night_row$observed_night = date
          for (s in species){
            auto_species_count   = dim(subset(night_data_at_location, night_data_at_location$auto_species == s))[1]
            manual_species_count = dim(subset(night_data_at_location, night_data_at_location$manual_species == s))[1]
            auto_night_row[,s]   = auto_species_count
            manual_night_row[,s] = manual_species_count
          }

          # Bind this row for this date to final dataframe
          if (dim(auto_project_data)[1]==0){
            auto_project_data = auto_night_row
            manual_project_data = manual_night_row
          }else{
            auto_project_data = rbind(auto_project_data, auto_night_row)
            manual_project_data = rbind(manual_project_data, manual_night_row)
          }
        }
      }
    }
  }
  return(list('auto_nightly_df'   = auto_project_data,
              'manual_nightly_df' = manual_project_data))
}
