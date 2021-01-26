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

#' @title Build a Wide Species count Dataframe
#'
#' @import dplyr
#'
#' @description Uses one of the outputs from get_observed_nights() to calculate totals for
#' number of species present at a GRTS cell or a site ID and then returns a dataframe.
#'
#' @param nightly_df Dataframe acoustic bulk dataframe from output of
#' get_observed_nights(). Note: must run seperately for MANUAL and AUTOMATIC
#' nights dataframes.
#' @keywords species, bats, NABat
#' @examples
#'
#' \dontrun{
#'
#' # MANUAL
#' manual_species_totals_w_   = get_species_counts_wide(manual_nights_df_)
#' # --- totals at sites
#' manual_species_sites_df_w_ = manual_species_totals_w_$species_site_id_df
#' # --- totals at GRTS cell id
#' manual_species_grts_df_w_  = manual_species_totals_w_$species_grts_df
#'
#' # AUTOMATIC
#' auto_species_totals_w_   = get_species_counts_wide(auto_nights_df_)
#' # --- totals at sites
#' auto_species_sites_df_w_ = auto_species_totals_w_$species_site_id_df
#' # --- totals at GRTS cell id
#' auto_species_grts_df_w_  = auto_species_totals_w_$species_grts_df
#'
#' }
#'
#' @export
#'
get_species_counts_wide = function(nightly_df){

  # Return NULL if no manual checked species found
  check_values = dplyr::select(nightly_df, -c('GRTS', 'site_id', 'site_name',
    'observed_night','type', 'project_id'))
  if (length(unique(colSums(check_values))) == 1){
    return(list('species_site_id_df'   = NULL,
      'species_grts_df' = NULL))
  }

  # Select only the wav file count data to run sums on
  species_grts_df = dplyr::select(nightly_df, -c('GRTS', 'site_id', 'site_name',
    'observed_night','type', 'project_id'))


  # All of the species found at project level
  spc_names_df = names(species_grts_df)
  all_species_at_project = spc_names_df[colSums(species_grts_df) > 0]

  # Extract all site and GRTS IDs
  site_ids = unique(nightly_df$site_id)
  grts_ids = unique(nightly_df$GRTS)

  # ------- This is for Site IDs ex: (132209_SW)
  # loop through Site Ids and build a summary count at each site for this project
  species_present_site_id_sums_df = data.frame()
  for (site_id_ in site_ids){
    # Dataframe of observed nights at this Site Id
    site_id_data            = subset(nightly_df, nightly_df$site_id == site_id_)
    # Site Id data to be used for summing up species counts
    site_id_species_grts_df = dplyr::select(site_id_data,
      -c('GRTS', 'site_id', 'site_name', 'observed_night','type', 'project_id'))

    # Select all species in project
    species_present_site_id_df = dplyr::select(site_id_species_grts_df,
      all_species_at_project)
    # Get species names
    species_present_site_id_names = names(species_present_site_id_df)
    # Get species counts
    col_sums = colSums(species_present_site_id_df)
    names(col_sums) = c()

    # Build species counts dataframe
    if (dim(species_present_site_id_sums_df)[1] == 0){
      # Create dataframe using species names at site and their sums
      species_present_site_id_sums_df =
        data.frame(names = species_present_site_id_names, counts = col_sums)
      names(species_present_site_id_sums_df)[names(species_present_site_id_sums_df) ==
          'counts'] = site_id_
    }else{
      species_present_site_id_sums_df['counts'] = col_sums
      names(species_present_site_id_sums_df)[names(species_present_site_id_sums_df) ==
          'counts'] = site_id_
    }
  }


  # Add site totals of all species
  totals_row = c('names'=NA, colSums(dplyr::select(species_present_site_id_sums_df, -c('names'))))
  species_present_site_id_sums_df = rbind(species_present_site_id_sums_df, totals_row)
  species_present_site_id_sums_df$names = as.character(species_present_site_id_sums_df$names)
  species_present_site_id_sums_df$names[is.na(species_present_site_id_sums_df$names)] = 'site_totals'
  # Add species totals for project
  species_present_site_id_sums_df['species_totals'] =
    rowSums(dplyr::select(species_present_site_id_sums_df, -c('names')))
  # Display the data
  species_present_site_id_sums_df


  # ------- This is for GRTS Cells as a whole ex: (132209)
  # loop through GRTS Cell and build a summary count at each site for this project
  species_present_grts_sums_df = data.frame()
  for (grts_ in grts_ids){

    # Dataframe of observed nights at this Site Id
    site_id_data            = subset(nightly_df, nightly_df$GRTS == grts_)
    # Site Id data to be used for summing up species counts
    site_id_species_grts_df = dplyr::select(site_id_data,
      -c('GRTS', 'site_id', 'site_name', 'observed_night', 'type', 'project_id'))

    # Select all species in project
    species_present_site_id_df = dplyr::select(site_id_species_grts_df,
      all_species_at_project)
    # Get species names
    species_present_site_id_names = names(species_present_site_id_df)
    # Get species counts
    col_sums = colSums(species_present_site_id_df)
    names(col_sums) = c()

    # Build species counts dataframe
    if (dim(species_present_grts_sums_df)[1] == 0){
      # Create dataframe using species names at site and their sums
      species_present_grts_sums_df =
        data.frame(names = species_present_site_id_names, counts = col_sums)
      names(species_present_grts_sums_df)[names(species_present_grts_sums_df) ==
          'counts'] = grts_
    }else{
      species_present_grts_sums_df['counts'] = col_sums
      names(species_present_grts_sums_df)[names(species_present_grts_sums_df) ==
          'counts'] = grts_
    }
    # Display the data
    species_present_grts_sums_df
  }
  # Add site totals of all species
  totals_row = c('names'=NA, colSums(dplyr::select(species_present_grts_sums_df,
    -c('names'))))
  species_present_grts_sums_df = rbind(species_present_grts_sums_df, totals_row)
  species_present_grts_sums_df$names = as.character(species_present_grts_sums_df$names)
  species_present_grts_sums_df$names[is.na(species_present_grts_sums_df$names)] = 'grts_totals'
  # Add species totals for project
  species_present_grts_sums_df['species_totals'] =
    rowSums(dplyr::select(species_present_grts_sums_df, -c('names')))

  return(list('species_site_id_df'   = species_present_site_id_sums_df,
    'species_grts_df' = species_present_grts_sums_df))

}


#' @title Build a Long Species count Dataframe at GRTS level
#'
#' @import dplyr
#'
#' @description
#' Extracting unique species found for manual or automatic nights acoustic data
#' @param nights_df Dataframe acoustic bulk dataframe from output of get_observed_nights().
#' @param filter Boolean filters out species that don't have any counts for that species
#'
#'
#' \dontrun{
#'
#' # Individual Manual or Auto
#' manual_species_totals_l_ = get_species_counts_long(manual_nights_df_, filter=TRUE)
#' auto_species_totals_l_   = get_species_counts_long(auto_nights_df_, filter=TRUE)
#'
#' }
#'
#' @export
#'
get_species_counts_long = function(nights_df, filter = FALSE){
  # Money pipe for manual or auto nights_df to get sums for each year at each site
  proj_all_counts = nights_df %>%
    dplyr::mutate(year = as.integer(substring(nights_df$observed_night,1,4))) %>%
    group_by(GRTS, year) %>%
    transmute_at(.vars = names(dplyr::select(nights_df,
      -c('GRTS', 'site_id', 'site_name', 'observed_night', 'type', 'project_id'))), .funs = sum) %>%
    distinct() %>% ungroup() %>%
    merge(dplyr::select(nights_df, GRTS, type, project_id)) %>%
    move_col(type, year) %>%
    move_col(project_id, year) %>%
    move_col(GRTS, type) %>% distinct() %>%
    mutate(GRTS = as.integer(GRTS)) %>%
    mutate(project_id = as.integer(project_id))

  # Filter dataframe and only grab columns with counts in them if filter = TRUE
  if (filter == TRUE){
    species_grts_df_ = proj_all_counts %>% dplyr::select(-c('GRTS', 'type', 'project_id','year'))
    # Filter out all species columns that don't have any data!
    spc_names_df = names(species_grts_df_)
    species_in_project = spc_names_df[colSums(species_grts_df_) > 0]
    species_in_project_df  = species_grts_df_ %>% dplyr::select( species_in_project ) %>%
      mutate(GRTS=proj_all_counts$GRTS, type=proj_all_counts$type,
        project_id=proj_all_counts$project_id, year=proj_all_counts$year) %>%
      move_col(year, ref = 1) %>% move_col(project_id, ref = 1) %>% move_col(type, ref = 1) %>%
      move_col(GRTS, ref = 1)
    return(species_in_project_df)
  } else{
    return(proj_all_counts)
  }
}

#' @title Build a Long Species count Dataframe at GRTS level for Manual and Automatic
#'
#' @description
#' Extracting unique species found for manual or automatic nights acoustic data
#'
#' \dontrun{
#'
#' # Both Manual and Auto
#' all_species_totals_l_l_ = get_all_species_counts_long(auto_nights_df_, manual_nights_df_, fil = TRUE)
#'
#'
#' }
#'
#' @export
#'
get_all_species_counts_long = function(auto_nights_df, manual_nights_df, fil = TRUE){
  # Filter out all species columns that don't have any data if filter == TRUE

  # MANUAL
  manual_species_totals_l = get_species_counts_long(manual_nights_df, filter=fil)
  # AUTOMATIC
  auto_species_totals_l   = get_species_counts_long(auto_nights_df, filter=fil)

  # COMBINED (in order to combine them we need to make sure columns matchup)
  auto_diff = setdiff(names(auto_species_totals_l), names(manual_species_totals_l))
  manual_diff = setdiff(names(manual_species_totals_l), names(auto_species_totals_l))

  # Add fields manual doesn't have (from auto)
  for (missing_field in auto_diff){
    manual_species_totals_l[,missing_field] = as.integer(0)
  }
  # Add fields auto doesn't have (from manua)
  for (missing_field in manual_diff){
    auto_species_totals_l[,missing_field] = as.integer(0)
  }

  all_species_totals_l_l = rbind(auto_species_totals_l, manual_species_totals_l)
  return(all_species_totals_l_l)
}





