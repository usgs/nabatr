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
# FILE DESCRIPTION:  This file contains functions to help build the Colony
# Count report build_col_doc()
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


#' @title Get Colony Count Species info
#'
#' @description Extracts Get all of the species found within these
#' Colony Count data
#' format = 'df' | 'vector' | 'vectorNoId' (remove NoID from list)
#'
#' @param cc_bulkd_df Dataframe colony count bulk data
#' @param species_df Dataframe species nabat lookup dataframe
#' @param format String format of data to return 'df' = dataframe of
#' the species from this data, 'vector' = a string vector of all
#' of the species from this data, 'vectorNoId' = a strin vector of all
#' of the species but exclude 'NoID'
#'
#' @export

get_cc_species = function(
  cc_bulk_df,
  species_df,
  format = 'df'){

  species_ids = subset(cc_bulk_df, !is.na(cc_bulk_df$species_id))$species_id
  # Colony Count species
  cc_project_species = (data.frame(id = unique(species_ids), stringsAsFactors = FALSE) %>%
      dplyr::left_join(species_df, by = 'id'))
  cc_species_found = subset(cc_project_species, cc_project_species$bat_call) %>%
      dplyr::mutate(detection_type = 'colony_count')
  cc_species_detected_wav = subset(cc_bulk_df, cc_bulk_df$species_id %in% cc_species_found$id)

  final_species = unique(cc_species_found$species_code)

  # Return data based on format
  if (format == 'df'){
    return(cc_species_found)
  }else if (format == 'vector'){
    return(final_species)
  }else if (format == 'vectorNoId'){
    return(final_species[final_species != 'NoID'])
  }
}



#' @title Get Colony Count Examples
#'
#' @description Returns a list of some preset colony count report examples
#'
#' @export

get_cc_examples = function(){
  # Description
  cc_description = "[EXAMPLE]:  "
  # Methods
  cc_methods = "[EXAMPLE]: Survey sites were chosen based on previous knowledge of winter hibernacula
 in the region, historical monitoring efforts, and suitability criteria outlined in Loeb et al. (2015).
 Because detection probability of hibernating bats is highly variable within seasons, surveys were
 conducted between late January and early March to maximize detection (Loeb et al. 2015). Abundance was
 estimated using visual counts and accompanying digital photographs. Multiple observers conducted counts
 in each section of the hibernacula to facilitate the estimation of detection probability and to validate
 species identifications."
  # Lit Cited
  cc_lit_cited = "Loeb, S.C., T.J. Rodhouse, L.E. Ellison, C.L. Lausen, J.D. Reichard, K.M. Irvine,
 T.E. Ingersoll, J.T.H. Coleman, W.E. Thogmartin, J.R. Sauer, C.M. Francis, M.L. Bayless, T.R. Stanley,
 and D.H. Johnson. 2015. A plan for the North American Bat Monitoring Program (NABat). General Technical
 Reports SRS-208. Asheville, NC: U.S. Department of Agriculture Forest Service, Southern Research
 Station. 112 p."

  return(list(description = cc_description,
              methods_1 = cc_methods,
              lit_cited = cc_lit_cited))
}



#' @title Get Colony Count Results
#'
#' @description Returns compiled results for this project's Colony Count data
#'
#' @param cc_bulkd_df Dataframe colony count bulk data
#'
#' @export

get_cc_results = function(
  cc_bulk_df){

  # Remove NA values for winter year and species
  cc_bulk_df = cc_bulk_df %>% tidyr::drop_na(wyear, species_code)

  ## Set variables to be printed in results section
  spp = unique(cc_bulk_df$species_code)
  species_sampled = paste(length(spp), " species ", "(",
                          paste(spp, collapse = ", "), ")", sep = "")
  number_of_sites = length(unique(cc_bulk_df$site_name))
  range_winter_years = paste(min(cc_bulk_df$wyear, na.rm = TRUE), "to",
                              max(cc_bulk_df$wyear, na.rm = TRUE))
  number_of_grts = length(unique(cc_bulk_df$grts_id))

  results_overview = paste0("Winter colonies for ",
    species_sampled, " were counted at ",
    number_of_sites, " sites from ", range_winter_years, ", and across ",
    number_of_grts, " grid cells (Table 1).")
}



#' @title Build Colony Count Table 1
#'
#' @description Summary of winter colony count surveys
#'
#' @param cc_bulkd_df Dataframe colony count bulk data
#' @param noid Boolean Whether or not to include NoID in table
#'
#' @export

build_cc_table_1 = function(
  cc_bulk_df,
  noid = TRUE){

  if (!noid){
    cc_bulk_df = subset(cc_bulk_df, !cc_bulk_df$species_code == 'NoID')
  }
  num_species = length(unique(cc_bulk_df$species_code))
  if (num_species < 9){
    width_ = 1
    font_  = 10
  }else {
    width_ = .7
    font_  = 8
  }
  survey_table = cc_bulk_df %>%
    dplyr::group_by(wyear, species_code) %>%
    dplyr::summarise(number_of_sites = length(unique(site_name))) %>%
    tidyr::drop_na() %>%
    tidyr::spread(species_code, number_of_sites) %>%
    dplyr::rename(`Winter Year` = wyear)

  cc_descr_table1 = paste0("Table 1. Summary of winter colony count surveys.
 Number of sites surveyed for species by winter year")

  big_border = fp_border(color="grey", width = 2)

  cc_ft1 = flextable::flextable(survey_table, col_keys = names(survey_table))
  cc_ft1 = flextable::border_remove(x = cc_ft1)
  cc_ft1 = flextable::bold(cc_ft1, part = "header")
  cc_ft1 = flextable::height(cc_ft1, height =.5, part = 'header')
  cc_ft1 = flextable::width(cc_ft1, width = width_)
  cc_ft1 = flextable::fontsize(cc_ft1, size = font_, part = "all")
  cc_ft1 = flextable::border(cc_ft1,
    border.top = fp_border(color = 'grey', width = 1),
    border.right = fp_border(color = 'grey', width = 1),
    border.left = fp_border(color = 'grey', width = 1),
    border.bottom = fp_border(color = 'grey', width = 1))
  cc_ft1 = flextable::align(cc_ft1, align = 'center', part = 'all')
  cc_ft1 = border_outer(cc_ft1, part="all", border = big_border )

  return (list(table = cc_ft1, description = cc_descr_table1))
}



#' @title Build Colony count table 2
#'
#' @description Number of species found dead
#' and alive at each GRTS cell
#'
#' @param cc_bulkd_df Dataframe colony count bulk data
#'
#' @export

build_cc_table_2 = function(
  cc_bulk_df){

  # Build description
  cc_descr_table_2 = 'Table 2. Number of species dead/alive found at each GRTS Cell.'

  cc_df_2 = cc_bulk_df %>%
    dplyr::select(site_id, site_name, site_type, wyear,
                  species_code, count_dead, count_alive) %>%
    dplyr::mutate(count_dead = ifelse(is.na(count_dead), 0, count_dead)) %>%
    dplyr::group_by(site_id, wyear, species_code) %>%
    dplyr::mutate(count_dead = sum(count_dead)) %>%
    dplyr::mutate(count_alive = sum(count_alive)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(site = ifelse(is.na(site_name), as.character(site_id), site_name)) %>%
    dplyr::mutate(site_type = ifelse(is.na(site_type), 'Not Found', site_type)) %>%
    dplyr::ungroup() %>%
    dplyr::select(site, site_type, wyear, species_code, count_dead, count_alive) %>%
    dplyr::arrange(site, wyear, species_code) %>%
    dplyr::rename(`Site` = site,
      `Site Type` = site_type,
      `Winter Year` = wyear,
      `Dead Count` = count_dead,
      `Alive Count` = count_alive,
      `Species Code` = species_code)

  big_border = fp_border(color="grey", width = 2)

  cc_ft2 = flextable::flextable(cc_df_2)
  cc_ft2 = flextable::border_remove(x = cc_ft2)
  cc_ft2 = flextable::fontsize(cc_ft2, part = "header", size = 12)
  cc_ft2 = flextable::bold(cc_ft2, part = "header")
  cc_ft2 = flextable::set_header_labels(cc_ft2, values = names(cc_df_2))
  cc_ft2 = flextable::height(cc_ft2, height =.5, part = 'header')
  cc_ft2 = flextable::width(cc_ft2, width = .9)
  cc_ft2 = flextable::width(cc_ft2, width = 1.3, j = 1)
  cc_ft2 = flextable::fontsize(cc_ft2, size = 10, part = "all")
  cc_ft2 = flextable::merge_v(cc_ft2, j = 1)
  cc_ft2 = flextable::merge_v(cc_ft2, j = 2)
  cc_ft2 = flextable::merge_v(cc_ft2, j = 3)

  cc_ft2 = flextable::border(cc_ft2, j = c(1,2),
    border.top = fp_border(color = 'grey', width = 1),
    border.right = fp_border(color = 'grey', width = 1),
    border.left = fp_border(color = 'grey', width = 1))

  cc_ft2 = flextable::border(cc_ft2, j = 3,
    border.top = fp_border(color = 'grey', width = 1),
    border.right = fp_border(color = 'grey', width = 1))
  cc_ft2 = flextable::border(cc_ft2, j = 4:6,
    border.top = fp_border(color = 'grey', width = 1),
    border.bottom = fp_border(color = 'grey', width = 1))
  cc_ft2 = flextable::border(cc_ft2, j = 6,
    border.right = fp_border(color = 'grey', width = 1))

  cc_ft2 = flextable::align(cc_ft2, align = 'center', part = 'all')
  cc_ft2 = border_outer(cc_ft2, part="all", border = big_border )

  return(list(table = cc_ft2, description = cc_descr_table_2, df = cc_df_2))
}



#' @title Build Colony Count Figure 1
#'
#' @description Winter Colony counts of bats by site and species
#'
#' @param cc_bulkd_df Dataframe colony count bulk data
#' @param out_dir String output directory. Make sure it doesn't
#' end with '/'
#' @save_bool Boolean Whether or not to save the figure out
#'
#' @export

build_cc_figure_1 = function(
  cc_bulk_df,
  out_dir,
  save_bool = TRUE){

  cc_sites = unique((cc_bulk_df %>%
      dplyr::arrange(site_name, wyear, species_code))$site_id)
  num_sites =length(cc_sites)
  figure_number = '1'
  fig_files = c()
  descriptions = c()
  # figures = list()
  limit = 8

  if (num_sites > limit){
    num_figures = ceiling(num_sites/limit)
    letters = myLetters(num_figures)
    for (num in c(1:num_figures)){
      fig_name = paste0(figure_number,letters[num])
      start_ = 1 + (num-1)*limit
      if (num == num_figures){
        stop_  = num_sites
      }else{
        stop_  = num*limit
      }
      site_ids = cc_sites[start_:stop_]
      figure_data = subset(cc_bulk_df, cc_bulk_df$site_id %in% site_ids)

      figure_data = figure_data %>%
        dplyr::mutate(site_id = ifelse(is.na(site_name), as.character(site_id), site_name))

      cc_figure1 = figure_data %>%
        dplyr::select(species_code, site_id, count_alive, wyear) %>%
        dplyr::mutate(wyear = as.Date(paste(wyear, 1, 1, sep = "-"))) %>%
        dplyr::mutate(site_id = as.character(site_id)) %>%
        dplyr::group_by(species_code, site_id, wyear) %>%
        dplyr::summarize(count_alive = sum(count_alive)) %>%
        dplyr::arrange(site_id, wyear, species_code) %>%
        # subset(!species_code == 'NoID') %>%
        ggplot(aes(x = wyear, y = count_alive, color = site_id)) +
        geom_point(size = 2, alpha = 0.7) +
        geom_line() +
        scale_y_log10() +
        facet_wrap(~species_code, scales = "free") +
        xlab("") +
        ylab("Count of bats") +
        scale_x_date(date_labels = '%Y') +
        theme_bw() +
        theme(panel.grid = element_blank(), strip.text = element_text(face = "italic"),
          legend.position = "bottom")

      cc_descr_fig1 = paste0("Figure ",
        fig_name,
        ".  Winter Year Colony Counts of bats by site and species. Sites included: ",
        paste0(unique(cc_figure1$data$site_id), collapse = ', '))

      descriptions = c(descriptions, cc_descr_fig1)

      # figures[paste0('figure_',fig_name)] = cc_figure1$data

      if(save_bool){
        cc_fig1_f = paste0(out_dir, "/temps/fig", fig_name,".png")
        fig_files = c(fig_files, cc_fig1_f)
        ggsave(cc_figure1, filename = cc_fig1_f, width = 7, height = 7)
      }else{
        cc_fig1_f = NULL
      }
    }
    return(list(description = descriptions, file = fig_files))
  }else{

    figure_data = cc_bulk_df %>%
      dplyr::mutate(site_id = ifelse(is.na(site_name), as.character(site_id), site_name))

    cc_figure1 = figure_data %>%
      dplyr::select(species_code, site_id, count_alive, wyear) %>%
      dplyr::mutate(wyear = as.Date(paste(wyear, 1, 1, sep = "-"))) %>%
      dplyr::mutate(site_id = as.character(site_id)) %>%
      dplyr::group_by(species_code, site_id, wyear) %>%
      dplyr::summarize(count_alive = sum(count_alive)) %>%
      dplyr::arrange(site_id, wyear, species_code) %>%
      # subset(!species_code == 'NoID') %>%
      ggplot(aes(x = wyear, y = count_alive, color = site_id)) +
      geom_point(size = 2, alpha = 0.7) +
      geom_line() +
      scale_y_log10() +
      facet_wrap(~species_code, scales = "free") +
      xlab("") +
      ylab("Count of bats") +
      scale_x_date(date_labels = '%Y') +
      theme_bw() +
      theme(panel.grid = element_blank(), strip.text = element_text(face = "italic"),
        legend.position = "bottom")

    cc_descr_fig1 = paste0("Figure 1. Winter Year Colony Counts of bats by site and species.  Sites included: ",
      paste0(unique(cc_figure1$data$site_id), collapse = ', '))

    if(save_bool){
      cc_fig1_f = paste0(out_dir, "/temps/fig1.png")
      ggsave(cc_figure1, filename = cc_fig1_f, width = 7, height = 7)
    }else{
      cc_fig1_f = NULL
    }
    return (list(description = cc_descr_fig1, file = cc_fig1_f))
  }
}

#' @title Display ggplot Colony count figure 1
#'
#' @description Use this to display returned figures in list from
#' build_cc_figure_1
#'
#' @param fig Ggplot2 output figures from build_cc_figure_1
#'
#' @export

cc_fig_1_ggplot = function(
  fig){

  fig %>%
    ggplot(aes(x = wyear, y = count_alive, color = site_id)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_line() +
    scale_y_log10() +
    facet_wrap(~species_code, scales = "free") +
    xlab("") +
    ylab("Count of bats") +
    scale_x_date(date_labels = '%Y') +
    theme_bw() +
    theme(panel.grid = element_blank(), strip.text = element_text(face = "italic"),
      legend.position = "bottom")
}











