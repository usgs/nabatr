



#' @title Get Colony Count Examples
#'
#' @description
#' Returns al list of some preset colony count report examples
#'
#' @export
#'

get_cc_examples = function(){
  # Description
  cc_description = "[EXAMPLE]:  "
  # Methods
  cc_methods = "[EXAMPLE]: Survey sites were chosen based on previous knowledge of winter hibernacula in the region, historical monitoring efforts, and suitability criteria outlined in Loeb et al. (2015). Because detection probability of hibernating bats is highly variable within seasons, surveys were conducted between late January and early March to maximize detection (Loeb et al. 2015). Abundance was estimated using visual counts and accompanying digital photographs. Multiple observers conducted counts in each section of the hibernacula to facilitate the estimation of detection probability and to validate species identifications."
  # Lit Cited
  cc_lit_cited = "Loeb, S.C., T.J. Rodhouse, L.E. Ellison, C.L. Lausen, J.D. Reichard, K.M. Irvine, T.E. Ingersoll, J.T.H. Coleman, W.E. Thogmartin, J.R. Sauer, C.M. Francis, M.L. Bayless, T.R. Stanley, and D.H. Johnson. 2015. A plan for the North American Bat Monitoring Program (NABat). General Technical Reports SRS-208. Asheville, NC: U.S. Department of Agriculture Forest Service, Southern Research Station. 112 p."

  return(list(description = cc_description,
              methods_1 = cc_methods,
              lit_cited = cc_lit_cited))
}




#' @title Get Colony Count Results
#'
#' @description
#' Returns compiled results for this project's Colony Count data
#'
#' @export
#'

get_cc_results = function(cc_bulk_df){
  # Remove NA values for winter year and species
  cc_bulk_df = cc_bulk_df %>% tidyr::drop_na(wyear, species)

  ## Set variables to be printed in results section
  spp = unique(cc_bulk_df$species)
  species_sampled = paste(length(spp), " species ", "(", paste(spp, collapse = ", "), ")", sep = "")
  number_of_sites = length(unique(cc_bulk_df$site_name))
  range_winter_years = paste(min(cc_bulk_df$wyear, na.rm = TRUE), "to", max(cc_bulk_df$wyear, na.rm = TRUE))
  number_of_grts = length(unique(cc_bulk_df$grts_id))

  results_overview = paste0("Winter colonies for ", species_sampled, " were counted at ", number_of_sites, " sites from ", range_winter_years, ", and across ", number_of_grts, " grid cells (Table 1).")
  return(results_overview)
}




#' @title Build Colony Count Table 1
#'
#' @description Summary of winter colony count surveys
#'
#' @export
#'
build_cc_table_1 = function(cc_bulk_df){
  survey_table = cc_bulk_df %>%
    dplyr::group_by(wyear, species) %>%
    dplyr::summarise(number_of_sites = length(unique(site_name))) %>%
    tidyr::spread(species, number_of_sites) %>%
    dplyr::rename(`Winter Year` = wyear)
  # Remove the spaces in the field names (breaks on website/docker)
  names(survey_table) = gsub(" ", "_", names(survey_table))

  cc_descr_table1 = paste0("Table 1. Summary of winter colony count surveys. Number of sites surveyed for species by winter year")

  cc_ft1 = flextable::flextable(survey_table, col_keys = names(survey_table))
  cc_ft1 = flextable::height(cc_ft1, height =.7, part = 'header')
  cc_ft1 = flextable::width(cc_ft1, width = 1)
  cc_ft1 = flextable::fontsize(cc_ft1, size = 10, part = "all")

  return (list(table = cc_ft1, description = cc_descr_table1))
}




#' @title Build Colony Count Figure 1
#'
#' @description Winter Colony counts of bats by site and species
#'
#' @export
#'
build_cc_figure_1 = function(cc_bulk_df, out_dir, save_bool = FALSE){
  cc_descr_fig1 = paste0("Figure 1. Winter colony counts of bats by site and species")

  cc_figure1 = cc_bulk_df %>%
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

  if(save_bool){
    cc_fig1_f = paste0(out_dir, "/temps/fig1.png")
    ggsave(cc_figure1, filename = cc_fig1_f)
  }else{
    cc_fig1_f = NULL
  }

  return (list(figure = cc_figure1, description = cc_descr_fig1, file = cc_fig1_f))
}












