#' @title Build Plot for GRTS or Sites Acoustic Stationary Count data
#'
#' @import plotly
#' @import dplyr
#'
#' @description uses the ordered_combined_data that contains bat counts for
#' species with values >0 at that location and then creates a plot to
#' visualize
#'
#' @export

build_grts_plot = function(ordered_combined_data, type = 'sites'){

  # sites
  grts_id = unique(ordered_combined_data$GRTS)
  sites = unique(ordered_combined_data$site_id)

  # Clean data at GRTS level to remove any species that aren't at this GRTS cell
  species_grts_df = dplyr::select(ordered_combined_data, -c('GRTS', 'site_id', 'site_name', 'observed_night', 'type', 'project_id'))

  species_present_grts = names(species_grts_df[,colSums(species_grts_df) > 0])

  # Clean data to include headers
  cleaned_grts_df = dplyr::select(ordered_combined_data, c('GRTS', 'site_id', 'site_name', 'observed_night', 'type', species_present_grts))

  # Build color palette to use for all sites in this grts cell
  n = length(species_present_grts)
  if (n > 0){
    color_palette = colorRampPalette(c("#29a4ff", "#ffd25c", "#8a443d"))(n)
    color_palette_op = colorRampPalette(c("#82caff", "#fff0c7", "#d6a7a3"))(n)
    color_palette_df = data.frame(colors = color_palette, op_colors = color_palette_op, species = species_present_grts)

    if (type == 'sites'){
      for (site in sites){
        # Subset data on a site level within a GRTS cell
        cleaned_site_df = subset(cleaned_grts_df, cleaned_grts_df$site_id == site)
        cleaned_site_df$opacity = ifelse(cleaned_site_df$type == 'automatic', 1, .5)
        cleaned_site_df$x_names = paste0(cleaned_site_df$observed_night,'_', cleaned_site_df$type)
        dates = as.character(cleaned_site_df$observed_night)

        plotly_ = FALSE
        count = 0
        width_ = .1

        for (name in species_present_grts){
          count = count + 1
          species_counts = cleaned_site_df[,name]
          color = as.character(subset(color_palette_df, species == name)$colors)
          opac_color = as.character(subset(color_palette_df, species == name)$op_colors)

          this_color = rep(c(color, opac_color), length(species_counts)/2)

          if (plotly_ == FALSE){
            plotly_ = TRUE
            p_species = plot_ly(x = species_counts, y = cleaned_site_df$x_names, type = 'bar', orientation = 'h',
              name = name, text = paste0(name), legendgroup = name,
              marker = list(color = color,
                line = list(color = 'black',
                  width = width_)))
          }else{
            p_species = p_species %>% add_trace(x = species_counts, name = name,
              text = paste0(name), legendgroup = name,
              marker = list(color = color,
                line = list(color = 'black',
                  width = width_)))
          }
        }
        m = list(l = 50, r = 50, b = 100, t = 50, pad = 4)
        p_species_final = p_species %>%
          layout(barmode = 'stack',
            xaxis = list(title = "Counts"),
            yaxis = list(title = "Observed Night"),
            title = paste0('Site ID: ', site),
            autosize = F,
            showLegend = F,
            margin = m,
            hoverinfo = 'text')
      }
      return (p_species_final)
    } else if (type == 'grts'){
      plotly_ = FALSE
      count = 0
      width_ = .1

      cleaned_grts_df$opacity = ifelse(cleaned_grts_df$type == 'automatic', 1, .5)
      cleaned_grts_df$x_names = paste0(cleaned_grts_df$observed_night,'_', cleaned_grts_df$type)

      for (name in species_present_grts){
        count = count + 1
        species_counts = cleaned_grts_df[,name]
        color = as.character(subset(color_palette_df, species == name)$colors)
        site_ids = cleaned_grts_df[,'site_id']

        if (plotly_ == FALSE){
          plotly_ = TRUE
          p_species = plot_ly(x = species_counts, y = cleaned_grts_df$x_names, type = 'bar', orientation = 'h',
            name = name, text = paste0(name, '_', site_ids), legendgroup = name,
            marker = list(color = color,
              line = list(color = 'black',
                width = width_)))
        }else{
          p_species = p_species %>% add_trace(x = species_counts, name = name,
            text = paste0(name, '_', site_ids), legendgroup = name,
            marker = list(color = color,
              line = list(color = 'black',
                width = width_)))
        }
      }
      m = list(l = 50, r = 50, b = 100, t = 50, pad = 4)
      p_species_final = p_species %>%
        layout(barmode = 'stack',
          xaxis = list(title = "Counts"),
          yaxis = list(title = "Observed Night"),
          title = paste0('GRTS: ', grts_id),
          autosize = F,
          margin = m,
          hoverinfo = 'text')
      return (p_species_final)
    }
  }else {
    if (type == 'grts'){
      title_ = paste0('GRTS: ', grts_id)
    }else if (type == 'sites'){
      title_ = paste0('Site ID: ', sites)
    }
    return (title_)
  }
}
