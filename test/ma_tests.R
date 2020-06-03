# Disclaimer::::
## This will only work if you have an admin acount with NABat.
##   This test will build all possible MA reports across all
##     NABat projects in the database

## CHANGE THESE VALUES####################################
branch_   = 'prod'
username_ = 'nabat_username'
dir_ = '/path/to/output/reports'
##########################################################

token_ = get_nabat_gql_token(username = username_, branch = branch_)
poject_types_df = get_all_project_types(token_)

all_ma_projects = unique(subset(poject_types_df, poject_types_df$number_ma_events > 0)$id)

completed_ma_projects = c()
failed_ma_projects = list()

for (project_id_ in all_ma_projects){
  my_var = tryCatch(
    {
      message(paste0("Running ma report builder for project: ", project_id_))

      token_ = get_refresh_token(token_, branch = branch_)
      # Import the newest version of the species_df
      species_df_ = get_species(token = token_, branch = branch_)
      # Get your projects lookup table
      project_df_ = get_projects(token  = token_,
        branch = branch_)

      # Get survey dataframe, This function also sets the global variable pkg.env$grts_df
      #   to the appropriate grts_frame.  ex: 'Canada'|'CONUS'
      token_ = get_refresh_token(token_, branch = branch_)
      ma_survey_df_ = get_ma_project_summary(token = token_,
        project_id = project_id_,
        project_df = project_df_,
        branch     = branch_)
      proj_dates = unique(ma_survey_df_$year)

      if (dim(ma_survey_df_)[1] == 0){
        message(paste0('No Data Available at project id: ',project_id_))
      } else{
        year_ = proj_dates[1]
        # Get stationary acoustic bulk upload format dataframe
        token_ = get_refresh_token(token_, branch = branch_)
        ma_bulk_df_ = get_ma_bulk_wavs(token      = token_,
          survey_df  = ma_survey_df_,
          project_id = project_id_,
          branch = branch_,
          year = year_) %>% clean_time_fields() %>%
          add_observed_nights() %>%
          add_start_end_nights()

        # Get Acoustic stationary acoustic bulk dataframe
        nightly_observed_list_ = get_observed_nights(ma_bulk_df_)

        token_ = get_refresh_token(token_, branch = branch_)
        ma_doc = build_ma_doc(out_dir = dir_,
          file_name = paste0('ma_report_',project_id_,'_',Sys.Date(),'.docx'),
          project_df = project_df_,
          project_id = project_id_,
          ma_bulk_df = ma_bulk_df_,
          species_df = species_df_,
          year = year_,
          nightly_observed_list = nightly_observed_list_)

        print(ma_doc, target = paste0(dir_, '/', paste0(year_, '_ma_report_',project_id_,'_',format(Sys.time(), '%Y_%m_%d_%H%M%S'),'.docx')))
        completed_ma_projects = c(completed_ma_projects, project_id_)
      }
    },
    error=function(cond) {
      message(paste("Project seems to Fail:", project_id_))
      message(cond)
      my_var = as.character(cond)

    },
    finally={
      message(paste0('finished project: ', project_id_))
    }
  )
  if (!is.integer(my_var)){
    failed_ma_projects[[as.character(project_id_)]] = my_var
  }

}
