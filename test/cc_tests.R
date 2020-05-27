# Disclaimer::::
## This will only work if you have an admin acount with NABat.
##   This test will build all possible CC reports across all
##     NABat projects in the database

## CHANGE THESE VALUES####################################
branch_   = 'prod'
username_ = 'nabat_username'
dir_ = '/path/to/output/reports'
##########################################################

token_ = get_nabat_gql_token(username = username_, branch = branch_)
poject_types_df = get_all_project_types(token_)

all_cc_projects = unique(subset(poject_types_df, poject_types_df$number_cc_events > 0)$id)

completed_cc_projects = c()
failed_cc_projects = list()

for (project_id_ in all_cc_projects){
  my_var = tryCatch(
    {
      message(paste0("Running cc report builder for project: ", project_id_))

      token_ = get_refresh_token(token_, branch = branch_)
      # Import the newest version of the species_df
      species_df_ = get_species(token = token_, branch = branch_)
      # Get your projects lookup table
      project_df_ = get_projects(token  = token_,
        branch = branch_)

      # Get survey dataframe, This function also sets the global variable pkg.env$grts_df
      #   to the appropriate grts_frame.  ex: 'Canada'|'CONUS'
      token_ = get_refresh_token(token_, branch = branch_)
      cc_survey_df_ = get_cc_project_summary(token = token_,
                                              project_id = project_id_,
                                              project_df = project_df_,
                                              branch     = branch_)

      # Get stationary acoustic bulk upload format dataframe
      token_ = get_refresh_token(token_, branch = branch_)
      cc_bulk_df_ = get_colony_bulk_counts(token_,
                                          cc_survey_df_,
                                          project_id_,
                                          species_df_)

      token_ = get_refresh_token(token_, branch = branch_)
      cc_doc = build_col_doc(out_dir = dir_,
        file_name =  paste0('cc_report_',project_id_,'_',Sys.Date(),'.docx'),
        project_df = project_df_,
        project_id = project_id_,
        cc_bulk_df = cc_bulk_df_)

      print(cc_doc, target = paste0(dir_, '/', paste0('cc_report_',project_id_,'_',format(Sys.time(), '%Y_%m_%d_%H%M%S'),'.docx')))
      completed_cc_projects = c(completed_cc_projects, project_id_)
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
    failed_cc_projects[[as.character(project_id_)]] = my_var
  }

}
