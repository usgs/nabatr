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
# Created: 2020-4-1
#############################################################################

#' @title Get a Project's Stationary Acoustic Summary Data
#'
#' @import jsonlite
#'
#' @description
#' Returns a dataframe with all of a project's acoustic stationary data down to the event
#' @param token List token created from get_nabat_gql_token() or get_refresh_token()
#' @param project_df Dataframe output from get_projects()
#' @param project_id Numeric or String a project id
#' @param branch (optional) String that defaults to 'prod' but can also be 'dev'|'beta'|'local'
#' @param url (optional) String url to use for GQL
#' @param aws_gql (optional) String url to use in aws
#' @param aws_alb (optional) String url to use in aws
#' @param docker (optional) Boolean if being run in docker container or not
#'
#' @keywords bats, NABat, GQL
#' @examples
#'
#' \dontrun{
#' survey_df = get_project_surveys(token,
#'                                 get_projects(token),
#'                                 project_id)
#' }
#'
#' @export
#'
get_acoustic_project_summary = function(token, project_df, project_id, branch ='prod', url = NULL, aws_gql = NULL, aws_alb = NULL, docker=FALSE){

  # When url is not passed in use these two gql urls, otherwise use the url passed through
  #  as a variable.
  if (is.null(url)){
    # Prod URL for NABat GQL
    if (branch == 'prod'){
      url_ = 'https://api.sciencebase.gov/nabat-graphql/graphql'
    } else if (branch == 'dev' | branch == 'beta' | branch == 'local'){
      url_ = 'https://nabat-graphql.staging.sciencebase.gov/graphql'
    }
  }else {
    url_ = url
  }

  if (docker){
    if(!is.null(aws_gql)){
      url_ = paste0(aws_alb, '/graphql')
      token = get_refresh_token(token, url = url_, aws_gql = aws_gql, aws_alb = aws_alb, docker = docker)
      headers_ = httr::add_headers(host = aws_gql, Authorization = paste0("Bearer ", token$access_token))
    }else {
      token = get_refresh_token(token, url = url_)
      headers_ = httr::add_headers(Authorization = paste0("Bearer ", token$access_token))
    }
  } else{
    # If Local, use this headers_
    token = get_refresh_token(token, url = url_)
    headers_ = httr::add_headers(Authorization = paste0('Bearer ', token$access_token))
  }

  # Set Query
  query = paste0('{ allVwStationaryAcousticSummaries (filter :{projectId:{equalTo:',project_id,'}}){
    nodes{
      projectId
      grtsId
      surveyId
      eventId
      type
      event
      verified
      missing
      }
    }
    }')
  pbody = list(query = query)

  # Post to nabat GQL
  res = httr::POST(url_, headers_, body = pbody, encode='json')
  content = httr::content(res, as = 'text')
  cont_json = jsonlite::fromJSON(content, flatten = TRUE)
  # Rename field names to snake case instead of camel case
  cont_df   = as.data.frame(cont_json$data$allVwStationaryAcousticSummaries$nodes, stringsAsFactors = FALSE)
  names(cont_df) = tolower(gsub("(?<=[a-z0-9])(?=[A-Z])", "_", names(cont_df), perl = TRUE))
  # Add a year field that is a string split from the event field
  if (dim(cont_df)[1] > 0){
    cont_df$year = as.integer(gsub("^.* ", "", cont_df$event))
    names(cont_df)[names(cont_df) == 'grts_id']    = 'grts_cell_id'
    row.names(cont_df) = NULL
  }

  # Define global grts_fname()
  grts_fname = get_grts_frame_name(project_df, project_id)
  assign('grts_fname', grts_fname, pkg.env)

  # Define package environmental variables
  if (is.null(pkg.env$bats_df)){
    print ('Setting species_df environmental variable')
    species_df = get_species(token = token, url = url_, aws_gql = aws_gql, aws_alb = aws_alb, docker = docker)
    assign('bats_df', species_df, pkg.env)
  }

  # Return dataframe of projects
  return (cont_df)
}
