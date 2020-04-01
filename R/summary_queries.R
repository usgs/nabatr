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
#' @import httr
#' @import jsonlite
#'
#' @description
#' Returns a dataframe with all of a project's acoustic stationary data down to the event
#' @param token String token created from get_nabat_gql_token function
#' @param project_id Numeric project id for a nabat project
#' @param branch (optional) String 'prod' | 'dev' | 'beta' | 'local'
#' @param url (optional) String testing option
#' @param aws_gql (optional) String option for AWS
#' @param aws_alb (optional) String option for AWS
#' @param docker (optional) Boolean option for docker
#' @keywords bats, NABat, GQL
#' @examples
#'
#' @export
#'
get_acoustic_project_summary = function(token, project_id, branch ='prod', url = NULL, aws_gql = NULL, aws_alb = NULL, docker=FALSE){

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

  # Sample frame lookup
  sample_frame_df = data.frame(ids = c(12,14,15,19,20,21),
    sample_frame_short = c('Mexico', 'Continental US', 'Hawaii', 'Canada', 'Alaska', 'Puerto Rico'),
    sample_frame_description = c('Mexico 10x10km Grid', 'Conus (Continental US) 10x10km Grid', 'Hawaii 5x5km Grid', 'Canada 10x10km Grid',
      'Alaska 10x10km Grid', 'Puerto Rico 5x5km Grid'))

  # Set Query
  query = paste0('{ allVwStationaryAcousticSummaries (filter :{projectId:{equalTo:',project_id_,'}}){
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
  cont_df$year = data.frame(do.call('rbind', strsplit(as.character(cont_df$event),' ',fixed=TRUE)), stringsAsFactors = FALSE)[,3]

  # Define package environmental varioables
  print ('Setting species_df environmental variable')
  if (is.null(pkg.env$bats_df)){
    species_df = get_species(token = token, url = url_, aws_gql = aws_gql, aws_alb = aws_alb, docker = docker)
    assign('bats_df', species_df, pkg.env)
  }

  # Return dataframe of projects
  return (cont_df)
}
