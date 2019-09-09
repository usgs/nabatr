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

#' @title NABat species lookup table
#'
#' @description
#' Reads in dataframe for NABat species lookup table
#' @keywords species, bats, NABat
#' @examples
#'
#' nabatr::bats_df
#
#' @export
#'
bats_df =  read.csv('data/bat_species.csv')

#' @title NABat login to NABAt Database GQL
#'
#' @import httr
#' @import jsonlite
#' @import ghql
#' @import plyr
#'
#' @description
#' Get a NABat GQL token to use for queries
#' @param username String your NABat username from https://sciencebase.usgs.gov/nabat/#/home
#' @param password (optional) String it will prompt you for your password
#' @keywords bats, NABat, GQL
#' @examples
#'
#' nabat_gql_token = get_nabat_gql_token(username = 'NABat_Username')
#' -- Will prompt for password
#
#' @export
#'
get_nabat_gql_token = function(username, password = NULL){
  # Prompts password input incase password isn't included in function call
  if (is.null(password)){
    password = .rs.askForPassword('Password')
  }
  # Returns a message with username
  message(paste0("Logging into the NABat database as ", username))

  url = 'https://api.sciencebase.gov/nabatmonitoring-survey/graphql'
  # Username and password
  variables = paste0('{"l":{"userName" : "',username,'", "password" : "',password,'"}}')
  # Mutation to get token
  query = 'mutation loging($l:LoginInput!){
    login(input:$l){
      token
      error
    }
  }'

  #Get login token
  # Finalize json request
  pbody = list(query = query, variables = variables)
  # POST to url
  res = POST(url, body = pbody, encode="json")
  # Remove variables with Password
  rm(password, variables, pbody)
  # Extract token
  content = content(res)
  bearer = content$data$login$token
  token = strsplit(bearer, 'Bearer ')[[1]][2]

  # Display token
  return (token)
}


#' @title Find your NABat Projects
#'
#' @import httr
#' @import jsonlite
#' @import ghql
#' @import plyr
#'
#' @description
#' Returns all projects that the user has permissions to view
#' @param username String your NABat username from https://sciencebase.usgs.gov/nabat/#/home
#' @param token String token created from get_nabat_gql_token function
#' @keywords bats, NABat, GQL
#' @examples
#'
#' project_df = get_projects(username = 'NABat_Username', token = 'generated-nabat-gql-token')
#'
#' @export
#'
get_projects = function(token, username){
  # Create cli
  print ('creating cli')
  cli = GraphqlClient$new(url = url,
                          headers = add_headers(.headers = c(Authorization = paste0('Bearer ', token),
                                                             'X-email-address' = username)))
  # Set empty Query
  print ('adding new query')
  qry = Query$new()
  # Build query for all projects under user
  print ('building query')
  qry$query('projIds',
            paste0('{allProjects{
                       nodes{
                         id
                         projectName
                         projectKey
                         }
                       }
                     }'))
  # Build dataframe of project data to return
  print ('execute query')
  proj_dat  = cli$exec(qry$queries$projIds)
  print ('build project dataframe')
  proj_json = fromJSON(proj_dat, flatten = TRUE)
  print ('build data.frame from data')
  # proj_df   = as.data.frame(proj_json)
  return (proj_json)
}




