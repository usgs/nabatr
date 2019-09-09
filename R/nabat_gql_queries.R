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
#'library(httr)
#'library(jsonlite)
#'library(ghql)
#'library(plyr)
#'
#'nabat_gql_token = nabat_gql_login('NABat_Username')
#
#' @export
get_nabat_gql_token = function(username, password = NULL){
  # Prompts password input incase password isn't included in function call
  if (is.null(password)){
    password = .rs.askForPassword('Password')
  }
  # Returns a message with username
  message(paste0("Loging into the NABat database as ", username))

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

