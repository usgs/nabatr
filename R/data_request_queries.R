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



#' @title Submit Build Data Request
#'
#' @description
#' Submits a build data request to Nabat that adds a new Zip file
#' for this data request id.
#'
#' @param token List token created from get_nabat_gql_token() or
#' get_refresh_token()
#' @param data_request_id Integer that specifies data request identifier
#' @param branch (optional) String that defaults to 'prod' but
#' can also be 'dev'|'beta'|'local'
#' @param url (optional) String url to use for GQL
#' @param aws_gql (optional) String url to use in aws
#' @param aws_alb (optional) String url to use in aws
#' @param docker (optional) Boolean if being run in docker container
#' or not
#'
#' @export
#'
build_data_request = function(
  token,
  data_request_id,
  branch = 'prod',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker = FALSE){

  # Set URL based on branch
  if (is.null(url)) url = get_gql_url(branch)

  # Refresh token
  token = get_refresh_token(token, branch, url, aws_gql, aws_alb, docker)

  # Get headers for token
  tkn_hdr = get_token_headers(token, url, aws_gql, aws_alb, docker)
  headers = tkn_hdr$headers
  token   = tkn_hdr$token

  # Username and password
  variables = paste0('{"create":{"dataRequestId" : ',as.character(data_request_id),' }}')
  # Mutation to get token
  query = 'mutation RRbuildDataRequest($create: buildDataRequestInput!) {
    buildDataRequest(input: $create) {
    success
    }
  }'

  # Finalize json request
  pbody = list(query = query, variables = variables,
    operationName = 'RRbuildDataRequest')
  # POST to url
  res = POST(url, headers, body = pbody, encode="json")
  content = content(res)
  return(content)
}




#' @title Get All Approvals for this Data Request
#'
#' @description
#' Returns a dataframe of all of the approvals within this
#' data request
#'
#' @param token List token created from get_nabat_gql_token() or
#' get_refresh_token()
#' @param data_request_id Integer that specifies data request identifier
#' @param branch (optional) String that defaults to 'prod' but
#' can also be 'dev'|'beta'|'local'
#' @param url (optional) String url to use for GQL
#' @param aws_gql (optional) String url to use in aws
#' @param aws_alb (optional) String url to use in aws
#' @param docker (optional) Boolean if being run in docker container
#' or not
#'
#' @export
#'
get_data_request_approvals = function(
  token,
  data_request_id,
  branch = 'prod',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker = FALSE){

  # Set URL based on branch
  if (is.null(url)) url = get_gql_url(branch)

  # Refresh token
  token = get_refresh_token(token, branch, url, aws_gql, aws_alb, docker)

  # Get headers for token
  tkn_hdr = get_token_headers(token, url, aws_gql, aws_alb, docker)
  headers = tkn_hdr$headers
  token   = tkn_hdr$token

  query =paste0('
    query RRAllVwDataRequestApprovals{
        allVwDataRequestApprovals(filter: {dataRequestId: {equalTo:', as.character(data_request_id),'}}, orderBy: PROJECT_ID_ASC){
        nodes{
        id
        dataRequestId
        projectId
        projectName
        userId
        firstName
        lastName
        email
        description
        response
        lastModified
        }
        }
    }')
  # Create body to send to GQL
  pbody = list(query = query, operationName = 'RRAllVwDataRequestApprovals')
  # Post to nabat GQL
  res      = httr::POST(url, headers, body = pbody, encode='json')
  content   = httr::content(res, as = 'text')
  json = fromJSON(content, flatten = TRUE)
  # This will change based on your query: admin_json$data$allCovarGrts$nodes (see below)
  df   = as.data.frame(json$data$allVwDataRequestApprovals$nodes, stringsAsFactors = FALSE)
  names(df) = tolower(gsub("(?<=[a-z0-9])(?=[A-Z])", "_", names(df), perl = TRUE))

  return (df)
}


#' @title Get downloadable Data Request files
#'
#' @description
#' returns a dataframe containing all of the downloadable files
#' from the specified data request
#'
#' @param token List token created from get_nabat_gql_token() or
#' get_refresh_token()
#' @param data_request_id Integer that specifies data request identifier
#' @param branch (optional) String that defaults to 'prod' but
#' can also be 'dev'|'beta'|'local'
#' @param url (optional) String url to use for GQL
#' @param aws_gql (optional) String url to use in aws
#' @param aws_alb (optional) String url to use in aws
#' @param docker (optional) Boolean if being run in docker container
#' or not
#'
#' @export
#'
get_data_request_files = function(
  token,
  data_request_id,
  branch = 'prod',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker = FALSE){

  # Set URL based on branch
  if (is.null(url)) url = get_gql_url(branch)

  # Refresh token
  token = get_refresh_token(token, branch, url, aws_gql, aws_alb, docker)

  # Get headers for token
  tkn_hdr = get_token_headers(token, url, aws_gql, aws_alb, docker)
  headers = tkn_hdr$headers
  token   = tkn_hdr$token

  query =paste0('
    query RRs3FileServiceListFiles{
    s3FileServiceListFiles(bucket:"nabat-beta-project-files",
    keyPrefix:"data-request/',as.character(data_request_id),'/"){
    objects{
    Key
    LastModified
    ETag
    Size
    StorageClass
    }
    }
    }')
  # Create body to send to GQL
  pbody = list(query = query, operationName = 'RRs3FileServiceListFiles')
  # Post to nabat GQL
  res      = httr::POST(url, headers, body = pbody, encode='json')
  content   = httr::content(res, as = 'text')
  json = fromJSON(content, flatten = TRUE)
  # This will change based on your query: admin_json$data$allCovarGrts$nodes (see below)
  df   = as.data.frame(json$data$s3FileServiceListFiles$objects, stringsAsFactors = FALSE)
  names(df) = tolower(gsub("(?<=[a-z0-9])(?=[A-Z])", "_", names(df), perl = TRUE))

  return (df)
}


#' @title Download Data Request
#'
#' @description
#' downloads a data request specified using the Key field from
#' the output of get_data_request_files()
#' @param token List token created from get_nabat_gql_token() or
#' get_refresh_token()
#' @param key String pulled from field Key in get_data_request_files()
#' @param output_file String output directory + zip file and extension,
#' ex: /path/to/mydirectory/new_file.zip
#' @param branch (optional) String that defaults to 'prod' but
#' can also be 'dev'|'beta'|'local'
#' @param url (optional) String url to use for GQL
#' @param aws_gql (optional) String url to use in aws
#' @param aws_alb (optional) String url to use in aws
#' @param docker (optional) Boolean if being run in docker container
#' or not
#'
#' @export
#'
download_data_request = function(
  token,
  key,
  output_file,
  branch = 'prod',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker = FALSE){

  # Set URL based on branch
  if (is.null(url)) url = get_gql_url(branch)

  # Refresh token
  token = get_refresh_token(token, branch, url, aws_gql, aws_alb, docker)

  # Get headers for token
  tkn_hdr = get_token_headers(token, url, aws_gql, aws_alb, docker)
  headers = tkn_hdr$headers
  token   = tkn_hdr$token

  key = "data-request/12/NABat__2020-11-18_21-58-56__v5_3_15.zip"

  query =paste0('
    query RRs3FileServiceDownloadFile{
    s3FileServiceDownloadFile(bucket: "nabat-beta-project-files",
    key: "',key ,'") {
    s3PresignedUrl
    success
    message
    }
    }')
# Create body to send to GQL
  pbody = list(query = query, operationName = 'RRs3FileServiceDownloadFile')
  # Post to nabat GQL
  res      = httr::POST(url, headers, body = pbody, encode='json')
  content   = httr::content(res, as = 'text')
  json = fromJSON(content, flatten = TRUE)
  # This will change based on your query: admin_json$data$allCovarGrts$nodes (see below)
  download_zip_url = json$data$s3FileServiceDownloadFile$s3PresignedUrl
  if (file.exists(output_file)){
    stop('Error - output file already exists.  Please delete file or change output_file name.')
  }
  download.file(download_zip_url, destfile = output_file)
}


