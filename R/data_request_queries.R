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
# Created: 2020-11-18
#############################################################################



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

  # When url is not passed in use these two gql urls, otherwise use
  ## the url passed through as a variable.
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
      token = get_refresh_token(token, url = url_, aws_gql = aws_gql,
        aws_alb = aws_alb, docker = docker)
      headers_ = httr::add_headers(host = aws_gql,
        Authorization = paste0("Bearer ", token$access_token))
    }else {
      token = get_refresh_token(token, url = url_)
      headers_ = httr::add_headers(Authorization = paste0("Bearer ",
        token$access_token))
    }
  } else{
    # If Local, use this headers_
    token = get_refresh_token(token, url = url_)
    headers_ = httr::add_headers(Authorization = paste0('Bearer ',
      token$access_token))
  }

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
  res      = httr::POST(url_, headers_, body = pbody, encode='json')
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

  # When url is not passed in use these two gql urls, otherwise use
  ## the url passed through as a variable.
  if (is.null(url)){
    # Prod URL for NABat GQL
    if (branch == 'prod'){
      url_ = 'https://api.sciencebase.gov/nabat-graphql/graphql'
      bucket = 'nabat-prod-project-files'
    } else if (branch == 'dev' | branch == 'beta' | branch == 'local'){
      url_ = 'https://nabat-graphql.staging.sciencebase.gov/graphql'
      bucket = 'nabat-beta-project-files'
    }
  }else {
    url_ = url
  }

  if (docker){
    if(!is.null(aws_gql)){
      url_ = paste0(aws_alb, '/graphql')
      token = get_refresh_token(token, url = url_, aws_gql = aws_gql,
        aws_alb = aws_alb, docker = docker)
      headers_ = httr::add_headers(host = aws_gql,
        Authorization = paste0("Bearer ", token$access_token))
    }else {
      token = get_refresh_token(token, url = url_)
      headers_ = httr::add_headers(Authorization = paste0("Bearer ",
        token$access_token))
    }
  } else{
    # If Local, use this headers_
    token = get_refresh_token(token, url = url_)
    headers_ = httr::add_headers(Authorization = paste0('Bearer ',
      token$access_token))
  }

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
  res      = httr::POST(url_, headers_, body = pbody, encode='json')
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

  # When url is not passed in use these two gql urls, otherwise use
  ## the url passed through as a variable.
  if (is.null(url)){
    # Prod URL for NABat GQL
    if (branch == 'prod'){
      url_ = 'https://api.sciencebase.gov/nabat-graphql/graphql'
      bucket = 'nabat-prod-project-files'
    } else if (branch == 'dev' | branch == 'beta' | branch == 'local'){
      url_ = 'https://nabat-graphql.staging.sciencebase.gov/graphql'
      bucket = 'nabat-beta-project-files'
    }
  }else {
    url_ = url
  }

  if (docker){
    if(!is.null(aws_gql)){
      url_ = paste0(aws_alb, '/graphql')
      token = get_refresh_token(token, url = url_, aws_gql = aws_gql,
        aws_alb = aws_alb, docker = docker)
      headers_ = httr::add_headers(host = aws_gql,
        Authorization = paste0("Bearer ", token$access_token))
    }else {
      token = get_refresh_token(token, url = url_)
      headers_ = httr::add_headers(Authorization = paste0("Bearer ",
        token$access_token))
    }
  } else{
    # If Local, use this headers_
    token = get_refresh_token(token, url = url_)
    headers_ = httr::add_headers(Authorization = paste0('Bearer ',
      token$access_token))
  }

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
  res      = httr::POST(url_, headers_, body = pbody, encode='json')
  content   = httr::content(res, as = 'text')
  json = fromJSON(content, flatten = TRUE)
  # This will change based on your query: admin_json$data$allCovarGrts$nodes (see below)
  download_zip_url = json$data$s3FileServiceDownloadFile$s3PresignedUrl
  if (file.exists(output_file)){
    stop('Error - output file already exists.  Please delete file or change output_file name.')
  }
  download.file(download_zip_url, destfile = output_file)
}


