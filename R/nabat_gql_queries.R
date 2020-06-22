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


# # # Global Variables for NABatR
pkg.env = new.env()
pkg.env$bats_df = NULL

#' @title NABat species range maps
#'
#' @description Reading in species range maps with readOGR
#'
#' @export

species_range_shps = rgdal::readOGR('data/bat_species_ranges.shp')[,1:4]



#' @title NABat GRTS lookup list with csvs of coordinates
#' for all GRTS in a region
#'
#' @description
#' Used to grab correct coordinates for GRTS lookups
#'
#' @keywords GRTS, spatial, NABat
#' @examples
#'
#' \dontrun{
#' nabatr::grts_lookup_df
#' }
#'
#' @export

grts_lookup_df = list('Canada' = read.csv(paste0('data/GRTS_coords_Canada.csv'),
  stringsAsFactors=FALSE),
  'Alaska' = read.csv(paste0('data/GRTS_coords_Alaska.csv'),
    stringsAsFactors=FALSE),
  'Mexico' = read.csv(paste0('data/GRTS_coords_Mexico.csv'),
    stringsAsFactors=FALSE),
  'Puerto Rico' = read.csv(paste0('data/GRTS_coords_Puerto_Rico.csv'),
    stringsAsFactors=FALSE),
  'Hawaii' = read.csv(paste0('data/GRTS_coords_Hawaii.csv'),
    stringsAsFactors=FALSE),
  'Continental US' = read.csv(paste0('data/GRTS_coords_CONUS.csv'),
    stringsAsFactors=FALSE))


#' @title Get the bat species lookup table
#'
#' @description
#' Reads in dataframe for NABat species lookup table
#'
#' @param token List token created from get_nabat_gql_token() or
#' get_refresh_token()
#' @param branch (optional) String that defaults to 'prod' but
#' can also be 'dev'|'beta'|'local'
#' @param url (optional) String url to use for GQL
#' @param aws_gql (optional) String url to use in aws
#' @param aws_alb (optional) String url to use in aws
#' @param docker (optional) Boolean if being run in docker container
#' or not
#'
#' @keywords species, bats, NABat
#' @examples
#'
#' \dontrun{
#' species_df = get_species(token = get_nabat_gql_token())
#' -- Prompts for username/password
#' -- username and password can be arguments in
#' get_nabat_gql_token(username,password)
#' }
#'
#' @export
#'
get_species = function(
  token,
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

  # Set Query
  query =   'query RRspecies{ allSpecies{
                nodes{
                  id
                  speciesCode
                  species
                  commonName
                  family
                  genus
                  commonName
                  speciesCode6
                }
              }}'
  pbody = list(query = query, operationName = 'RRspecies')
  # Post to nabat GQL
  res       = httr::POST(url_, headers_, body = pbody, encode='json')
  content   = httr::content(res, as = 'text')
  species_json = fromJSON(content, flatten = TRUE)
  species_df   = as.data.frame(species_json$data$allSpecies$nodes,
    stringsAsFactors = FALSE)
  names(species_df) = tolower(gsub("(?<=[a-z0-9])(?=[A-Z])", "_",
    names(species_df), perl = TRUE))

  if (dim(species_df)[1] > 0){
    names(species_df)[names(species_df) == 'species_code6']    = 'species_code_6'
    # Add a field that determines whether this record is a bat or not a bat
    species_df = species_df %>%
      dplyr::mutate(bat_call = ifelse(species_code == 'NOISE' , FALSE, TRUE)) %>%
      dplyr::mutate(species = ifelse(is.na(species), 'NoID', species))
  }

  # Define package environmental varioables
  if (is.null(pkg.env$bats_df)){
    assign('bats_df', species_df, pkg.env)
  }

  return(species_df)
}

#' @title NABat login to NABAt Database GQL
#'
#' @description
#' Get a NABat GQL token to use for queries
#' @param username (optional) String your NABat username from https://sciencebase.usgs.gov/nabat/#/home
#' @param password (optional) String it will prompt you for your password
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
#' nabat_gql_token = get_nabat_gql_token(username = 'NABat_Username')
#' # Prompts for password
#' }
#'
#' @export
#'
get_nabat_gql_token = function(
  username = NULL,
  password = NULL,
  branch = 'prod',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker = FALSE){

  # Prompts password input incase password isn't included in function call
  if (is.null(username)){
    username = rstudioapi::showPrompt(title = "Username",
      message = "Username", default = "")
  }
  if (is.null(password)){
    password = .rs.askForPassword('Password')
  }

  out = tryCatch({
    # Returns a message with username
    message(paste0("Logging into the NABat database as ", username))

    # When url is not passed in use these two gql urls,
    ## otherwise use the url passed through as a variable.
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
      # If Docker 3_5_3 use this headers_
      if(!is.null(aws_gql)){
        url_ = paste0(aws_alb, '/graphql')
        headers_ = httr::add_headers(host = aws_gql)
      }else {
        headers_ = httr::add_headers(Accept = "")
      }
    } else{
      # If Local, use this headers_
      headers_ = httr::add_headers(Accept = "")
    }

    # Username and password
    variables = paste0('{"l":{"userName" : "',username,'", "password" : "',
      password,'"}}')
    # Mutation to get token
    query = 'mutation RRlogin($l:LoginInput!){
      login(input:$l){
      access_token,
      refresh_token,
      expires_in
      }
    }'
    # Finalize json request
    pbody = list(query = query, variables = variables, operationName = 'RRlogin')
    # POST to url
    res = POST(url_, headers_, body = pbody, encode="json")
    # Remove variables with Password
    rm(password, variables, pbody)
    # Extract token
    content = content(res)
    error  = content$data$login$error
    bearer = content$data$login$access_token
    refresh_token = content$data$login$refresh_token

    if (res$status_code != 200){stop(paste0('Status code: ', res$status_code))}
    if (is.null(refresh_token)){stop('Error on login. Check Password/Username ')}

    access_token = strsplit(bearer, 'Bearer ')[[1]][2]
    message("Returning a GQL token for NABat.")
    expires = content$data$login$expires_in - (60 * 10)
    refresh_at_this = Sys.time() + expires
    return (list(refresh_token = refresh_token, access_token = access_token,
      refresh_at = refresh_at_this))
    },
    # If it errors or refresh_token = NULL then function will fail
    error = function(cond) {
      message(cond)
      return(NULL)
    })
  return (out)
  }


#' @title NABat login to NABAt Database GQL and get access_token
#'
#' @description
#' Get a NABat GQL token to use for queries
#'
#' @param token List token created from get_nabat_gql_token() or
#' get_refresh_token()
#' @param branch (optional) String that defaults to 'prod' but can
#' also be 'dev'|'beta'|'local'
#' @param url (optional) String url to use for GQL
#' @param aws_gql (optional) String url to use in aws
#' @param aws_alb (optional) String url to use in aws
#' @param docker (optional) Boolean if being run in docker container
#' or not
#' @keywords bats, NABat, GQL
#' @examples
#'
#' #' \dontrun{
#' nabat_gql_token = get_refresh_token(token)
#' -- Prompts for password
#' }
#'
#' @export
#'
get_refresh_token = function(
  token = NULL,
  branch = 'prod',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker = FALSE,
  force = FALSE){

  # When url is not passed in use these two gql urls, otherwise
  ## use the url passed through as a variable.
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
    # If Docker 3_5_3 use this headers_
    if(!is.null(aws_gql)){
      url_ = paste0(aws_alb, '/graphql')
      headers_ = httr::add_headers(host = aws_gql)
    }else {
      headers_ = httr::add_headers(Accept = "")
    }
  } else{
    # If Local, use this headers_
    headers_ = httr::add_headers(Accept = "")
  }

  if (is.null(token)){
    message('Run get_nabat_gql_token() to log back in.')
    # return (get_nabat_gql_token(username=NULL,
    #   password =NULL,
    #   branch = branch,
    #   url = url,
    #   aws_gql = aws_gql,
    #   aws_alb = aws_alb,
    #   docker = docker))
  }

  expires_in = token$refresh_at - Sys.time()
  # If the token has expired than refresh the access_token and
  ## use this new one

  out = tryCatch({
    if (expires_in < 0 | force){
      # print ('Token is expired, Returning a new one.')
      # Username and password
      variables = paste0('{"l":{"userName" : "", "password" : "", "refreshToken": "',
        token$refresh_token,'"}}')
      # Mutation to get token
      query = 'mutation RRlogin($l:LoginInput!){
      login(input:$l){
      access_token,
      refresh_token,
      expires_in
      }
    }'
      # Finalize json request0
      pbody = list(query = query, variables = variables, operationName = 'RRlogin')
      # POST to url
      res = httr::POST(url_, headers_, body = pbody, encode="json")

      # Extract token
      content = content(res)
      error  = content$data$login$error
      bearer = content$data$login$access_token
      refresh_token  = content$data$login$refresh_token

      if (res$status_code != 200){stop(paste0('Status code: ', res$status_code))}
      if (is.null(refresh_token)){stop('Error on login. Check Password/Username ')}

      access_token = strsplit(bearer, 'Bearer ')[[1]][2]
      message("Returning a GQL token for NABat.")
      expires = content$data$login$expires_in - (60 * 10)
      refresh_at_this = Sys.time() + expires
      return (list(refresh_token = refresh_token, access_token = access_token,
        refresh_at = refresh_at_this))

}else{
  # If the access token has not expired, then use the original one from token$access_token
  # print ('Token is still good. Returning original')
  refresh_at_this = token$refresh_at
  refresh_token = token$refresh_token
  access_token = token$access_token
  return (list(refresh_token = refresh_token, access_token = access_token,
    refresh_at = refresh_at_this))
}
    },
    # If it errors or refresh_token = NULL then function will fail
    error = function(cond) {
      message(cond)
      return(NULL)
    })
  return (out)
}


#' @title Get User Id from username
#'
#' @description
#' Uses GQL to query username for user ID
#'
#' @param username String NABat username (email)
#' @param token List token created from get_nabat_gql_token()
#' or get_refresh_token()
#' @param branch (optional) String that defaults to 'prod' but
#' can also be 'dev'|'beta'|'local'
#' @param url (optional) String url to use for GQL
#' @param aws_gql (optional) String url to use in aws
#' @param aws_alb (optional) String url to use in aws
#' @param docker (optional) Boolean if being run in docker
#' container or not
#'
#' @keywords species, bats, NABat
#'
#' @export
#'
get_user_id_by_email = function(
  username,
  token,
  branch = 'prod',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker = FALSE){

  # When url is not passed in use these two gql urls, otherwise
  ## use the url passed through as a variable.
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
      headers_ = httr::add_headers(host = aws_gql, Authorization = paste0("Bearer ",
        token$access_token))
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

  # Set Query
  query =   'query RRuserByEmailQuery ($email: String!) {
  userByEmail (email: $email) {
  id
  }
}'
  variables = paste0('{"email": "',username,'"}')
  pbody = list(query = query, variables = variables,
    operationName = 'RRuserByEmailQuery')
  # Post to nabat GQL
  res       = httr::POST(url_, headers_, body = pbody, encode='json')
  content   = httr::content(res)
  user_id =  content$data$userByEmail$id

  return(user_id)
  }


#' @title Search NABat Projects
#'
#' @description
#' Returns all projects that the user has permissions to view
#'
#' @param token List token created from get_nabat_gql_token()
#' or get_refresh_token()
#' @param branch (optional) String that defaults to 'prod' but
#'  can also be 'dev'|'beta'|'local'
#' @param url (optional) String url to use for GQL
#' @param aws_gql (optional) String url to use in aws
#' @param aws_alb (optional) String url to use in aws
#' @param docker (optional) Boolean if being run in docker
#' container or not
#'
#' @keywords bats, NABat, GQL
#' @examples
#'
#' \dontrun{
#' project_df = get_projects(token)
#' }
#'
#' @export
#'
get_projects = function(
  token,
  branch ='prod',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker=FALSE){

  # When url is not passed in use these two gql urls, otherwise
  ## use the url passed through as a variable.
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

  # Sample frame lookup
  sample_frame_df = data.frame(ids = c(12,14,15,19,20,21),
    sample_frame_short = c('Mexico', 'Continental US', 'Hawaii',
      'Canada', 'Alaska', 'Puerto Rico'),
    sample_frame_description = c('Mexico 10x10km Grid',
      'Conus (Continental US) 10x10km Grid', 'Hawaii 5x5km Grid',
      'Canada 10x10km Grid', 'Alaska 10x10km Grid', 'Puerto Rico 5x5km Grid'))

  # Set Query
  query = 'query RRallProjects{allProjects{
                       nodes{
                          id
                          projectName
                          projectKey
                          description
                          sampleFrameId
                          organizationByOwningOrganizationId{
                          name
                          address
                          city
                          stateProvince
                          postalCode
                          }
                        }
                        }
          }'
  pbody = list(query = query, operationName = 'RRallProjects')
  # Post to nabat GQL
  res       = httr::POST(url_, headers_, body = pbody, encode='json')
  content   = httr::content(res, as = 'text')
  proj_json = fromJSON(content, flatten = TRUE)
  proj_df   = rename_project_df(as.data.frame(proj_json,
    stringsAsFactors = FALSE)) %>%
    left_join(sample_frame_df, by = c('sample_frame_id' = 'ids'))

  # Define package environmental varioables
  if (is.null(pkg.env$bats_df)){
    species_df = get_species(token = token, url = url_,
      aws_gql = aws_gql, aws_alb = aws_alb, docker = docker)
    assign('bats_df', species_df, pkg.env)
  }

  # Return dataframe of projects
  return (proj_df)
}



#' @title Get a project's Stationary Acoustic Surveys (DEPRECIATED)
#' see get_acoustic_project_summary()
#'
#' @description
#' (DEPRECIATED)
#' Returns all surveys within a single project (project_id)
#'
#' @param token List token created from get_nabat_gql_token()
#' or get_refresh_token()
#' @param project_df Dataframe output from get_projects()
#' @param project_id Numeric or String a project id
#' @param branch (optional) String that defaults to 'prod'
#' but can also be 'dev'|'beta'|'local'
#' @param url (optional) String url to use for GQL
#' @param aws_gql (optional) String url to use in aws
#' @param aws_alb (optional) String url to use in aws
#' @param docker (optional) Boolean if being run in docker
#' container or not
#'
#' @keywords bats, NABat, GQL, Surveys
#' @examples
#'
#' \dontrun{
#' survey_df = get_project_surveys(token,
#'                                 get_projects(token),
#'                                 project_id)
#' }
#'
#' @export
get_project_surveys = function(
  token,
  project_df,
  project_id,
  branch ='prod',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker = FALSE){

  # When url is not passed in use these two gql urls, otherwise
  ## use the url passed through as a variable.
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

  # Set Query
  query = paste0('query RRallSurveys{allSurveys (filter :
          {projectId:{equalTo:', as.numeric(project_id) ,'}}){
                       nodes{
                            id
                            projectId
                            grtsId
                          }
                          }
          }')
  pbody = list(query = query, operationName = 'RRallSurveys')
  # Post to nabat GQL
  res       = httr::POST(url_, headers_, body = pbody, encode='json')
  content   = httr::content(res, as = 'text')
  survey_json = fromJSON(content, flatten = TRUE)
  survey_df   = rename_survey_df(as.data.frame(survey_json))

  # Define global grts_fname ()
  grts_fname = get_grts_frame_name(project_df, project_id)
  assign('grts_fname', grts_fname, pkg.env)

  # Define package environmental varioables
  if (is.null(pkg.env$bats_df)){
    species_df = get_species(token = token, url = url_,
      aws_gql = aws_gql, aws_alb = aws_alb, docker = docker)
    assign('bats_df', species_df, pkg.env)
  }

  return (survey_df)
}



#' @title Get the GRTS Frame name based on project_id and project_df
#'
#' @description returns the name of the grts frame region. Used to assign
#' a pkg.env global variable for what coordinates csv to use for this project
#'  -- project specific
#'  ex:  When you pull survey data for a project in the US, the package imports
#'         the GRTS coordinates for Continental United states.  If your project
#'         is in Canada, it imports the Canadian coordinates .. etc for Hawaii,
#'         Puerto Rico, Alaska, and Mexico.
#'
#' @param project_df Dataframe output from get_projects()
#' @param project_id Numeric or String a project id
#'
#' @examples
#'
#' \dontrun{
#' grts_fname = get_project_surveys(get_projects(token),
#'                                  project_id)
#' }
#'
#' @export
#'
get_grts_frame_name = function(
  project_df,
  project_id){

  proj_id = project_id
  project_sample_frame = as.character(subset(project_df,
    project_df$project_id == proj_id)$sample_frame_short)
  return(project_sample_frame)
}



#' @title Get Acoustic stationary bulk upload template dataframe
#' for a project
#'
#' @description
#' Returns all surveys within a single project (project_id)
#' @param token List token created from get_nabat_gql_token()
#'  or get_refresh_token()
#' @param survey_df Dataframe a survey dataframe from the output
#' of get_acoustic_project_summary()
#' @param project_id Numeric or String a project id
#' @param year (optional) Numeric year of data to be returned.
#'               NULL = first year, 'all' = all years, 2018 =
#'               only 2018 data
#' @param branch (optional) String that defaults to 'prod' but
#' can also be 'dev'|'beta'|'local'
#' @param url (optional) String url to use for GQL
#' @param aws_gql (optional) String url to use in aws
#' @param aws_alb (optional) String url to use in aws
#' @param docker (optional) Boolean if being run in docker container
#'  or not
#'
#' @keywords bats, NABat, GQL, Surveys
#' @examples
#'
#' \dontrun{
#' acoustic_bulk_df = get_sa_bulk_wavs(
#'   token,
#'   get_acoustic_project_summary(),
#'   project_id)
#' }
#'
#' @export
get_sa_bulk_wavs = function(
  token,
  survey_df,
  project_id,
  year = NULL,
  branch = 'prod',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker = FALSE){

  # When url is not passed in use these two gql urls, otherwise
  ## use the url passed through as a variable.
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

  # Pull in geoms dataframe
  query ='query RReventGeometries{allEventGeometries {
        nodes{
      id
      name
      geom{
      geojson
      }
    }
    }
  }'
  pbody = list(query = query, operationName = 'RReventGeometries')

  res = httr::POST(url_, headers_, body = pbody, encode='json')
  content = httr::content(res, as = 'text')
  geom_json = fromJSON(content, flatten = TRUE)
  geom_df   = rename_geoms_df(as.data.frame(geom_json,
    stringsAsFactors = FALSE))

  # Extract all survey ids from survey_df
  if (is.null(year)){
    year_ = unique(survey_df$year)[1]
    survey_ids = unique(subset(survey_df, survey_df$year == year_)$survey_id)
  } else if (year == 'all'){
    survey_ids = unique(survey_df$survey_id)
    year_ = NULL
  } else{
    year_ = year
    survey_ids = unique(subset(survey_df, survey_df$year == year_)$survey_id)
  }
  # print (year_)

  # Set empty dataframe to build acoustic stationary bulk template data in
  all_wav_n_acc = data.frame()

  # Query each survey through GQL to extract and build a dataframe with all
  #   acoustic stationary records for these acoustic survey ids
  for (survey in survey_ids){

    # Attempt to refresh token every loop
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

    # Set Query
    query = paste0('query RRsaSurveys{allSurveys (filter :
      {id:{equalTo:', as.numeric(survey),'}}){
      nodes{
      id
      projectId
      grtsId
      stationaryAcousticEventsBySurveyId {
      nodes{
      id
      surveyId
      activationStartTime
      activationEndTime
      deviceId
      microphoneId
      microphoneOrientationId
      microphoneHeight
      distanceToClutterMeters
      clutterTypeId
      distanceToWater
      waterType
      percentClutterMethod
      habitatTypeId
      eventGeometryId
      stationaryAcousticValuesBySaSurveyId{
      nodes{
      wavFileName
      recordingTime
      softwareId
      speciesId
      manualId
      }
      }
      }
      }
      }
  }}')
    pbody = list(query = query, operationName = 'RRsaSurveys')

    res       = httr::POST(url_, headers_, body = pbody, encode='json')
    content   = httr::content(res, as = 'text')
    grts_json = fromJSON(content, flatten = TRUE)

    proj_id_df  = as.data.frame(grts_json$data$allSurveys$nodes,
      stringsAsFactors = FALSE)
    acc_events = as.data.frame(proj_id_df$stationaryAcousticEventsBySurveyId.nodes,
      stringsAsFactors = FALSE)

    # Get grts cell for this survey
    grts_cell = unique(subset(survey_df,
      survey_df$survey_id == survey)$grts_cell_id)

    # Build wave files dataframe or raise error message if survey has no data
    if (dim(acc_events)[1] == 0){
      message (paste0('This survey has no Sationary acoustic data present: ',
        survey, ' GRTS id: ', grts_cell))
    }else{
      message (paste0('Compiling stationary acoustic data for survey: ',
        survey, ' GRTS id: ', grts_cell))
      wav_files = data.frame()
      acc_events = acc_events %>%
        dplyr::left_join(geom_df, by= c('eventGeometryId'= 'event_geometry_id')) %>%
                    mutate(site_name = paste0(proj_id_df$grtsId, '_', location_name)) %>%
                    dplyr::mutate(geom_type = ifelse(is.na(geom_type),'NA',geom_type))
      for (x in 1:dim(acc_events)[1]){
        rename = TRUE
        this_site_name = acc_events[x,]$site_name
        # Check for no data in this survey acoustic
        if (dim(as.data.frame(acc_events$stationaryAcousticValuesBySaSurveyId.nodes[x],
          stringsAsFactors = FALSE))[1] == 0){
          message (paste0('Site name ', this_site_name,
            ' is missing Acoustic values at this survey: ', survey))
          rename = FALSE
        }else{
          if (acc_events$geom_type[x] == 'Point'){
            # defining the lat/lon for this accoustic Event
            lon = acc_events$geom_coordinates[x][[1]][1]
            lat = acc_events$geom_coordinates[x][[1]][2]
          } else{
            lon = NA
            lat = NA
          }

          wav_int_files  = as.data.frame(acc_events$stationaryAcousticValuesBySaSurveyId.nodes[x],
            stringsAsFactors=FALSE)
          if (dim(wav_int_files)[1]==0){
            wav_int_files = data.frame()
          } else{
            id       = acc_events[x,]$id
            wav_int_files['stationary_acoustic_values_id'] = id
            wav_int_files['latitude'] = lat
            wav_int_files['longitude'] = lon
          }
          if (dim(wav_files)[1] <1){
            wav_files = wav_int_files
          }else {
            wav_files = rbind(wav_files, wav_int_files)
          }
        }
      }

      # If rename = TRUE (The acoustic data exists for this site_name)
      if (rename){
        # Rename and select from the 3 tables
        wav_files_rn  = rename_acoustic_df(wav_files)[,c('audio_recording_name',
          'recording_time', 'software_id', 'auto_id',
          'manual_id', 'stationary_acoustic_values_id', 'latitude', 'longitude')]
        acc_events_rn = rename_acoustic_df(acc_events)[,c('stationary_acoustic_values_id',
          'location_name', 'survey_start_time',
          'survey_end_time', 'device_id', 'microphone_id' ,'microphone_orientation',
          'microphone_height', 'distance_to_nearest_clutter', 'clutter_type_id', 'site_name',
          'distance_to_nearest_water', 'water_type', 'percent_clutter', 'habitat_type_id')]

        # Set values for survey, project, and grts ids in dataframe
        wav_files_rn[,'survey_id']    = survey
        wav_files_rn[,'project_id']   = project_id
        wav_files_rn[,'grts_cell_id'] = proj_id_df$grtsId

        # Merge wav files dataframe and acoustic events dataframe for all data
        wav_n_acc = merge(wav_files_rn, acc_events_rn, by = 'stationary_acoustic_values_id')

        # Iteratively combine the wav_n_acc dataframes together for each new survey
        all_wav_n_acc = rbind(all_wav_n_acc, wav_n_acc)
      }
    }
  }
  # Return the combined data in the format of the acoustic stationary bulk upload template form
  if (is.null(year_)){
    return (all_wav_n_acc)
  }else {
    all_wav_n_acc = subset(all_wav_n_acc, format(as.Date(all_wav_n_acc$recording_time), '%Y') == as.integer(year_))
    return(all_wav_n_acc)
  }
}




#' @title Get Acoustic mobile bulk upload template dataframe for a project
#'
#' @description
#' Returns all surveys within a single project (project_id)
#' @param token List token created from get_nabat_gql_token() or get_refresh_token()
#' @param survey_df Dataframe a survey dataframe from the output of get_acoustic_m_project_summary()
#' @param project_id Numeric or String a project id
#' @param year (optional) Numeric year of data to be returned.
#'               NULL = first year, 'all' = all years, 2018 = only 2018 data
#' @param branch (optional) String that defaults to 'prod' but can also be 'dev'|'beta'|'local'
#' @param url (optional) String url to use for GQL
#' @param aws_gql (optional) String url to use in aws
#' @param aws_alb (optional) String url to use in aws
#' @param docker (optional) Boolean if being run in docker container or not
#'
#' @keywords bats, NABat, GQL, Surveys
#' @examples
#'
#' \dontrun{
#' acoustic_bulk_df = get_sa_bulk_wavs(token,
#'                                           get_acoustic_m_project_summary(),
#'                                           project_id)
#' }
#'
#' @export
get_ma_bulk_wavs = function(
  token,
  survey_df,
  project_id,
  year = NULL,
  branch = 'prod',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker = FALSE){


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

# Pull in geoms dataframe
query ='query RReventGeometries {allEventGeometries {
  nodes{
  id
  name
  geom{
    geojson
      }
    }
  }
}'
pbody = list(query = query, operationName = 'RReventGeometries')

res = httr::POST(url_, headers_, body = pbody, encode='json')
content = httr::content(res, as = 'text')
geom_json = fromJSON(content, flatten = TRUE)
geom_df   = rename_geoms_df(as.data.frame(geom_json, stringsAsFactors = FALSE))

# Extract all survey ids from survey_df
if (is.null(year)){
  year_ = unique(survey_df$year)[1]
  survey_ids = unique(subset(survey_df, survey_df$year == year_)$survey_id)
} else if (year == 'all'){
  survey_ids = unique(survey_df$survey_id)
  year_ = NULL
} else{
  year_ = year
  survey_ids = unique(subset(survey_df, survey_df$year == year_)$survey_id)
}
# print (year_)

# Set empty dataframe to build acoustic stationary bulk template data in
all_wav_n_acc = data.frame()

# Query each survey through GQL to extract and build a dataframe with all
#   acoustic mobile records for these acoustic survey ids
for (survey in survey_ids){
  # Refresh token every loop
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
  query = paste0('query RRmaSurveys{ allSurveys (filter :{id:{equalTo:', as.numeric(survey),'}}){
    nodes{
    id
    projectId
    grtsId
    mobileAcousticEventsBySurveyId {
    nodes{
    id
    surveyId
    activationStartTime
    activationEndTime
    deviceId
    microphoneId
    habitatTypeId
    eventGeometryId
    mobileAcousticValuesByMaSurveyId{
    nodes{
    wavFileName
    recordingTime
    softwareId
    speciesId
    manualId
    recordingLocation{
    geojson
    }}}}}}}}')
  pbody = list(query = query, operationName = 'RRmaSurveys')

  res       = httr::POST(url_, headers_, body = pbody, encode='json')
  content   = httr::content(res, as = 'text')
  grts_json = fromJSON(content, flatten = TRUE)

  proj_id_df  = as.data.frame(grts_json$data$allSurveys$nodes, stringsAsFactors = FALSE)
  acc_events = as.data.frame(proj_id_df$mobileAcousticEventsBySurveyId.nodes, stringsAsFactors = FALSE)

  # Change headers if the data exists
  if (dim(acc_events)[1] > 0){
    names(acc_events) = tolower(gsub("(?<=[a-z0-9])(?=[A-Z])", "_", names(acc_events), perl = TRUE))
    names(acc_events)[names(acc_events) == 'mobile_acoustic_values_by_ma_survey_id.nodes']    = 'values'
    names(acc_events)[names(acc_events) == 'event_geometry_id']    = 'grts_geometry_id'
  }

  # Get grts cell for this survey
  grts_cell = unique(subset(survey_df, survey_df$survey_id == survey)$grts_cell_id)
  # Build wave files dataframe or raise error message if survey has no data
  if (dim(acc_events)[1] == 0){
    message (paste0('This survey has no Mobile acoustic data present: ', survey, ' GRTS id: ', grts_cell))
  }else{
    message (paste0('Compiling mobile acoustic data for survey: ', survey, ' GRTS id: ', grts_cell))
    wav_files = data.frame()
    acc_events = acc_events %>% dplyr::left_join(geom_df, by= c('grts_geometry_id' = 'event_geometry_id')) %>%
      mutate(site_name = paste0(proj_id_df$grtsId, '_', location_name)) %>%
      dplyr::select(-c(geom_type, geom_coordinates))

    for (x in 1:dim(acc_events)[1]){
      this_site_name = acc_events[x,]$site_name
      event_data_df = as.data.frame(acc_events$values[x], stringsAsFactors = FALSE)
      names(event_data_df) = tolower(gsub("(?<=[a-z0-9])(?=[A-Z])", "_", names(event_data_df), perl = TRUE))

      # Check for no data in this survey acoustic
      if (dim(event_data_df)[1] == 0){
        # message (paste0('Site name ', this_site_name, ' is missing Acoustic values at this survey: ', survey))
        wav_int_files = data.frame()
      }else{
        # print (paste0('Number of wav files at this site ',this_site_name ,': ', dim(event_data_df)[1]))
        if ('recording_location' %in% names(event_data_df)){
          wav_int_files = event_data_df
          wav_int_files['latitude'] = NA
          wav_int_files['longitude'] = NA
          wav_int_files['recording_location'] = FALSE
        }else{
          # Add Lat/Lon for wav files
          wav_int_files = event_data_df %>%
            subset(!is.na(recording_location.geojson.type)) %>%
            dplyr::select(-c(recording_location.geojson.type, recording_location.geojson.coordinates))
          if (dim(wav_int_files)[1]==0){
            wav_int_files = data.frame()
          } else{
            # Extract lat/lon for each recording (wav file)
            lon = matrix(unlist(event_data_df$recording_location.geojson.coordinates), 2)[1,]
            lat = matrix(unlist(event_data_df$recording_location.geojson.coordinates), 2)[2,]
            wav_int_files['latitude'] = lat
            wav_int_files['longitude'] = lon
            wav_int_files['recording_location'] = TRUE
          }
        }
        id  = acc_events[x,]$id
        wav_int_files['mobile_acoustic_values_id'] = id
      }


      # Add accoustic event wav files 1 by 1 to a full dataframe called wav_files
      if (dim(wav_files)[1] < 1){
        wav_files = wav_int_files
      }else {
        wav_files = rbind(wav_files, wav_int_files)
      }
    }

    # If rename = TRUE (The acoustic data exists for this site_name)
    if (dim(wav_files)[1] > 0){
      # Rename and select from the 3 tables
      names(acc_events)[names(acc_events) == 'id'] = 'mobile_acoustic_values_id'
      acc_events = acc_events %>% dplyr::select(-c(values))

      wav_files[,'project_id']   = project_id
      wav_files[,'grts_cell_id'] = proj_id_df$grtsId

      names(wav_files)[names(wav_files) == 'wav_file_name'] = 'audio_recording_name'
      names(wav_files)[names(wav_files) == 'species_id'] = 'auto_id'

      # Merge wav files dataframe and acoustic events dataframe for all data
      wav_n_acc = merge(wav_files, acc_events, by = 'mobile_acoustic_values_id') %>%
        dplyr::rename("survey_start_time" = activation_start_time, "survey_end_time" = activation_end_time)

      # Iteratively combine the wav_n_acc dataframes together for each new survey
      all_wav_n_acc = rbind(all_wav_n_acc, wav_n_acc)
    }
  }
  }
  # Return the combined data in the format of the acoustic stationary bulk upload template form
  if (is.null(year_)){
    return (all_wav_n_acc)
  }else {
    all_wav_n_acc = subset(all_wav_n_acc, format(as.Date(all_wav_n_acc$recording_time), '%Y') == as.integer(year_))
    return(all_wav_n_acc)
  }
}



#' @title Get Bat banding data for States
#'
#' @description
#' Returns a dataframe of all the bat banding data from state/states selected
#'
#' @param token List token created from get_nabat_gql_token() or get_refresh_token()
#' @param states String Vector state or states.  options:
#' 'Alabama','Alaska','Arizona','Arkansas','California','Colorado','Connecticut','Delaware',
#' 'Florida','Georgia','Hawaii','Idaho','Illinois','Indiana','Iowa','Kansas','Kentucky',
#' 'Louisiana','Maine','Maryland','Massachusetts','Michigan','Minnesota','Mississippi',
#' 'Missouri','Montana','Nebraska','Nevada','New Hampshire','New Jersey','New Mexico',
#' 'New York','North Carolina','North Dakota','Ohio','Oklahoma','Oregon','Pennsylvania',
#' 'Rhode Island','South Carolina','South Dakota','Tennessee','Texas','Utah','Vermont',
#' 'Virginia','Washington','West Virgina','Wisconsin','Wyoming'
#' @param branch (optional) String that defaults to 'prod' but can also be 'dev'|'beta'|'local'
#' @param url (optional) String url to use for GQL
#' @param aws_gql (optional) String url to use in aws
#' @param aws_alb (optional) String url to use in aws
#' @param docker (optional) Boolean if being run in docker container or not
#'
#' @keywords bats, NABat, GQL, Surveys
#' @examples
#'
#' \dontrun{
#' survey_df = get_project_surveys(token,
#'                                 project_id)
#' }
#'
#' @export
get_nabat_banding_by_states = function(
  token,
  states,
  branch='prod',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker = FALSE){

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


  final_df = data.frame()
  states_check = c('Alabama','Alaska','Arizona','Arkansas','California','Colorado','Connecticut','Delaware','Florida','Georgia','Hawaii','Idaho','Illinois','Indiana','Iowa','Kansas','Kentucky',
    'Louisiana','Maine','Maryland','Massachusetts','Michigan','Minnesota','Mississippi','Missouri','Montana','Nebraska','Nevada','New Hampshire','New Jersey','New Mexico',
    'New York','North Carolina','North Dakota','Ohio','Oklahoma','Oregon','Pennsylvania','Rhode Island','South Carolina','South Dakota','Tennessee','Texas','Utah','Vermont',
    'Virginia','Washington','West Virgina','Wisconsin','Wyoming')

  for (state in states){
    if (state %in% states_check){
      # Set Query
      query = paste0('query RRbatBanding{ allBatbandings (filter :{state:{equalTo:"',state,'"}}) {
            nodes{
                observers
                captureId
                date
                siteDescr
                state
                countyName
                countyName
                xCoordCentroid
                yCoordCentroid
                xySource
                species
                markRecapture
                unknownBandId
                sex
                age
                reproduction
                forearmLength
                weight
                progrm
            }
          }
          }')
      pbody = list(query = query , operationName = 'RRbatBanding')

      res       = httr::POST(url_, headers_, body = pbody, encode='json')
      content   = httr::content(res, as = 'text')
      state_json = fromJSON(content, flatten = TRUE)
      names(state_json) = substring(names(state_json), 27)

      if (dim(final_df)[1]==0){
        final_df = state_json
      }else {
        final_df = rbind(final_df, state_json)
      }
    }else{
      message(paste0('Error: Spelling for this state is incorrect.. ', states))
    }
  }
  return(final_df)
}



#' @title Get Winter colony count bulk upload template dataframe for a project
#'
#' @description Returns all surveys within a single project (project_id)
#'
#' @param token List token created from get_nabat_gql_token() or get_refresh_token()
#' @param survey_df Dataframe a survey dataframe from the output of get_acoustic_project_summary()
#' @param project_id Numeric or String a project id
#' @param branch (optional) String that defaults to 'prod' but can also be 'dev'|'beta'|'local'
#' @param url (optional) String url to use for GQL
#' @param aws_gql (optional) String url to use in aws
#' @param aws_alb (optional) String url to use in aws
#' @param docker (optional) Boolean if being run in docker container or not
#'
#'
#' @keywords bats, NABat, GQL, Surveys
#'
#' @examples
#'
#' \dontrun{
#' cc_bulk_df = get_colony_bulk_counts(token,
#'                                     get_cc_project_summary(),
#'                                     project_id)
#' }
#'
#' @export
#'
get_colony_bulk_counts = function(
  token,
  survey_df,
  project_id,
  species_df,
  branch = 'prod',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker = FALSE){

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

  sites_query = paste0('
      query RRsites {allSites{
        nodes{
          id,
          siteIdentifier,
          siteTypeBySiteTypeId{
            type
          }
        }
      }
    }')
    pbody = list(query = sites_query)
    res       = httr::POST(url_, headers_, body = pbody, encode='json')
    content   = httr::content(res, as = 'text')
    site_json = fromJSON(content, flatten = TRUE)
    site_type_df = as_tibble(site_json$data$allSites$nodes) %>%
      dplyr::rename('site_id' = id,
        'site_type' = siteTypeBySiteTypeId.type) %>%
      as.data.frame(stringsAsFactors = FALSE)

  # Define package environmental varioables
  if (is.null(pkg.env$bats_df)){
    species_df = get_species(token = token, url = url_, aws_gql = aws_gql, aws_alb = aws_alb, docker = docker)
    assign('bats_df', species_df, pkg.env)
  }

  # Extract all survey ids from survey_df
  survey_ids = unique(survey_df$survey_id)

  # Set empty dataframe to build acoustic stationary bulk template data in
  all_colony_count = data.frame()

  # Query each survey through GQL to extract and build a dataframe with all
  #   acoustic stationary records for these acoustic survey ids
  for (survey in survey_ids){
    token = get_refresh_token(token, url = url_, aws_gql = aws_gql, aws_alb = aws_alb, docker = docker)
    message(paste0('Getting colony count data for survey: ', survey))



    # Set Query
    query = paste0('
      query RRccSurveys {allSurveys (filter:{id:{equalTo:', as.numeric(survey),'}}){
        nodes{
          id
          grtsId
          colonyCountEventsBySurveyId{
            nodes{
              id,
              surveyId,
              dateTimeStart,
              dateTimeEnd,
              siteId,
              winterYearPdPresumed,
              winterYearWnsPresumed,
              siteBySiteId{siteName}
              colonyCountValuesByEventId{
                nodes{
                  id,
                  eventId,
                  speciesId,
                  speciesBySpeciesId{species}
                  countValue
                  countDead
                }
              }
            }
          }
        }
      }
    }')
    pbody = list(query = query, operationName = 'RRccSurveys')

    res       = httr::POST(url_, headers_, body = pbody, encode='json')
    content   = httr::content(res, as = 'text')
    count_json = fromJSON(content, flatten = TRUE)
    count_df = as_tibble(count_json$data$allSurveys$nodes) %>%
      dplyr::select(- id) %>%
      tidyr::unnest(cols = c(colonyCountEventsBySurveyId.nodes)) %>%
      dplyr::select(- id) %>%
      tidyr::unnest(cols = c(colonyCountValuesByEventId.nodes)) %>%
      dplyr::rename('ccId' = id,
        'siteName' = siteBySiteId.siteName,
        'species' = speciesBySpeciesId.species) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      dplyr::left_join(site_type_df, by= c('siteId' = 'site_id'))

    names(count_df) = tolower(gsub("(?<=[a-z0-9])(?=[A-Z])", "_", names(count_df), perl = TRUE))

    all_colony_count = rbind(all_colony_count, count_df)
  }

  all_colony_count_final =   all_colony_count %>%
    dplyr::rename('survey_start_time' = date_time_start,
      'survey_end_time' = date_time_end) %>%
    mutate(survey_start_time = ifelse(nchar(survey_start_time) > 19, NA, survey_start_time)) %>%
    mutate(survey_end_time = ifelse(nchar(survey_end_time) > 19, NA, survey_end_time)) %>%
    subset(!is.na(survey_start_time)) %>%
    clean_time_fields() %>%
    dplyr::rename('date_sampled' = survey_start_time,
      'date_time_end' = survey_end_time,
      'count_alive' = count_value) %>%
    dplyr::left_join(species_df %>% dplyr::select(species_code, id), by = c('species_id' = 'id')) %>%
    dplyr::mutate(
      month = as.integer(format(date_sampled, "%m")),
      year = as.integer(format(date_sampled, "%Y")),
      wyear = case_when(
        month %in% c(1:8) ~ as.integer(year),
        month %in% c(9:12) ~ as.integer(year + 1)
      )) %>%
    dplyr::select(-c(winter_year_pd_presumed, winter_year_wns_presumed))


  return(all_colony_count_final)
}



#' @title Get Upload file preview
#'
#' @description Returns a template to be uploaded with the processing function
#'
#' @param file_path String full path to CSV file for preview
#' @param token List token created from get_nabat_gql_token() or get_refresh_token()
#' @param survey_type (optional) String 'bulk_sae' | 'bulk_mae' | 'bulk_cc'
#' @param branch (optional) String that defaults to 'prod' but can also be 'dev'|'beta'|'local'
#' @param url (optional) String url to use for GQL
#' @param aws_gql (optional) String url to use in aws
#' @param aws_alb (optional) String url to use in aws
#' @param docker (optional) Boolean if being run in docker container or not
#'
#' @export
#'

get_upload_file_preview = function(
  file_path,
  token,
  survey_type = 'bulk_sae',
  branch = 'beta',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker = FALSE){

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
    bucket = NULL
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

  data_type = 'full'
  operation_name = paste0('RR',survey_type,'Preview')
  preview_query = paste0('query ',operation_name,'(
  $surveyType: String
  $data: String
  $transactionUuid: String
  $dataType: String
  $requiredFields: [String]
  $template: JSON
  ) {
  bulkPreview(
  surveyType: $surveyType
  data: $data
  transactionUuid: $transactionUuid
  dataType: $dataType
  requiredFields: $requiredFields
  template: $template
  ) {
  headersDetected
  missing
  matched
  preview
  }
  }')

  # parse file and only upload headers, so that preview takes less time
  tmp_df = read.csv(file_path, stringsAsFactors = FALSE)[1,]
  tmp_file = tempfile()
  write.csv(tmp_df, tmp_file, row.names = FALSE)

  # Remove characters that break
  upload_data = readChar(tmp_file, file.info(tmp_file)$size, useBytes = TRUE)
  upload_data = gsub('\"', '', upload_data, fixed = TRUE)
  upload_data = gsub('\n', '', upload_data, fixed = TRUE)
  upload_data = gsub('\t', '', upload_data, fixed = TRUE)
  upload_data = gsub('List\r',  ' ', upload_data, fixed = TRUE)
  upload_data = gsub('\r',  ' ', upload_data, fixed = TRUE)

  file.remove(tmp_file)

  pr_variables = paste0('{"data" : "',upload_data,'", "dataType" : "',data_type,'", "requiredFields" : [], "surveyType" : "',survey_type,'", "transactionUuid" : "", "template" : "" }')

  pr_pbody = list(query = preview_query, variables = pr_variables, operationName = operation_name)
  pr_res = httr::POST(url_, headers_, body = pr_pbody, encode='json')
  pr_content   = httr::content(pr_res, as = 'text')
  pr_content_json = fromJSON(pr_content, flatten = TRUE)

  return(pr_content_json$data$bulkPreview$matched)
}



#' @title Get presigned data
#'
#' @description Returns a uuid and presigned url to upload a csv into the AWS bucket
#'
#' @param file_path String full path to CSV file for preview
#' @param token List token created from get_nabat_gql_token() or get_refresh_token()
#' @param survey_type (optional) String 'bulk_sae' | 'bulk_mae' | 'bulk_cc'
#' @param branch (optional) String that defaults to 'prod' but can also be 'dev'|'beta'|'local'
#' @param url (optional) String url to use for GQL
#' @param aws_gql (optional) String url to use in aws
#' @param aws_alb (optional) String url to use in aws
#' @param docker (optional) Boolean if being run in docker container or not
#'
#' @export
#'

get_presigned_data = function(
  project_id,
  token,
  branch = 'beta',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker = FALSE){

  if (is.null(url)){
    # Prod URL for NABat GQL
    if (branch ==  'prod'){
      url_ = 'https://api.sciencebase.gov/nabat-graphql/graphql'
      bucket = 'nabat-prod-project-files'
    } else if (branch == 'dev' | branch == 'beta' | branch == 'local'){
      url_ = 'https://nabat-graphql.staging.sciencebase.gov/graphql'
      bucket = 'nabat-beta-project-files'
    }
  }else {
    url_ = url
    bucket = NULL
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

  content_type = 'text/plain'
  key = paste0(project_id, '/bulk-uploads')

  variables = paste0('{"bucket" : "',bucket,'", "key" : "',key,'", "contentType" : "',content_type,'", "asUuid" : "True"}')
  query = 'query RRs3FileServiceUploadFile ($bucket: String!, $key: String!, $contentType: String!, $asUuid: Boolean) {
  s3FileServiceUploadFile(bucket: $bucket, key: $key, contentType: $contentType, asUuid: $asUuid) {
  s3PresignedUrl
  transactionUuid
  success
  message
  }
}'
  pbody = list(query = query, variables = variables, operationName = 'RRs3FileServiceUploadFile')
  res = httr::POST(url_, headers_, body = pbody, encode='json')
  content   = httr::content(res, as = 'text')
  pre_content_json = fromJSON(content, flatten = TRUE)
  asUUid = pre_content_json$data$s3FileServiceUploadFile$transactionUuid
  presigned_url = pre_content_json$data$s3FileServiceUploadFile$s3PresignedUrl

  return (list(asUUid = asUUid, presigned_url = presigned_url))
}


#' @title Upload CSV to aws
#'
#' @description Upload a csv to a presigned url
#'
#' @export
#'

upload_csv = function(
  presigned_url,
  file_path
){
  content_type = 'text/plain'
  headers_put = httr::add_headers('Content-Type' = content_type)
  res_put = httr::PUT(presigned_url,
    body = upload_file(file_path, type = content_type),
    headers_put)
  return(res_put)
}



#' @title Process uploaded CSV
#'
#' @description Upload a csv to a presigned url
#'
#' @export
#'

process_uploaded_csv = function(
  user_id,
  project_id,
  asUUid,
  template,
  file_name,
  token,
  survey_type = 'bulk_sae',
  branch = 'beta',
  url = NULL,
  aws_gql = NULL,
  aws_alb = NULL,
  docker = FALSE
){

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
    bucket = NULL
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
  operation_name = paste0('RR',survey_type,'CsvProcess')
  process_query = paste0('query ',operation_name,' (
    $userId: Int!
    $projectId: Int!
    $transactionUuid: String!
    $fileName: String!
    $type: String!
    $subType: Int
    $template: JSON!
    $requiredFields: JSON
  ) {
    startBulkCsvProcess(
    userId: $userId
    projectId: $projectId
    transactionUuid: $transactionUuid
    fileName: $fileName
    type: $type
    subType: $subType
    template: $template
    requiredFields: $requiredFields
    ) {
    success
    }
  }')

  template_df = template %>% dplyr::select(-c(options)) %>%
    subset(key != 'skip') %>% dplyr::select(key, name)
  template_json = jsonlite::toJSON(template_df)
  # template = gsub('"', "'", template, fixed = TRUE)

  short_name = file_name
  proc_variables = paste0('{"userId" : ',user_id,',
    "projectId" : ',project_id,',
    "transactionUuid" : "',asUUid,'",
    "fileName" : "',short_name,'",
    "type" : "',survey_type,'",
    "template" : ',template_json,' ,
    "subType" : null,
    "requiredFields" : "[]" }')

  pro_pbody = list(query = process_query, variables = proc_variables, operationName = operation_name)

  pro_res = httr::POST(url_, headers_, body = pro_pbody, encode='json')
  pro_content = httr::content(pro_res, as = 'text')
  content_json = fromJSON(pro_content, flatten = TRUE)
  return(pro_content)
  }



