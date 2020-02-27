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

# Global Variables for NABatR
pkg.env = new.env()
pkg.env$bats_df = NULL

#' @title NABat GRTS lookup list with csvs of coordinates
#'
#' @description
#' Used to grab correct coordinates for GRTS lookups
#' @keywords GRTS, spatial, NABat
#' @examples
#'
#' \dontrun{
#' nabatr::grts_lookup_df
#' }
#'
#' @export
#'
grts_lookup_df = list('Canada' = read.csv(paste0('data/GRTS_coords_Canada.csv'), stringsAsFactors=FALSE),
  'Alaska' = read.csv(paste0('data/GRTS_coords_Alaska.csv'), stringsAsFactors=FALSE),
  'Mexico' = read.csv(paste0('data/GRTS_coords_Mexico.csv'), stringsAsFactors=FALSE),
  'Puerto Rico' = read.csv(paste0('data/GRTS_coords_Puerto_Rico.csv'), stringsAsFactors=FALSE),
  'Hawaii' = read.csv(paste0('data/GRTS_coords_Hawaii.csv'), stringsAsFactors=FALSE),
  'Continental US' = read.csv(paste0('data/GRTS_coords_CONUS.csv'), stringsAsFactors=FALSE))


#' @title Get the bat species lookup table
#'
#' @description
#' Reads in dataframe for NABat species lookup table
#'
#' @param token String token created from get_nabat_gql_token function
#' @param branch (optional) String that defaults to 'prod' but can also be 'dev'|'beta'|'local'
#' @keywords species, bats, NABat
#' @examples
#'
#' @export
#'
get_species = function(token, branch = 'prod', url = NULL, aws_gql = NULL, aws_alb = NULL){

  # When url is not passed in use these two gql urls, otherwise use the url passed through
  #  as a variable.
  if (is.null(url)){
    # Prod URL for NABat GQL
    if (branch == 'prod'){
      url = 'https://api.sciencebase.gov/nabat-graphql/graphql'
    } else if (branch == 'dev' | branch == 'beta' | branch == 'local'){
      url = 'https://nabat-graphql.staging.sciencebase.gov/graphql'
    }
  }else {
    url = url
  }

  if (!is.null(aws_gql)){
    print ('GQL using alb_url and gql_query_endpoint')
    cli = GraphqlClient$new(url = paste0(aws_alb, '/graphql'),
      headers = add_headers(.headers = c(Authorization = paste0('Bearer ', token), host = aws_gql)))
  }else {
    cli = GraphqlClient$new(url = url,
      headers = add_headers(.headers = c(Authorization = paste0('Bearer ', token))))
  }

  # Set empty Query
  qry = Query$new()
  # Build query for all projects under user
  qry$query('speciesDf',
    paste0('{ allSpecies{
      nodes{
        id
        speciesCode
        species
        commonName
      }
    }}'))

  # Build dataframe of project data to return
  species_dat  = cli$exec(qry$queries$speciesDf)
  species_json = fromJSON(species_dat, flatten = TRUE)
  species_df   = rename_species_df(as.data.frame(species_json))

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
#' @param username String your NABat username from https://sciencebase.usgs.gov/nabat/#/home
#' @param password (optional) String it will prompt you for your password
#' @param branch (optional) String that defaults to 'prod' but can also be 'dev'|'beta'|'local'
#' @keywords bats, NABat, GQL
#' @examples
#'
#' \dontrun{
#' nabat_gql_token = get_nabat_gql_token(username = 'NABat_Username')
#' -- Prompts for password
#' }
#'
#' @export
#'
get_nabat_gql_token = function(username, password = NULL, branch = 'prod', url = NULL){

  # Prompts password input incase password isn't included in function call
  if (is.null(password)){
    password = .rs.askForPassword('Password')
  }

  # Returns a message with username
  message(paste0("Logging into the NABat database as ", username))

  # When url is not passed in use these two, otherwise use the url passed through
  #  as a variable.
  if (is.null(url)){
    # Prod URL for NABat GQL
    if (branch == 'prod'){
      url = 'https://api.sciencebase.gov/nabat-graphql/graphql'
    } else if (branch == 'dev' | branch == 'beta' | branch == 'local'){
      url = 'https://nabat-graphql.staging.sciencebase.gov/graphql'
    }
  }else {
    url = url
  }

  # Username and password
  variables = paste0('{"l":{"userName" : "',username,'", "password" : "',password,'"}}')
  # Mutation to get token
  query = 'mutation loging($l:LoginInput!){
    login(input:$l){
      token
    }
  }'
  # Finalize json request
  pbody = list(query = query, variables = variables)
  # POST to url
  res = POST(url, body = pbody, encode="json")
  print (res)
  # Remove variables with Password
  rm(password, variables, pbody)
  # Extract token
  content = content(res)
  error  = content$data$login$error
  bearer = content$data$login$token

  if (is.null(error)){
    token = strsplit(bearer, 'Bearer ')[[1]][2]
    if (is.na(token)){
      message('Error, no token returned. Issue regarding Username/Password combo.  Be sure to use the same NABat Username/Password for logging into https://sciencebase.usgs.gov/nabat')
      return (content)
      }else {
      message("Returning a GQL token for NABat.")
      return (token)
    }
  } else {
    # Post message with error for user
    message(paste0("Error: ", error))
    return (content)
  }
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
#' @param token String token created from get_nabat_gql_token function
#' @keywords bats, NABat, GQL
#' @examples
#'
#' \dontrun{
#' project_df = get_projects(token = 'generated-nabat-gql-token')
#' }
#'
#' @export
#'
get_projects = function(token, branch ='prod', url = NULL, aws_gql = NULL, aws_alb = NULL){

  # When url is not passed in use these two gql urls, otherwise use the url passed through
  #  as a variable.
  if (is.null(url)){
    # Prod URL for NABat GQL
    if (branch == 'prod'){
      url = 'https://api.sciencebase.gov/nabat-graphql/graphql'
    } else if (branch == 'dev' | branch == 'beta' | branch == 'local'){
      url = 'https://nabat-graphql.staging.sciencebase.gov/graphql'
    }
  }else {
    url = url
  }

  if (!is.null(aws_gql)){
    print ('GQL using alb_url and gql_query_endpoint')
    print ('URL:')
    print (paste0(aws_alb, '/graphql'))
    print ('host:')
    print (aws_gql)
    cli = GraphqlClient$new(url = paste0(aws_alb, '/graphql'),
      headers = add_headers(.headers = c(Authorization = paste0('Bearer ', token), host = aws_gql)))
  }else {
    cli = GraphqlClient$new(url = url,
      headers = add_headers(.headers = c(Authorization = paste0('Bearer ', token))))
  }

  # Sample frame lookup
  sample_frame_df = data.frame(ids = c(12,14,15,19,20,21),
    sample_frame_short = c('Mexico', 'Continental US', 'Hawaii', 'Canada', 'Alaska', 'Puerto Rico'),
    sample_frame_description = c('Mexico 10x10km Grid', 'Conus (Continental US) 10x10km Grid', 'Hawaii 5x5km Grid', 'Canada 10x10km Grid',
      'Alaska 10x10km Grid', 'Puerto Rico 5x5km Grid'))

  # Set empty Query
  qry = Query$new()
  # Build query for all projects under user
  qry$query('projIds',
            paste0('{allProjects{
                       nodes{
                         id
                         projectName
                         projectKey
                         description
                         mrOwnerEmail
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
                     }'))

  # Build dataframe of project data to return
  proj_dat  = cli$exec(qry$queries$projIds)
  proj_json = fromJSON(proj_dat, flatten = TRUE)
  proj_df   = rename_project_df(as.data.frame(proj_json)) %>% left_join(sample_frame_df, by = c('sample_frame_id' = 'ids'))

  # Define package environmental varioables
  print ('Setting species_df environmental variable')
  if (is.null(pkg.env$bats_df)){
    species_df = get_species(token = token, aws_gql = aws_gql, aws_alb = aws_alb)
    assign('bats_df', species_df, pkg.env)
  }

  # Return dataframe of projects
  return (proj_df)
}



#' @title Get a project's Stationary Acoustic Surveys
#'
#' @description
#' Returns all surveys within a single project (project_id)
#' @param token String token created from get_nabat_gql_token function
#' @param project_df Dataframe output from get_projects()
#' @param project_id Numeric or String a project id
#' @keywords bats, NABat, GQL, Surveys
#' @examples
#'
#' \dontrun{
#' survey_df = get_project_surveys(token      = 'generated-nabat-gql-token',
#'                                 project_id = 'number or string of a number')
#' }
#'
#' @export
get_project_surveys = function(token, project_df, project_id, branch ='prod', url = NULL, aws_gql = NULL, aws_alb = NULL){

  # When url is not passed in use these two gql urls, otherwise use the url passed through
  #  as a variable.
  if (is.null(url)){
    # Prod URL for NABat GQL
    if (branch == 'prod'){
      url = 'https://api.sciencebase.gov/nabat-graphql/graphql'
    } else if (branch == 'dev' | branch == 'beta' | branch == 'local'){
      url = 'https://nabat-graphql.staging.sciencebase.gov/graphql'
    }
  }else {
    url = url
  }

  if (!is.null(aws_gql)){
    print ('GQL using alb_url and gql_query_endpoint')
    cli = GraphqlClient$new(url = paste0(aws_alb, '/graphql'),
      headers = add_headers(.headers = c(Authorization = paste0('Bearer ', token), host = aws_gql)))
  }else {
    cli = GraphqlClient$new(url = url,
      headers = add_headers(.headers = c(Authorization = paste0('Bearer ', token))))
  }

  # Set empty Query
  qry = Query$new()
  # Build query for all surveys under user project
  qry$query('allSurveys',
            paste0('{allSurveys (filter :{projectId:{equalTo:',as.numeric(project_id),'}}){
                       nodes{
                         id
                         projectId
                         grtsId
                       }
                     }
                   }'))
  survey_dat  = cli$exec(qry$queries$allSurveys)
  survey_json = fromJSON(survey_dat, flatten = TRUE)
  survey_df   = rename_survey_df(as.data.frame(survey_json))

  # Define global grts_fname ()
  grts_fname = get_grts_frame_name(project_df, project_id)
  assign('grts_fname', grts_fname, pkg.env)

  # Define package environmental varioables
  if (is.null(pkg.env$bats_df)){
    species_df = get_species(token = token, aws_gql = aws_gql, aws_alb = aws_alb)
    assign('bats_df', species_df, pkg.env)
  }

  return (survey_df)
}



#' @title Get the GRTS Frame name based on project_id and project_df
#'
#' @export
#'
get_grts_frame_name = function(project_df, project_id){
  proj_id = project_id
  project_sample_frame = as.character(subset(project_df, project_df$project_id == proj_id)$sample_frame_short)
  print (paste0('Using ', project_sample_frame, ' as the Frame name for GRTS Cells.'))
  return(project_sample_frame)
}


#' @title Get Acoustic stationary bulk upload template dataframe for a project
#'
#' @description
#' Returns all surveys within a single project (project_id)
#' @param token String token created from get_nabat_gql_token function
#' @param survey_df Dataframe a survey dataframe from the output of get_project_surveys
#' @param project_id Numeric or String a project id
#' @keywords bats, NABat, GQL, Surveys
#' @examples
#'
#' \dontrun{
#' acoustic_bulk_df = get_acoustic_bulk_wavs(token      = 'generated-nabat-gql-token',
#'                                         survey_df  = 'dataframe from output of get_project_surveys()',
#'                                         project_id = 'number or string of a number')
#' }
#'
#' @export
get_acoustic_bulk_wavs = function(token, survey_df, project_id, year = NULL, branch = 'prod', url = NULL, aws_gql = NULL, aws_alb = NULL){

  # When url is not passed in use these two gql urls, otherwise use the url passed through
  #  as a variable.
  if (is.null(url)){
    # Prod URL for NABat GQL
    if (branch == 'prod'){
      url = 'https://api.sciencebase.gov/nabat-graphql/graphql'
    } else if (branch == 'dev' | branch == 'beta' | branch == 'local'){
      url = 'https://nabat-graphql.staging.sciencebase.gov/graphql'
    }
  }else {
    url = url
  }

  if (!is.null(aws_gql)){
    print ('GQL using alb_url and gql_query_endpoint')
    cli = GraphqlClient$new(url = paste0(aws_alb, '/graphql'),
      headers = add_headers(.headers = c(Authorization = paste0('Bearer ', token), host = aws_gql)))
  }else {
    cli = GraphqlClient$new(url = url,
      headers = add_headers(.headers = c(Authorization = paste0('Bearer ', token))))
  }

  # Extract all survey ids from survey_df
  survey_ids = survey_df$survey_id

  # Set empty dataframe to build acoustic stationary bulk template data in
  all_wav_n_acc = data.frame()

  # Query each survey through GQL to extract and build a dataframe with all
  #   acoustic stationary records for these acoustic survey ids
  for (survey in survey_ids){
    qry = Query$new()
    qry$query('grtsIds', paste0('{
      allSurveys (filter :{id:{equalTo:', as.numeric(survey),'}}){
      nodes{
      id
      projectId
      grtsId
      stationaryAcousticEventsBySurveyId {
        nodes{
          id
          locationName
          surveyId
          location{
            geojson
          }
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
      }
  }'))


    # Execute GRTS GQL Query
    grts_dat  = cli$exec(qry$queries$grtsIds)
    grts_json = fromJSON(grts_dat, flatten = TRUE)

    proj_id_df  = as.data.frame(grts_json$data$allSurveys$nodes)
    acc_events = as.data.frame(proj_id_df$stationaryAcousticEventsBySurveyId.nodes)


    # Get grts cell for this survey
    grts_cell = subset(survey_df, survey_df$survey_id == survey)$grts_cell_id

    # Build wave files dataframe or raise error message if survey has no data
    if (dim(acc_events)[1] == 0){
      message (paste0('This survey has no Sationary acoustic data present: ', survey, ' GRTS id: ', grts_cell))
    }else{
      message (paste0('Compiling stationary acoustic data for survey: ', survey, ' GRTS id: ', grts_cell))
      wav_files = data.frame()
      acc_events = acc_events %>% mutate(site_name = paste0(proj_id_df$grtsId, '_', acc_events$locationName))
      for (x in 1:dim(acc_events)[1]){
        rename = TRUE
        this_site_name = acc_events[x,]$site_name
        # Check for no data in this survey acoustic
        if (dim(as.data.frame(acc_events$stationaryAcousticValuesBySaSurveyId.nodes[x]))[1] == 0){
          message (paste0('Site name ', this_site_name, ' is missing Acoustic values at this survey: ', survey))
          rename = FALSE
        }else{
        }

        if ('location.geojson.coordinates' %in% names(acc_events) & !is.null(acc_events$location.geojson.coordinates[x][[1]])){
          lon = as.data.frame(acc_events$location.geojson.coordinates[x])[,1][1]
          lat = as.data.frame(acc_events$location.geojson.coordinates[x])[,1][2]
        }else {
          lon = NA
          lat = NA
        }
        wav_int_files  = as.data.frame(acc_events$stationaryAcousticValuesBySaSurveyId.nodes[x], stringsAsFactors=FALSE)
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

      # If rename = TRUE (The acoustic data exists for this site_name)
      if (rename){
        # Rename and select from the 3 tables
        proj_id_rn    = rename_acoustic_df(proj_id_df)[,c('stationary_acoustic_values_id', 'project_id', 'grts_cell_id')]
        wav_files_rn  = rename_acoustic_df(wav_files)[,c('audio_recording_name', 'recording_time', 'software_id', 'auto_id',
          'manual_id', 'stationary_acoustic_values_id', 'latitude', 'longitude')]
        acc_events_rn = rename_acoustic_df(acc_events)[,c('stationary_acoustic_values_id', 'location_name', 'survey_start_time',
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
  if (is.null(year)){
    return (all_wav_n_acc)
  }else {
    all_wav_n_acc = subset(all_wav_n_acc, format(as.Date(all_wav_n_acc$recording_time), '%Y') == year )
    return(all_wav_n_acc)
  }
}



#' @title Get Bat banding data for States
#'
#' @description
#' Returns a dataframe of all the bat banding data from that/those states
#' @param token String token created from get_nabat_gql_token function
#' @param project_id Numeric or String a project id
#' @keywords bats, NABat, GQL, Surveys
#' @examples
#'
#' \dontrun{
#' survey_df = get_project_surveys(token      = 'generated-nabat-gql-token',
#'                                 project_id = 'number or string of a number')
#' }
#'
#' @export
get_nabat_banding_by_states = function(token, states, branch='prod', url = NULL, aws_gql = NULL, aws_alb = NULL){

  # When url is not passed in use these two gql urls, otherwise use the url passed through
  #  as a variable.
  if (is.null(url)){
    # Prod URL for NABat GQL
    if (branch == 'prod'){
      url = 'https://api.sciencebase.gov/nabat-graphql/graphql'
    } else if (branch == 'dev' | branch == 'beta' | branch == 'local'){
      url = 'https://nabat-graphql.staging.sciencebase.gov/graphql'
    }
  }else {
    url = url
  }

  if (!is.null(aws_gql)){
    print ('GQL using alb_url and gql_query_endpoint')
    cli = GraphqlClient$new(url = paste0(aws_alb, '/graphql'),
      headers = add_headers(.headers = c(Authorization = paste0('Bearer ', token), host = aws_gql)))
  }else {
    cli = GraphqlClient$new(url = url,
      headers = add_headers(.headers = c(Authorization = paste0('Bearer ', token))))
  }

  final_df = data.frame()
  states_check = c('Alabama','Alaska','Arizona','Arkansas','California','Colorado','Connecticut','Delaware','Florida','Georgia','Hawaii','Idaho','Illinois','Indiana','Iowa','Kansas','Kentucky',
    'Louisiana','Maine','Maryland','Massachusetts','Michigan','Minnesota','Mississippi','Missouri','Montana','Nebraska','Nevada','New Hampshire','New Jersey','New Mexico',
    'New York','North Carolina','North Dakota','Ohio','Oklahoma','Oregon','Pennsylvania','Rhode Island','South Carolina','South Dakota','Tennessee','Texas','Utah','Vermont',
    'Virginia','Washington','West Virgina','Wisconsin','Wyoming')

  for (state in states){
    if (state %in% states_check){
      qry = Query$new()
      print (state)
      qry$query('bandingData',
        paste0('{ allBatbandings (filter :{state:{equalTo:"',state,'"}}) {
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
      }'))
      proj_dat  = cli$exec(qry$queries$bandingData)
      proj_json = fromJSON(proj_dat, flatten = TRUE)
      proj_df   = as.data.frame(proj_json)
      names(proj_df) = substring(names(proj_df), 27)

      if (dim(final_df)[1]==0){
        final_df = proj_df
      }else {
        final_df = rbind(final_df, proj_df)
      }
    }else{
      message(paste0('Error: Spelling for this state is incorrect.. ', states))
    }
  }
  return(final_df)
}


#' @title Get Winter colony count bulk upload template dataframe for a project
#'
#' @description
#' Returns all surveys within a single project (project_id)
#' @param token String token created from get_nabat_gql_token function
#' @param survey_df Dataframe a survey dataframe from the output of get_project_surveys
#' @param project_id Numeric or String a project id
#' @keywords bats, NABat, GQL, Surveys
#' @examples
#'
#' \dontrun{
#' acoustic_bulk_df = get_acoustic_bulk_wavs(token      = 'generated-nabat-gql-token',
#'                                         survey_df  = 'dataframe from output of get_project_surveys()',
#'                                         project_id = 'number or string of a number')
#' }
#'
#' @export
get_colony_bulk_counts = function(token, survey_df, project_id, branch = 'prod', url = NULL, aws_gql = NULL, aws_alb = NULL){

  # When url is not passed in use these two gql urls, otherwise use the url passed through
  #  as a variable.
  if (is.null(url)){
    # Prod URL for NABat GQL
    if (branch == 'prod'){
      url = 'https://api.sciencebase.gov/nabat-graphql/graphql'
    } else if (branch == 'dev' | branch == 'beta' | branch == 'local'){
      url = 'https://nabat-graphql.staging.sciencebase.gov/graphql'
    }
  }else {
    url = url
  }

  if (!is.null(aws_gql)){
    print ('GQL using alb_url and gql_query_endpoint')
    cli = GraphqlClient$new(url = paste0(aws_alb, '/graphql'),
      headers = add_headers(.headers = c(Authorization = paste0('Bearer ', token), host = aws_gql)))
  }else {
    cli = GraphqlClient$new(url = url,
      headers = add_headers(.headers = c(Authorization = paste0('Bearer ', token))))
  }

  # Define package environmental varioables
  if (is.null(pkg.env$bats_df)){
    species_df = get_species(token = token, aws_gql = aws_gql, aws_alb = aws_alb)
    assign('bats_df', species_df, pkg.env)
  }

  # Extract all survey ids from survey_df
  survey_ids = survey_df$survey_id

  # Set empty dataframe to build acoustic stationary bulk template data in
  all_colony_count = data.frame()

  # Query each survey through GQL to extract and build a dataframe with all
  #   acoustic stationary records for these acoustic survey ids
  for (survey in survey_ids){
    qry = Query$new()

    qry$query('counts', paste0('{
      allSurveys (filter :{id:{equalTo:', as.numeric(survey),'}}){
        nodes{
          id
          grtsId
          colonyCountEventsBySurveyId{
            nodes{
              id
              surveyId
              dateTimeStart
              dateTimeEnd
              colonyCountEventSitesByEventId{
                nodes{
                  id
                  eventId
                  siteId
                  siteBySiteId{siteName}
                  winterYearPdPresumed
                  winterYearWnsPresumed
                  colonyCountSiteValuesByEventSiteId{
                    nodes{
                      id
                      eventSiteId
                      speciesId
                      speciesBySpeciesId{species}
                      countValue
                    }
                  }
                }
              }
            }
          }
        }
      }
      }'))


    # Execute Counts GQL Query
    count_dat <- cli$exec(qry$queries$counts)
    count_json <- fromJSON(count_dat, flatten = TRUE)
    count_tb <- as_tibble(count_json$data$allSurveys$nodes) %>% tidyr::unnest() %>% tidyr::unnest() %>% tidyr::unnest()
    all_colony_count <- rbind(all_colony_count, count_tb)
  }

  all_colony_count_final <- rename_colony_df(all_colony_count) %>%
    mutate(date_sampled = ymd(format(ymd_hms(date_sampled), "%Y-%m-%d")),
           month = as.numeric(format(date_sampled, "%m")),
           year = as.numeric(format(date_sampled, "%Y")),
           wyear = case_when(
             month %in% c(1:8) ~ year,
             month %in% c(9:12) ~ year + 1
           )) %>%
    dplyr::select("grts_id", "date_sampled", "site_name", "winterYearPdPresumed", "winterYearWnsPresumed",
                  "species", "count", "wyear")

  return(all_colony_count_final)

}

