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
#' @description
#' Get a NABat GQL token to use for queries
#' @param username String your NABat username from https://sciencebase.usgs.gov/nabat/#/home
#' @param password (optional) String it will prompt you for your password
#' @keywords bats, NABat, GQL
#' @examples
#'
#' nabat_gql_token = get_nabat_gql_token(username = 'NABat_Username')
#' -- Prompts for password
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
  # Prod URL for NABat GQL
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

  # Return token
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
#' @param token String token created from get_nabat_gql_token function
#' @param username String your NABat username from https://sciencebase.usgs.gov/nabat/#/home
#' @keywords bats, NABat, GQL
#' @examples
#'
#' project_df = get_projects(username = 'NABat_Username',
#'                           token    = 'generated-nabat-gql-token')
#'
#' @export
#'
get_projects = function(token, username){

  # Create cli using NABat prod url and ghql library
  url = 'https://api.sciencebase.gov/nabatmonitoring-survey/graphql'
  cli = GraphqlClient$new(url = url,
                          headers = add_headers(.headers = c(Authorization = paste0('Bearer ', token),
                                                             'X-email-address' = username)))
  # Set empty Query
  qry = Query$new()
  # Build query for all projects under user
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
  proj_dat  = cli$exec(qry$queries$projIds)
  proj_json = fromJSON(proj_dat, flatten = TRUE)
  proj_df   = rename_project_df(as.data.frame(proj_json))
  # Return dataframe of projects
  return (proj_df)
}



#' @title Get a project's Stationary Acoustic Surveys
#'
#' @description
#' Returns all surveys within a single project (project_id)
#' @param token String token created from get_nabat_gql_token function
#' @param username String your NABat username from https://sciencebase.usgs.gov/nabat/#/home
#' @param project_id Numeric or String a project id
#' @keywords bats, NABat, GQL, Surveys
#' @examples
#'
#' survey_df = get_project_surveys(token      = 'generated-nabat-gql-token',
#'                                 username   = 'NABat_Username',
#'                                 project_id = 'number or string of a number')
#'
#' @export
get_project_surveys = function(token, username, project_id){

  # Create cli using NABat prod url and ghql library
  url = 'https://api.sciencebase.gov/nabatmonitoring-survey/graphql'
  cli = GraphqlClient$new(url = url,
                          headers = add_headers(.headers = c(Authorization = paste0('Bearer ', token),
                                                             'X-email-address' = username)))
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
  return (survey_df)
}



#' @title Get Acoustic stationary bulk upload template dataframe for a project
#'
#' @description
#' Returns all surveys within a single project (project_id)
#' @param token String token created from get_nabat_gql_token function
#' @param username String your NABat username from https://sciencebase.usgs.gov/nabat/#/home
#' @param survey_df Dataframe a survey dataframe from the output of get_project_surveys
#' @param project_id Numeric or String a project id
#' @keywords bats, NABat, GQL, Surveys
#' @examples
#'
#' acoustic_bulk_df = get_acoustic_bulk_wavs(username   = 'NABat_Username',
#'                                         token      = 'generated-nabat-gql-token',
#'                                         survey_df  = 'dataframe from output of get_project_surveys()',
#'                                         project_id = 'number or string of a number')
#'
#' @export
get_acoustic_bulk_wavs = function(token, username, survey_df, project_id){

  # Create cli using NABat prod url and ghql library
  url = 'https://api.sciencebase.gov/nabatmonitoring-survey/graphql'
  cli = GraphqlClient$new(url = url,
                          headers = add_headers(.headers = c(Authorization = paste0('Bearer ', token),
                                                             'X-email-address' = username)))

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

    # Build wave files dataframe or raise error message if survey has no data
    if (dim(acc_events)[1] == 0){
      message (paste0('This survey has no Sationary acoustic data present: ', survey))
    }else{
      message (paste0('Compiling stationary acoustic data for survey: ', survey))
      wav_files = data.frame()
      for (x in 1:dim(acc_events)[1]){
        wav_int_files  = as.data.frame(acc_events$stationaryAcousticValuesBySaSurveyId.nodes[x])
        id       = acc_events[x,]$id
        wav_int_files['stationary_acoustic_values_id'] = id
        if (dim(wav_files)[1] <1){
          wav_files = wav_int_files
        }else {
          wav_files = rbind(wav_files, wav_int_files)
        }
      }

      # Rename and select from the 3 tables
      proj_id_rn    = rename_acoustic_df(proj_id_df)[,c('stationary_acoustic_values_id', 'project_id', 'grts_cell_id')]
      wav_files_rn  = rename_acoustic_df(wav_files)[,c('audio_recording_name', 'recording_time', 'software_id', 'auto_id',
                                                       'manual_id', 'stationary_acoustic_values_id')]
      acc_events_rn = rename_acoustic_df(acc_events)[,c('stationary_acoustic_values_id', 'location_name', 'survey_start_time',
                                                        'survey_end_time', 'device_id', 'microphone_id' ,'microphone_orientation',
                                                        'microphone_height', 'distance_to_nearest_clutter', 'clutter_type_id',
                                                        'distance_to_nearest_water', 'water_type', 'percent_clutter', 'habitat_type_id')]

      # Set values for survey, project, and grts ids in dataframe
      wav_files_rn[,'survey_id']    = survey
      wav_files_rn[,'project_id']   = project_id
      wav_files_rn[,'grts_cell_id'] = proj_id_df$grtsId

      # Merge wav files dataframe and acoustic events dataframe for all data
      wav_n_acc = merge(wav_files_rn, acc_events_rn, by= 'stationary_acoustic_values_id')

      # Iteratively combine the wav_n_acc dataframes together for each new survey
      all_wav_n_acc = rbind(all_wav_n_acc, wav_n_acc)
    }
  }
  # Return the combined data in the format of the acoustic stationary bulk upload template form
  return (all_wav_n_acc)
}




