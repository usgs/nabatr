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
# Created: 2019-9-10
#############################################################################

rename_project_df = function(project_df){
  names(project_df)[names(project_df) == 'data.allProjects.nodes.id']          = 'project_id'
  names(project_df)[names(project_df) == 'data.allProjects.nodes.projectName'] = 'project_name'
  names(project_df)[names(project_df) == 'data.allProjects.nodes.projectKey']  = 'project_key'
  row.names(project_df) = c()
  return (project_df)
}

rename_survey_df = function(survey_df){
  names(survey_df)[names(survey_df) == 'data.allSurveys.nodes.id']        = 'survey_id'
  names(survey_df)[names(survey_df) == 'data.allSurveys.nodes.projectId'] = 'project_id'
  names(survey_df)[names(survey_df) == 'data.allSurveys.nodes.grtsId']    = 'grts_cell_id'
  row.names(survey_df) = c()
  return (survey_df)
}

rename_acoustic_df = function(acoustic_df){
  names(acoustic_df)[names(acoustic_df) == 'projectId']     = 'project_id'
  names(acoustic_df)[names(acoustic_df) == 'recordingTime'] = 'recording_time'
  names(acoustic_df)[names(acoustic_df) == 'grtsId']        = 'grts_cell_id'
  names(acoustic_df)[names(acoustic_df) == 'id']            = 'stationary_acoustic_values_id'
  names(acoustic_df)[names(acoustic_df) == 'locationName']  = 'location_name'
  names(acoustic_df)[names(acoustic_df) == 'surveyId']      = 'survey_id'
  names(acoustic_df)[names(acoustic_df) == 'wavFileName']   = 'audio_recording_name'
  names(acoustic_df)[names(acoustic_df) == 'waterType']     = 'water_type'
  names(acoustic_df)[names(acoustic_df) == 'activationStartTime']       = 'survey_start_time'
  names(acoustic_df)[names(acoustic_df) == 'activationEndTime']         = 'survey_end_time'
  names(acoustic_df)[names(acoustic_df) == 'microphoneHeight']          = 'microphone_height'
  names(acoustic_df)[names(acoustic_df) == 'distanceToClutterMeters']   = 'distance_to_nearest_clutter'
  names(acoustic_df)[names(acoustic_df) == 'softwareBySoftwareId.name'] = 'software_name'
  names(acoustic_df)[names(acoustic_df) == 'distanceToWater']           = 'distance_to_nearest_water'
  names(acoustic_df)[names(acoustic_df) == 'percentClutterMethod']      = 'percent_clutter'
  names(acoustic_df)[names(acoustic_df) == 'clutterTypeByClutterTypeId.description']         = 'clutter_type'
  names(acoustic_df)[names(acoustic_df) == 'habitatTypeByHabitatTypeId.description']         = 'broad_habitat_type'
  names(acoustic_df)[names(acoustic_df) == 'microphoneOrientationByMicrophoneOrientationId'] = 'microphone_orientation'
  names(acoustic_df)[names(acoustic_df) == 'softwareBySoftwareId.versionNumber']             = 'software_version'
  names(acoustic_df)[names(acoustic_df) == 'speciesBySpeciesId.speciesCode']                 = 'auto_id'
  names(acoustic_df)[names(acoustic_df) == 'speciesByManualId.speciesCode']                  = 'manual_id'
  names(acoustic_df)[names(acoustic_df) == 'deviceByDeviceId.deviceTypeByDeviceTypeId.manufacturer']                 = 'device_manufacturer'
  names(acoustic_df)[names(acoustic_df) == 'deviceByDeviceId.deviceTypeByDeviceTypeId.model']                        = 'device_model'
  names(acoustic_df)[names(acoustic_df) == 'microphoneByMicrophoneId.microphoneTypeByMicrophoneTypeId.model']        = 'microphone_model'
  names(acoustic_df)[names(acoustic_df) == 'microphoneByMicrophoneId.microphoneTypeByMicrophoneTypeId.manufacturer'] = 'microphone_manufacturer'
  names(acoustic_df)[names(acoustic_df) == 'speciesId'] = 'auto_id'
  names(acoustic_df)[names(acoustic_df) == 'manualId']  = 'manual_id'
  names(acoustic_df)[names(acoustic_df) == 'id1']       = 'location_id'
  names(acoustic_df)[names(acoustic_df) == 'deviceId']  = 'device_id'
  names(acoustic_df)[names(acoustic_df) == 'microphoneId']            = 'microphone_id'
  names(acoustic_df)[names(acoustic_df) == 'microphoneOrientationId'] = 'microphone_orientation'
  names(acoustic_df)[names(acoustic_df) == 'clutterTypeId']           = 'clutter_type_id'
  names(acoustic_df)[names(acoustic_df) == 'habitatTypeId']           = 'habitat_type_id'
  names(acoustic_df)[names(acoustic_df) == 'softwareId']              = 'software_id'
  row.names(acoustic_df) = c()
  return (acoustic_df)
}




