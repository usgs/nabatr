

#' @title Clean Acoustic NABat time fields
#'
#' @description
#' Takes a dataframe with these three fieldnames
#' (recording_time, survey_start_time, and survey_end_time)
#' from NABat database and converts the date format into a POSIX object
#'
#' @param df Dataframe create from either get_sa_bulk_wavs() or
#' get_ma_bulk_wavs()
#'
#' @examples
#'
#' \dontrun{
#' clean_time_fields(acoustic_bulk_df)
#' }
#'
#' @export
#'
clean_time_fields = function(df){
  if ('recording_time' %in% names(df)){
    df$recording_time = gsub("T", " ", df$recording_time, fixed = TRUE)
    df$recording_time = as.POSIXct(df$recording_time,
      tryFormats = c('%Y-%m-%d' , '%Y-%m-%d %H:%M:%S', '%m/%d/%y %H:%M', '%Y-%m-%d %H:%M:%S+%Z'), tz=Sys.timezone())
  }
  if ('survey_start_time' %in% names(df)){
    df$survey_start_time = gsub("T", " ", df$survey_start_time, fixed = TRUE)
    df$survey_start_time = as.POSIXct(df$survey_start_time,
      tryFormats = c('%Y-%m-%d' ,'%Y-%m-%d %H:%M:%S', '%m/%d/%y %H:%M', '%Y-%m-%d %H:%M:%S+%Z'), tz=Sys.timezone())
  }
  if ('survey_end_time' %in% names(df)){
    df$survey_end_time = gsub("T", " ", df$survey_end_time, fixed = TRUE)
    df$survey_end_time = as.POSIXct(df$survey_end_time,
      tryFormats = c('%Y-%m-%d' ,'%Y-%m-%d %H:%M:%S', '%m/%d/%y %H:%M', '%Y-%m-%d %H:%M:%S+%Z'), tz=Sys.timezone())
  }
  return (df)
}


#' @title Add Observed Night using recording_time
#'
#' @description
#' Adds an observed_night field to the dataframe using the recording_time
#' field.  Uses Noon as the cut off for previous vs current night
#'
#' @param df Dataframe create from either get_sa_bulk_wavs() or
#' get_ma_bulk_wavs()
#'
#' @examples
#'
#' \dontrun{
#' # Be sure to run df = clean_time_fields(df) first
#' df = add_observed_nights(df)
#' }
#'
#' @export
#'
add_observed_nights = function(df){
  # Set a field for observed_night
  if ('recording_time' %in% names(df) & 'POSIXct' %in% class(df$recording_time) ){
    clean_df = df %>%
      dplyr::mutate(observed_night = ifelse(format(recording_time, '%H') >=12,
        format(recording_time, '%Y-%m-%d') ,
        format(recording_time - days(1), '%Y-%m-%d'))) %>%
      dplyr::mutate(observed_night = as.Date(observed_night))
    return(clean_df)
  } else if('recording_time' %in% names(df) & !'POSIXct' %in% class(df$recording_time)){
    message('Field name recording_time is not in POSIXct format.  Run clean_time_fields() against your dataframe first')
  } else{
    message('Field name recording_time is not found in input dataframe')
  }
}


#' @title Add Start and End nights for an Acoustic Survey
#'
#' @description
#' Uses survey_start_time and survey_end_time to create 2 new fields
#' (survey_night_start and survey_night_end) from cleaned acoustic data.
#' Be sure to run clean_time_frames() on the input dataframe before
#' running this function.
#'
#' @param df Dataframe create from either get_sa_bulk_wavs() or
#' get_ma_bulk_wavs()
#'
#' @examples
#'
#' \dontrun{
#' # Be sure to run df = clean_time_fields(df) first
#' df = add_start_end_nights(df)
#' }
#'
#' @export
#'
add_start_end_nights = function(df){
  if('survey_start_time' %in% names(df) & 'survey_end_time' %in% names(df)){
    clean_df = df %>%
      dplyr::mutate(survey_night_start = ifelse(format(survey_start_time, '%H') >=12,
        format(survey_start_time, '%Y-%m-%d') ,
        format(survey_start_time - days(1), '%Y-%m-%d'))) %>%
      dplyr::mutate(survey_night_end = ifelse(format(survey_end_time, '%H') >=12,
        format(survey_end_time, '%Y-%m-%d') ,
        format(survey_end_time - days(1), '%Y-%m-%d'))) %>%
      dplyr::mutate(survey_night_start = as.Date(survey_night_start)) %>%
      dplyr::mutate(survey_night_end = as.Date(survey_night_end))
    return(clean_df)
  }else{
    message('Missing either survey_start_time or survey_end_time in dataframe fields')
  }
}


#' @title Move Column in dataframe to other Column location
#'
#' @description
#' Moves one column with field name x to the location of different
#' field name y
#'

move_col = function(data, cols, ref, side = c("before","after")){
  if(! requireNamespace("dplyr")) stop("Make sure package 'dplyr' is installed to use function 'move'")
  side = match.arg(side)
  cols = rlang::enquo(cols)
  ref  = rlang::enquo(ref)
  if(side == "before") dplyr::select(data,1:!!ref,-!!ref,-!!cols,!!cols,dplyr::everything()) else
    dplyr::select(data,1:!!ref,-!!cols,!!cols,dplyr::everything())
}

#' @title Create a vector of letters
#'
#' @description
#' Returns a vector of letters like 'a','b','c' if length.out = 3
#'
myLetters = function(length.out) {
  a = rep(letters, length.out = length.out)
  grp = cumsum(a == "a")
  vapply(seq_along(a),
    function(x) paste(rep(a[x], grp[x]), collapse = ""),
    character(1L))
}


#' @title Get Stationary Acoustic Results for Report
#'
#' @description
#' Get the county and state for a lat/lon wgs84 point
#'
#' @export
#'

ll_to_county_state = function(points_df) {
  # type = 'state' | 'county'
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  this_map =  map('county' , fill=TRUE, col="transparent", plot=FALSE)
  # Upper case to first letter in states or counties
  IDs = sapply(sapply(strsplit(this_map$names, ":"), function(x) x[1]), .simpleCap)
  names(IDs) = NULL
  states_sp = map2SpatialPolygons(this_map, IDs=IDs,proj4string=CRS("+proj=longlat +datum=WGS84"))
  # Convert points_df to a SpatialPoints object
  points_sp = SpatialPoints(points_df, proj4string=CRS("+proj=longlat +datum=WGS84"))
  # Use 'over' to get _indices_ of the Polygons object containing each point
  indices = over(points_sp, states_sp)
  # Return the state or county names of the Polygons object containing each point
  stateNames = sapply(states_sp@polygons, function(x) x@ID)
  return (stateNames[indices])
}

#' @title Get Stationary Acoustic Results for Report
#'
#' @description
#' Combine the spatial information (states and counties) with the detector info and species detected
#'
#' @export
#'
.simpleCap = function(x) {
  s = strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
    sep = "", collapse = " ")
}



#' @title Get GQL url
#'
#' @description
#' Combine the spatial information (states and counties) with the detector info and species detected
#'
#' @export
#'
get_gql_url =  function(
  branch = 'prod'){
    if (branch == 'prod'){
      url_ = 'https://api.sciencebase.gov/nabat-graphql/graphql'
    } else if (branch == 'dev' | branch == 'beta' | branch == 'local'){
      url_ = 'https://nabat-graphql.staging.sciencebase.gov/graphql'
    }
  return(url_)
}

#' @title Get Project file bucket for AWS
#'
#' @description
#' Combine the spatial information (states and counties) with the detector info and species detected
#'
#' @export
#'
get_project_file_bucket = function(
  branch = 'prod'){
  if (branch == 'prod'){
    bucket = 'nabat-prod-project-files'
  } else if (branch == 'dev' | branch == 'beta' | branch == 'local'){
    bucket = 'nabat-beta-project-files'
  }
  return(bucket)
}

