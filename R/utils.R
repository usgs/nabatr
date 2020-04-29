

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
    df$recording_time = as.POSIXct(df$recording_time, tryFormats = c('%Y-%m-%d %H:%M:%S', '%m/%d/%y %H:%M', '%Y-%m-%d %H:%M:%S+%Z'), tz=Sys.timezone())
  }else{
    message('recording_time field not found')
  }
  if ('survey_start_time' %in% names(df)){
    df$survey_start_time = gsub("T", " ", df$survey_start_time, fixed = TRUE)
    df$survey_start_time = as.POSIXct(df$survey_start_time, tryFormats = c('%Y-%m-%d %H:%M:%S', '%m/%d/%y %H:%M', '%Y-%m-%d %H:%M:%S+%Z'), tz=Sys.timezone())
  }else{
    message('survey_start_time field not found')
  }
  if ('survey_end_time' %in% names(df)){
    df$survey_end_time = gsub("T", " ", df$survey_end_time, fixed = TRUE)
    df$survey_end_time = as.POSIXct(df$survey_end_time, tryFormats = c('%Y-%m-%d %H:%M:%S', '%m/%d/%y %H:%M', '%Y-%m-%d %H:%M:%S+%Z'), tz=Sys.timezone())
  }else{
    message('survey_end_time field not found')
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

