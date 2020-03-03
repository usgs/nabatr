
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


myLetters = function(length.out) {
  a = rep(letters, length.out = length.out)
  grp = cumsum(a == "a")
  vapply(seq_along(a),
    function(x) paste(rep(a[x], grp[x]), collapse = ""),
    character(1L))
}
