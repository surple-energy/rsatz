#' @title generate a diurnal signal of half hours
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[tibble]{tibble}}
#' @rdname pure_signal
#' @export
#' @importFrom tibble tibble

pure_signal <- function(days = 7 * 4, start = "2020-01-01") {
  intervals <- days * 48

  start_date <- as.Date(start)

  end_date <- start_date + days

  date_time <- seq(as.POSIXct(start_date),
                   as.POSIXct(end_date),
                   by = "30 min")[1:intervals]

  df <- tibble::tibble(
    date_time = date_time,
    pintervals = seq(
      from = 0,
      to = (days * 2) * pi,
      length.out = intervals
    ),
    pure = -cos(pintervals),
    signal = pure
  )
  return(df)
}

#' @title shift a signal by a margin
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param centre_point PARAM_DESCRIPTION, Default: 10
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate}}
#' @rdname shift_signal
#' @export
#' @importFrom dplyr mutate

shift_signal <- function(x, centre_point = 10) {

  df <- dplyr::mutate(x,
                      shift = 10,
                      signal = signal + shift)

  return(df)
}

#' @title add noise to a signal
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param low PARAM_DESCRIPTION, Default: 0
#' @param high PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate}}
#' @rdname add_noise
#' @export
#' @importFrom dplyr mutate

add_noise <- function(x, low=0, high = 1) {

  df <- dplyr::mutate(x,
                      noise = runif(nrow(x), low, high),
                      signal = signal + noise)

  return(df)
}

#' @title add seasonality to a signal
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param seasonality_strength PARAM_DESCRIPTION, Default: 5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname add_seasonality
#' @export
#' @importFrom dplyr mutate

add_seasonality <- function(x, seasonality_strength = 5) {
  seasonality_seq <- cos(seq(
    from = 0,
    to = pi * 2,
    length.out = 365 * 48
  )) * seasonality_strength

  intervals <- nrow(x)

  annual_length <- floor(intervals / (365 * 48))

  if (annual_length > 0) {
    remainder_length <- intervals - length(seasonality_seq)

    df <- dplyr::mutate(x,
                        seasonality = append(rep(seasonality_seq, annual_length),
                                             seasonality_seq[1:remainder_length]),
                        signal = signal + seasonality)
  } else {
    df <- dplyr::mutate(x,
                        seasonality = seasonality_seq[1:intervals],
                        signal = signal + seasonality)
  }
  return(df)
}
