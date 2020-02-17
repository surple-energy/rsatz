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

pure_signal <- function(x) {
  intervals <- x * 48

  df <- tibble::tibble("generated_intervals" = seq(from = 0,
                                               to = (x * 2) * pi,
                                               length.out = intervals),
                       "cos_signal" =-cos(generated_intervals)
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

  df <- dplyr::mutate(x, shifted_signal = cos_signal + 10)

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

  df <- dplyr::mutate(x, noise = runif(nrow(x), low, high))

  return(df)
}
