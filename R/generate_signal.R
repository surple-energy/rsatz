#' @title generate a diurnal signal of half hours
#' @description FUNCTION_DESCRIPTION
#' @param days PARAM_DESCRIPTION, Default: 7 * 4
#' @param start PARAM_DESCRIPTION, Default: "2020"
#' @param amplitude PARAM_DESCRIPTION, Default: NA
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
#' @importFrom dplyr mutate

generate_signal <-
  function(days,
           start = "2020",
           amplitude = NA) {
    intervals <- days * 48

    start_date <- as.Date(paste0(start, "-01-01"))

    end_date <- start_date + days

    date_time <- seq(as.POSIXct(start_date),
                     as.POSIXct(end_date),
                     by = "30 min")[1:intervals]

    tibble::tibble(
      date_time = date_time,
      interval = seq(
        from = 0,
        to = (days * 2) * pi,
        length.out = intervals
      ),
      pure = -cos(interval)
    ) -> df

    if(is.na(amplitude)){
      dplyr::mutate(df,
                    signal = pure)
    } else {
      dplyr::mutate(df,
                    signal = amplitude * pure)
    }
  }

#' @title shift a signal by a margin
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param centre_point PARAM_DESCRIPTION
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

shift_signal <- function(x, centre_point) {
  dplyr::mutate(x,
                shift = centre_point,
                signal = signal + shift)
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

add_noise <- function(x, low = 0, high = 1) {
  dplyr::mutate(x,
                noise = runif(nrow(x), low, high),
                signal = signal + noise)
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

  remainder_length <- intervals - (length(seasonality_seq) * annual_length)

  if (remainder_length != 0) {
    dplyr::mutate(
      x,
      seasonality = append(rep(seasonality_seq, annual_length),
                           seasonality_seq[1:remainder_length]),
      signal = signal + seasonality
    )
  } else {
    dplyr::mutate(x,
                  seasonality = rep(seasonality_seq, annual_length),
                  signal = signal + seasonality)
  }
}

#' @title make anomaly features
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param n PARAM_DESCRIPTION, Default: 0.01
#' @param strength PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{sample}},\code{\link[dplyr]{pull}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{case_when}}
#' @rdname add_anomalies
#' @export
#' @importFrom dplyr sample_frac pull mutate case_when

make_anomalies <- function(x, n = 0.01, strength = 1) {

  anomalies <- dplyr::sample_frac(x, n) %>%
    dplyr::pull(date_time)

  dplyr::mutate(
    x,
    anomaly = dplyr::case_when(date_time %in% anomalies ~ strength),
    signal = dplyr::case_when(date_time %in% anomalies ~ signal + anomaly,
                              TRUE ~ signal)
  )
}

#' @title make trend features
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param strength PARAM_DESCRIPTION, Default: 1
#' @param start_interval PARAM_DESCRIPTION, Default: NA
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname add_trend
#' @export
#' @importFrom dplyr mutate

make_trend <- function(x, strength = 10, start_interval = NA) {

  intervals <- nrow(x)

  if (is.na(start_interval)) {
    dplyr::mutate(x,
           trend = seq(from = 0, to = strength, length.out = intervals),
           signal = signal + trend)
  } else {
    na_seq <-
      rep(NA_integer_, start_interval)

    value_seq <- seq(from = 0, to = strength, length.out= intervals - start_interval)

    dplyr::mutate(x,
           trend = append(na_seq, value_seq),
           signal = dplyr::case_when(!is.na(trend) ~ signal + trend,
                                     TRUE ~ signal))
  }
}

#' @title make weekend features
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
#'  \code{\link[dplyr]{mutate}}
#'  \code{\link[lubridate]{day}}
#'  \code{\link[tsibble]{as_tsibble}},\code{\link[tsibble]{slide}}
#' @rdname add_weekends
#' @export
#' @importFrom dplyr mutate case_when
#' @importFrom lubridate wday
#' @importFrom tsibble as_tsibble slide_dbl

make_weekends <- function(x) {
  dplyr::mutate(x,
                weekday = lubridate::wday(date_time, label = TRUE)) %>%
    tsibble::as_tsibble(index = date_time) %>%
    dplyr::mutate(
      sliding_mean_signal = tsibble::slide_dbl(signal, ~ mean(., na.rm = TRUE), .size = 48),
      signal = dplyr::case_when(
        weekday %in% c("Sat", "Sun") & !is.na(sliding_mean_signal) & sliding_mean_signal < signal ~ sliding_mean_signal,
        TRUE ~ signal
        )
      )
}
