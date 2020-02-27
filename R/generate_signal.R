#' @title generate a diurnal signal of half hours
#' @description generate a diurnal signal of half hours over a range of days
#'   starting from the first of the selected year, with varying amplitude
#' @param days number of days to generate signal for as an integer, Default: 7 *
#'   4
#' @param start the year to start from as a string of `yyyy``, Default: "2020"
#' @param amplitude the height of the signal as a numeric, Default: NA
#' @return a tibble
#' @examples
#' \dontrun{
#' if(interactive()){
#'  generate_signal()
#'
#'  generate_signal(365, start = "2019", amplitude = 12)
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
#' @description shift signal by a margin up or down to represent magnitude of
#'   the value
#' @param x a tibble from \code{\link{generate_signal}}
#' @param centre_point the amount to shift up by as a numeric
#' @return a tibble
#' @examples
#' \dontrun{
#' if(interactive()){
#'  generate_signal(5) %>% shift_signal(9)
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
#' @description add noise to a signal with set minimum and maximums
#' @param x a tibble from \code{\link{generate_signal}}
#' @param low lowest noise to add as a numeric, Default: 0
#' @param high highest noise to add as a numeric, Default: 1
#' @return a tibble
#' @examples
#' \dontrun{
#' if(interactive()){
#'  generate_signal() %>% add_noise()
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
#' @description add seasonality to a signal of a specific strength, that can be
#'   high in summer or high in winter, that is exactly a year long
#' @param x a tibble from \code{\link{generate_signal}}
#' @param strength the strength of the change in a seasonal cycle as a numeric,
#'   Default: 5
#' @param invert if the signal should be higher in summer than it is in winter
#'   as a booleen, Default: FALSE
#' @return a tibble
#' @examples
#' \dontrun{
#' if(interactive()){
#'  generate_signal() %>%  add_seasonality()
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate}}
#' @rdname add_seasonality
#' @export
#' @importFrom dplyr mutate

add_seasonality <- function(x, strength = 5, invert = FALSE) {
  if(invert) {
    seasonality_seq <- -cos(seq(
      from = 0,
      to = pi * 2,
      length.out = 365 * 48
    )) * strength
  } else{
    seasonality_seq <- cos(seq(
      from = 0,
      to = pi * 2,
      length.out = 365 * 48
    )) * strength
  }

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
#' @description make anomaly features of varying number that randomly increase
#'   the signal by a specific volume
#' @param x a tibble from \code{\link{generate_signal}}
#' @param n the percentage of the data to insert anomalies into as a numeric <=
#'   1, Default: 0.01
#' @param strength the strength of the anomalies in the signal as a numeric,
#'   Default: 1
#' @return a tibble
#' @examples
#' \dontrun{
#' if(interactive()){
#'  generate_signal() %>% make_anomalies()
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{sample}},\code{\link[dplyr]{pull}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{case_when}}
#' @rdname make_anomalies
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
#' @description make trend features that start at a specific interval that
#'   increase or decrease the signal linearly by a specific strength
#' @param x a tibble from \code{\link{generate_signal}}
#' @param strength the strength of the trend in the signal as a numeric,
#'   Default: 1
#' @param start interval to start on as an integer, Default: NA
#' @return a tibble
#' @examples
#' \dontrun{
#' if(interactive()){
#'  generate_signal() %>% make_trend()
#'  }
#' }
#' @rdname make_trend
#' @export
#' @importFrom dplyr mutate case_when

make_trend <- function(x, strength = 10, start = NA) {

  intervals <- nrow(x)

  if (is.na(start)) {
    dplyr::mutate(x,
           trend = seq(from = 0, to = strength, length.out = intervals),
           signal = signal + trend)
  } else {
    na_seq <-
      rep(NA_integer_, start)

    value_seq <- seq(from = 0, to = strength, length.out= intervals - start)

    dplyr::mutate(x,
           trend = append(na_seq, value_seq),
           signal = dplyr::case_when(!is.na(trend) ~ signal + trend,
                                     TRUE ~ signal))
  }
}

#' @title make weekend features
#' @description make weekend features for saturdays and sundays that are capped
#'   in value to the local average
#' @param x a tibble from \code{\link{generate_signal}}
#' @return a tssibble
#' @examples
#' \dontrun{
#' if(interactive()){
#'  generate_signal() %>% make_weekends()
#'  }
#' }
#' @seealso \code{\link[dplyr]{mutate}} \code{\link[lubridate]{day}}
#' \code{\link[tsibble]{as_tsibble}},\code{\link[tsibble]{slide}}
#' @rdname make_weekends
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
        weekday %in% c("Sat", "Sun") &
          !is.na(sliding_mean_signal) &
          sliding_mean_signal < signal ~ sliding_mean_signal,
        TRUE ~ signal
      )
    )
}
