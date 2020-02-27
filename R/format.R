#' @title format tidy data to industry wide format
#' @description format tidy data to industry wide format with columns as half hours and rows as days
#' @param x a tibble from \code{\link{generate_signal}}
#' @return a tibble
#' @examples
#' \dontrun{
#' if(interactive()){
#'  generate_signal() %>% format_wide()
#'  }
#' }
#' @seealso
#'  \code{\link[lubridate]{date}}
#' @rdname format_wide
#' @export
#' @importFrom lubridate date

format_wide <- function(x){
  as_tibble(x) %>%
    arrange(date_time) %>%
    mutate(date = lubridate::date(date_time)) %>%
    group_by(date) %>%
    mutate(HH = paste0("HH_", row_number())) %>%
    pivot_wider(id_cols = date, names_from = HH, values_from = signal)
}
