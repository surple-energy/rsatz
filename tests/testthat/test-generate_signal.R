test_that("generate signal gives tibble", {
  generate_signal(10) -> x
  expect_equal(class(x),
               c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(x),
               c("date_time", "interval", "pure", "signal"))
  expect_length(x$date_time, 480)

})

test_that("generate signal and shift gives tibble", {
  generate_signal(10) %>%  shift_signal(10) -> x
  expect_equal(class(x),
               c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(x),
               c("date_time", "interval", "pure", "signal", "shift"))
  expect_length(x$date_time, 480)
})

test_that("generate signal and seasonality gives tibble", {
  generate_signal(10) %>% add_seasonality() -> x
  expect_equal(class(x),
               c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(x),
               c("date_time", "interval", "pure", "signal", "seasonality"))
  expect_length(x$date_time, 480)
})

test_that("generate signal  and noise gives tibble", {
  generate_signal(10) %>% add_noise() -> x
  expect_equal(class(x),
               c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(x),
               c("date_time", "interval", "pure", "signal", "noise"))
  expect_length(x$date_time, 480)
})

test_that("generate signal and trend gives tibble", {
  generate_signal(10) %>%  make_trend() -> x
  expect_equal(class(x),
               c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(x),
               c("date_time", "interval", "pure", "signal", "trend"))
  expect_length(x$date_time, 480)
})

test_that("generate signal and weekends gives tibble", {
  generate_signal(10) %>% make_weekends() -> x
  expect_equal(class(x),
               c("tbl_ts", "tbl_df", "tbl", "data.frame"))
  expect_equal(
    names(x),
    c(
      "date_time",
      "interval",
      "pure",
      "signal",
      "weekday",
      "sliding_mean_signal"
    )
  )
  expect_length(x$date_time, 480)
})
