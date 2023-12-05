# All wrappers ------------------------------------------------------------

test_that("wrappers accept a spliced list of query options", {
  # for the general get_wdo_list
  q1 <- list(request = "getStationList", station_no = "00018")
  expect_no_error(get_wdo_list(!!!q1))

  # for a specific list function
  q2 <- list(station_no = c("00018", "00184"))
  expect_no_error(get_parameter_list(!!!q2))

  # for get_timeseries_values
  q3 <- list(ts_id = 83527010, from = "2021-01-01", to = "2021-01-02")
  expect_no_error(get_timeseries_values(!!!q3))
})

test_that("wrappers apply column type conversions", {
  # most converted cols are in timeseries values
  df1 <- get_timeseries_values(
    ts_id = 608153010,
    from = "2021-01-01",
    to = "2021-01-01",
    md_returnfields = c("station_no", "station_id", "station_latitude",
                        "station_longitude")
  )

  # also look at coverage from timeseries list
  df2 <- get_timeseries_list(
    station_no = "00018",
    returnfields = c("station_name", "ts_id", "coverage"))

  # basic types
  expect_type(df1$station_no, "character")
  expect_type(df1$station_id, "integer")
  expect_type(df1$station_latitude, "double")
  expect_type(df1$station_longitude, "double")
  expect_type(df1$Value, "double")

  # factors
  expect_true(is.factor(df1$`Quality Code`))
  expect_true(is.factor(df1$`Interpolation Type`))

  # timestamps
  expect_true(lubridate::is.timepoint(df1$Timestamp))
  expect_true(lubridate::is.timepoint(df2$from))
  expect_true(lubridate::is.timepoint(df2$to))
})

# Timeseries values -------------------------------------------------------

test_that("empty timeseries are returned as a zero-row tibble", {
  # not empty
  ts1 <- get_timeseries_values(ts_id = 83527010,
                               from = "2021-01-01",
                               to = "2021-01-02")
  # empty, but has metadata
  ts2 <- get_timeseries_values(ts_id = 337486010)

  # compare
  expect_true(tibble::is_tibble(ts1))
  expect_true(tibble::is_tibble(ts2))
  expect_equal(dim(ts1), c(288, 6))
  expect_equal(dim(ts2), c(0, 6))
  expect_equal(names(ts1), names(ts2))
  expect_equal(purrr::map(ts1, class), purrr::map(ts2, class))
})

# Lists -------------------------------------------------------------------

test_that("empty list responses are returned as a zero-row tibble", {
  # Request info on two rainfall stations
  df1 <- get_wdo_list(request = "getStationList",
                      station_no = c("00018", "00184"))

  # Specifying discharge means the response should be empty
  df2 <- get_wdo_list(request = "getStationList",
                      station_no = c("00018", "00184"),
                      parametertype_name = "Water Course Discharge")

  # compare
  expect_true(tibble::is_tibble(df1))
  expect_true(tibble::is_tibble(df2))
  expect_equal(dim(df1), c(2, 5))
  expect_equal(dim(df2), c(0, 5))
  expect_equal(names(df1), names(df2))
  expect_equal(purrr::map(df1, class), purrr::map(df2, class))
})

test_that("get_wdo_list only accepts csv list requests", {
  # not a list
  expect_error(get_wdo_list("getTimeseriesValues"))

  # cannot be returned as a csv
  expect_error(get_wdo_list("getStandardRemarkTypeList"))
})
