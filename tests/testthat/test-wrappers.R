test_that("wrapper functions can be called by splicing a list of query options", {
  q <- list(request = "getStationList", station_no = "00018")
  get_wdo_list(!!!q)
  # TODO repeat for get_timeseries_values
})

test_that("list responses are always returned as a tibble", {
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

test_that("timeseries values are always returned as a tibble", {
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
  expect_equal(dim(ts2), c(1, 6))
  expect_equal(names(ts1), names(ts2))
  expect_equal(purrr::map(ts1, class), purrr::map(ts2, class))
})

test_that("column type conversions are applied", {
  # TODO test 'coverage' -> 'from' and 'to' with getTimeseriesList

  df1 <- get_timeseries_values(
    ts_id = 608153010,
    from = "2021-01-01",
    to = "2021-01-01",
    md_returnfields = c("station_no", "station_id", "station_latitude", "station_longitude")
  )

  expect_type(df1$station_no, "character")
  expect_type(df1$station_id, "integer")
  expect_type(df1$station_latitude, "double")
  expect_type(df1$station_longitude, "double")
  expect_type(df1$Value, "double")
  expect_true(is.factor(df1$`Quality Code`))
  expect_type(is.factor(df1$`Interpolation Type`))
  expect_true(lubridate::is.timepoint(df1$Timestamp))

  # Additional cols
  df2 <- get_timeseries_list(
    station_no = "00018",
    returnfields = c("station_name", "ts_id", "coverage"))

  expect_true(lubridate::is.timepoint(df2$from))
  expect_true(lubridate::is.timepoint(df2$to))

})

test_that("the return fields are idential for all timeseries", {
  # TODO
  # Don't know how to phrase, but I'm considering the case where different
  # timeseries might have different return fields (e.g. because of optional args)

  # ts <- get_timeseries_values(ts_id = c(608153010, 425964010),
  #                             from = "2021-01-01",
  #                             to = "2021-01-02")
})

test_that("Returned columns always match md_returnfields", {
  # TODO find an example when a (valid) metadata option is NULL for a given
  # timeseries. Request at the same time as another sereies which is non null
})

test_that("null values are converted to NAs in timeseries", {
  # TODO
  # This has introduced errors before
})

test_that("A tibble is returned even when metadata isn't specified", {
  # TODO
  # Include invalid metadata
})

test_that("empty timeseries metadata is maintained in timeseries tibble", {
  # TODO
  # i.e. check that there is a row with no timeseries attached
  # And when no timeseries is returned at all
})

test_that("returnfields always match the returned columns for list requests", {
  # TODO
  # Try and trick it with custom attributes etc.
})

test_that("returnfields always match the returned columns for timeseries requests", {
  # TODO
  # Try and trick it with custom attributes etc.
  # Make sure there are explicit NA columns when nothing is returned for
  # custom attributes
})

test_that("A warning is raised when there is no metadata key for the timeseries", {
  # TODO
  # Could use janitor check row key tool, or just specify specific metadata options
})

test_that(".return can be specified from wrappers", {
  # TODO make sure it covers timeseries and list requests
})

test_that("A warning is raised when wrapper functions don't recognise a query option", {
  # TODO This is mostly to avoid silently dropping query options when the
  # query is otherwise valid. This will be easiest to do using wdo_query_fields.
})
