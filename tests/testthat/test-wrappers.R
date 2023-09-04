test_that("list responses are always returned as a tibble", {

  df <- get_wdo_list(
    request = "getStationList",
    station_no = c("00018", "00184")
  )

  # Same request but with parametertype specified
  # These are rainfall only stations, so there are no results
  df_empty <- get_wdo_list(
    request = "getStationList",
    station_no = c("00018", "00184"),
    parametertype_name = "Water Course Discharge"
  )

  expect_true(tibble::is_tibble(df))
  expect_true(tibble::is_tibble(df_empty))
  expect_equal(dim(df), c(2, 5))
  expect_equal(dim(df_empty), c(0, 5))
  expect_equal(names(df), names(df_empty))
})

test_that("get_wdo_list only accepts list requests", {
  # TODO i.e not getTimeseriesValues etc.
})

test_that("column type conversions are applied", {
  # TODO cover all type conversions
  # TODO look at list and timeseries requests
  # TODO check that everything else is a character

  df <- get_wdo_list(
    request = "getStationList",
    station_no = c("00018", "00184")
  )

  expect_type(df$station_id, "integer")
  expect_type(df$station_latitude, "double")
  expect_type(df$station_longitude, "double")
})

test_that("timeseries values are always returned as a tibble", {
  # TODO also check empty case

  ts <- get_timeseries_values(ts_id = 83527010,
                              from = "2021-01-01",
                              to = "2021-01-07")
  expect_true(tibble::is_tibble(ts))
  expect_equal(dim(ts), c(1008, 6))
})

test_that("the return fields are idential for all timeseries", {
  # Don't know how to phrase, but I'm considering the case where different
  # timeseries might have different return fields (e.g. because of optional args)
  # TODO I'm not sure that I've actually covered that situation yet

  ts <- get_timeseries_values(ts_id = c(608153010, 425964010),
                              from = "2021-01-01",
                              to = "2021-01-02")


})

test_that("metadata is identical for all timeseries", {
  # TODO
  # i.e. when different timeseries might have different timeseries,
  # or not have a value for some options
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

test_that(".return_url can be specified from wrappers", {
  # TODO make sure it covers timeseries and list requests
})
