# response of all formats,
# errors in all formats
# change tries
# missing request
# missing format
# Invalid request
# Invalid format
# Invalid tries

test_that("get_bom_response works with manditory arguments", {
  r <- get_bom_response(
    format = "json",
    request = "getParameterTypeList"
  )
  expect_equal(httr2::resp_status(r), 200)
  expect_equal(httr2::resp_content_type(r), "application/json")
  expect_true(length(httr2::resp_body_json(r)) > 140)
})

test_that("get_bom_response works with atomic arguments", {
  r <- get_bom_response(
    format = "csv",
    request = "getStationList",
    station_no = "G0060047"
  )
  expect_equal(httr2::resp_status(r), 200)
  expect_equal(httr2::resp_content_type(r), "application/csv")
  expect_true(length(httr2::resp_headers(r)) > 0)
})

test_that("get_bom_response works with vector arguments", {
  r <- get_bom_response(
    format = "tabjson",
    request = "getParameterList",
    station_no = c("1200.1", "1203.1", "1204.1")
  )
  expect_equal(httr2::resp_status(r), 200)
  expect_equal(httr2::resp_content_type(r), "application/json")
  expect_equal(httr2::resp_body_json(r)[[2]][[5]], "Water Course Discharge")
})

test_that("get_bom_response errors if arguments are invalid", {
  expect_error(get_bom_response(format = "fakeformat", request = "getStationList"))
  expect_error(get_bom_response(format = "json", request = "fakeRequest"))
  expect_error(get_bom_response(format = "", request = ""))
  expect_error(get_bom_response(format = "json"))
  expect_error(get_bom_response(request = "getStationList"))
})

test_that("get_bom_response returns error messages from the response body", {

})

test_that("get_bom_data always returns a tibble", {
  # Valid request
  data1 <- get_bom_data(request = "getStationList", station_no = "G0060047")
  expect_true(tibble::is_tibble(data1))
  expect_true(nrow(data1) > 0)
  # Valid request with no result
  data2 <- get_bom_data(request = "getStationList", station_no = "abcd")
  expect_true(tibble::is_tibble(data2))
  expect_true(nrow(data2) == 0)
})

test_that("get_bom_data works for all supported request types", {
  station_list <- get_bom_data(
    request = "getStationList",
    station_no = c(403239, 403251)
  )
  parameter_list <- get_bom_data(
    request = "getParameterList",
    station_no = c(403239, 403251)
  )
  timeseries_list <- get_bom_data(
    request = "getTimeseriesList",
    station_no = c(403239, 403251)
  )
  timeseries_values <- get_bom_data(
    request = "getTimeseriesValues",
    ts_id = c(254923010, 255253010),
    from = "2020-01-01",
    to = "2020-01-02"
  )
  expect_equal(dim(station_list), c(2, 5))
  expect_equal(dim(parameter_list), c(4, 6))
  expect_equal(dim(timeseries_list), c(100, 7))
  expect_equal(dim(timeseries_values), c(3, 2))
})

test_that("getTimeseriesValues returns 0-length results explicitly", {

})

test_that("Column types are converted as expected", {

})
