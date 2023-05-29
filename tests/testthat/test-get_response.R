# response of all formats,
# errors in all formats
# change tries
# missing request
# missing format
# Invalid request
# Invalid format
# Invalid tries

test_that("get_bom_response returns a valid response", {
  response <- get_bom_response(
    format = "csv",
    request = "getStationList",
    station_no = "G0060047"
  )
  expect_true(length(httr2::resp_headers(response)) > 0)
  expect_true(httr2::resp_status(response) == 200)
  expect_true(httr2::resp_content_type(response) == "application/csv")
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
