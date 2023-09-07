test_that("query options are stored in a named list", {
  query <- get_wdo_response(a = "x", .return = "query")
  expect_type(query, "list")
  expect_named(query)
})

test_that("not providing any query options raises an error", {
  expect_snapshot(get_wdo_response(), error = TRUE)
  expect_snapshot(get_wdo_response(.return = "url"), error = TRUE)
})

test_that("unnamed query options raise an error", {
  expect_snapshot(get_wdo_response("x"), error = TRUE)
  # .return should not count as a named argument
  expect_snapshot(get_wdo_response(.return = "url"), error = TRUE)
})

test_that(".return must be one of the specified values", {
  expect_snapshot(get_wdo_response(a = "x", .return = "happiness"), error = TRUE)
})

test_that("extra white space is removed from query options", {
  # the API is sensitive to white space
  q <- get_wdo_response(a = " x ", b =  c(" x x ", "y  y", " z "), .return = "query")
  expect_equal(q$a, "x")
  expect_equal(q$b, "x x,y y,z")
})

test_that("arguments can be specified by splicing a named list", {
  # get the url without a list
  url <- get_wdo_response(request = "getStationList",
                          station_no = "00018",
                          .return = "url")

  # get the url with query options in a list
  q1 <- list(request = "getStationList", station_no = "00018")
  url1 <- get_wdo_response(!!!q1, .return = "url")

  # get the url with everything in a list
  q2 <- c(query_list, .return = "url")
  url2 <- get_wdo_response(!!!q2)

  # compare
  expect_equal(url, url1)
  expect_equal(url, url2)
})

test_that("invalid formats raise an error", {
  expect_snapshot(
    get_wdo_response(format = "fakeformat", request = "getStationList"),
    error = TRUE
  )
})

test_that("invalid requests raise an error", {
  # fake request
  expect_snapshot(
    get_wdo_response(format = "html", request = "fakerequest"),
    error = TRUE
  )

  # empty request
  expect_snapshot(
    get_wdo_response(format = "html", request = ""),
    error = TRUE
  )
})

test_that("the API specificaiton hasn't changed", {
  # This shouldn't be run using a cached response
  resp <- get_wdo_response(format = "json", request = "getRequestInfo")
  request_info <- httr2::resp_body_json(resp)
  expect_snapshot(request_info)
})
