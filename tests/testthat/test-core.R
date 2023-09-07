test_that(".return must be one of the specified values", {
  expect_error(get_wdo_response(a = "x", .return = "happiness"))
})

test_that("query options are stored in a named list", {
  query <- get_wdo_response(a = "x", .return = "query")
  expect_type(query, "list")
  expect_named(query)
})

test_that("empty queries raise an error", {
  expect_error(get_wdo_response())
  # .return is not part of the query
  expect_error(get_wdo_response(.return = "url"))
})

test_that("unnamed query options raises an error", {
  expect_error(get_wdo_response("x"))
  # .return should not count as a named argument
  expect_error(get_wdo_response(.return = "url"))
})

test_that("extra white space is removed from query options", {
  # the API is sensitive to white space
  q <- get_wdo_response(a = " x ", b =  c(" x x ", "y  y"), .return = "query")
  expect_equal(q$a, "x")
  expect_equal(q$b, "x x,y y")
})

test_that("arguments can be specified by splicing a named list", {
  # get the url as usual
  url <- get_wdo_response(request = "getStationList",
                          station_no = "00018",
                          .return = "url")

  # get the url with query options in a list
  q1 <- list(request = "getStationList", station_no = "00018")
  url1 <- get_wdo_response(!!!q1, .return = "url")

  # get the url .return added to the list
  q2 <- c(query_list, .return = "url")
  url2 <- get_wdo_response(!!!q2)

  # compare
  expect_equal(url, url1)
  expect_equal(url, url2)
})

test_that("trailing commas are ignored", {
  expect_no_error(
    get_wdo_response(
      request = "getParameterList",
      station_no = "00018",
      .return = "request",
    )
  )
})

test_that("invalid formats raise an error", {
  expect_error(
    get_wdo_response(format = "fakeformat", request = "getStationList")
  )
})

test_that("invalid requests raise an error", {
  # fake request
  expect_snapshot(get_wdo_response(format = "html", request = "fakeRequest"))

  # empty request
  expect_error(get_wdo_response(format = "html", request = ""))
})

test_that("the API specificaiton hasn't changed", {
  # This shouldn't be run using a cached response
  resp <- get_wdo_response(format = "json", request = "getRequestInfo")
  request_info <- httr2::resp_body_json(resp)
  expect_snapshot(request_info)
})

test_that("unused query options raise a warning", {
  # TODO This is mostly to avoid silently dropping query options when the
  # query is otherwise valid. This will be easiest to do using wdo_query_fields.
})
