test_that("query options are converted to a named list", {
  q <- construct_wdo_query("f", "r", a = "x")
  expect_true(is.list(q))
  expect_named(q)
  # rlang::names2 converts any empty names to ""
  expect_false(any(rlang::names2(q) == ""))
})

test_that("unnamed query options raise an error", {
  expect_error(construct_wdo_query("f", "r", "x"),
               class = "unnamed_query_options")
  # Explicit `format` call leaves the first argument unnamed
  expect_error(construct_wdo_query("f1", "r", format = "f2"),
               class = "unnamed_query_options")
})

test_that("repeated query options raise an error", {
  # repeated formal arg
  expect_error(construct_wdo_query(format = "f1", "r", format = "f2"))
  # repeated dots arg
  expect_error(construct_wdo_query("f", "r", a = "x", a = "y"))
})

test_that("white space is removed from query options", {
  q <- construct_wdo_query("f ", " r ", a = c("x x", "y  y", " z "))
  expect_equal(q$format, "f")
  expect_equal(q$request, "r")
  expect_equal(q$a, "x x,y y,z")
})

test_that("concatenation keeps all **unique** values", {
  q <- construct_wdo_query("f", "r", a = c("x", " x", "xy", "y"), b = c(1, 2, 3, 2))
  expect_equal(q$a, "x,xy,y")
  expect_equal(q$b, "1,2,3")
})

test_that("query options can be passed as a named list", {
  # TODO I want to be able specify all arguments in a list
  # This should work for all 'get' functions, including wrappers etc.
  # At present, this seems to be too difficult
  # q <- list(format = "csv",
  #           request = "getStationList",
  #           station_no = c("00018", "00184"))
  # get_wdo_response(!!!q, .return = "url")

  # get_wdo_list(!!!q)
})

test_that("A url is returned if requested", {
  # TODO check that this also works for list,ts etc.
  wdo_url <- get_wdo_response(
    format = "html",
    request = "getStationList",
    .return_url = TRUE)

  expect_type(wdo_url, "character")
  expect_match(wdo_url, "http://.*service=kisters")
})

test_that("invalid formats raise an error", {
  expect_error(
    get_wdo_response(format = "fakeformat", request = "getStationList"),
    "Format fakeformat is not supported by this request"
  )
  # "" is not an invalid format, it just reverts to the default
})

test_that("invalid requests raise an error", {
  # fake request
  expect_error(
    get_wdo_response(format = "html", request = "fakerequest"),
    "Request parameter 'fakerequest' is unknown."
  )
  # empty request
  expect_error(
    get_wdo_response(format = "html", request = ""),
    "Request parameter '' is unknown."
  )
})

test_that("request info hasn't changed", {
  request_info_resp <- get_wdo_response(format = "json", request = "getRequestInfo")
  request_info <- httr2::resp_body_json(request_info_resp)
  expect_snapshot(request_info)
})
