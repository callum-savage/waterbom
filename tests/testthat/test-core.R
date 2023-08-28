# TODO test if having white space around args causes issues
# Particularly with staiton no./name etc.

test_that("query options are converted to a named list", {
  q <- construct_wdo_query("f", "r", a = "x")
  expect_true(is.list(q)) # q is a list
  expect_named(q) # q has names
  expect_false(any(rlang::names2(q) == "")) # no names are missing
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

test_that("responses are returned in the expected format", {
  # should use httr2 content type
  # station_request <- function(format) {
  #   r = get_wdo_response(format, "getStationList", station_no = 402329)
  #   r$headers$`Content-Type`
  # }
  #
  # list_formats <- c("lpk", "geojson", "tabjson", "objson", "ascii", "csv",
  #                   "html", "xlsx", "kml", "json")
  #
  # list_responses <- purrr::map(list_formats, station_request)

  # ts_formats <- c("dajson", "wml2", "zrxp", "esrijson")
  # img_formats <- c("jpg", "png")
  # web_formats <- "json"
})

test_that("all request types are supported", {
  # Minimal requests for all types, not just supported
})

test_that("error messages are identical across all formats", {
  # Try and raise the same error in xml, json, csv, etc.
})

test_that("query order doesn't matter", {
  # Test that providing args in a different order creates the same url
})
