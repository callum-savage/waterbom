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

test_that("A url is returned if requested", {
  # check that this also works for list,ts etc.
})

test_that("responses are returned in the expected format", {

  # TODO rewrite this using user-facing helper functions
  # i.e. add content type to list_formats() or similar

  # Define a helper for making a station list request
  # st_type <- function(format) {
  #   r = get_wdo_response(format, "getStationList", station_no = 402329)
  #   httr2::resp_content_type(r)
  # }
  #
  # expect_equal(st_type("ascii"),   "text/plain")
  # expect_equal(st_type("html"),    "text/html")
  # expect_equal(st_type("csv"),     "application/csv")
  # expect_equal(st_type("json"),    "application/json")
  # expect_equal(st_type("geojson"), "application/json")
  # expect_equal(st_type("tabjson"), "application/json")
  # expect_equal(st_type("objson"),  "application/json")
  # expect_equal(st_type("lpk"),     "application/lpk")
  # expect_equal(st_type("xlsx"),    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
  # expect_equal(st_type("kml"),     "application/vnd.google-earth.kml+xml")
  #
  # # Define a helper for making a timeseries request
  #
  # ts_type <- function(format) {
  #   r = get_wdo_response(format, "getTimeseriesValues", ts_id = 254923010)
  #   httr2::resp_content_type(r)
  # }
  #
  # expect_equal(ts_type("dajson"), "application/json")
  # expect_equal(ts_type("esrijson"), "application/json")
  # expect_equal(ts_type("wml2"), "text/xml")
  # expect_equal(ts_type("zrxp"), "text/plain")

  # Other formats
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

test_that("invalid formats raise an error", {
  # e.g. format = fakeformat
  # And formats which don't match the request
  # Check empty format "" too
})

test_that("invalid requests raise an error", {
  # request = fakerequest
  # Check empty request "" too
})

test_that("invalid query options raise an error", {
  # fakeoption = 1
  # I don't actually think this will, or should, raise an error
  # Also check query options which are not valid for the request
})
