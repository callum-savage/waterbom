test_that("I can build a query", {
  query <- build_query(request = "getStationList")
  expect_true(is.list(query))
  expect_true(names(query) == "request")
  expect_length(query, 1)
})

test_that("Invalid query names raises an error", {
  expect_error(build_query())

})
