test_that("query options are converted to a named character vector", {
  q <- construct_wdo_query("f", "r", a = "x")
  expect_true(is_character(q)) # q is a character vector
  expect_named(q) # q has names
  expect_false(any(names2(q) == "")) # no names are missing
})

test_that("unnamed query options raise an error", {
  expect_error(construct_wdo_query("f", "r", "unnamed_option"))
  # Explicit `format` call leaves the first argument unnamed
  expect_error(construct_wdo_query("f1", "r", format = "f2"))
})

test_that("repeated query options raise an error", {
  # repeated formal arg
  expect_error(construct_wdo_query(format = "f1", "r", format = "f2"))
  # repeated dots arg
  expect_error(construct_wdo_query("f", "r", a = "x", a = "y"))
})

test_that("concatenation keeps all **unique** values", {
  q <- construct_wdo_query("f", "r", x = c(1, 2, 3, 2, 4))
  expect_equal(q$x, "1,2,3,4")
})
