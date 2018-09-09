context("get_markers")

markers <- get_markers()

test_that("get_markers works", {
  expect_is(markers, "data.frame")
  expect_is(markers[["human"]], "character")
  expect_is(markers[["mouse"]], "character")
})
