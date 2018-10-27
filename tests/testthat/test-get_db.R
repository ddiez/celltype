context("get_db")

db <- get_db()

test_that("get_db works", {
  expect_is(db, "data.frame")
  expect_is(db[["celltype"]], "character")
  expect_is(db[["symbol"]], "character")
  expect_is(db[["expression"]], "numeric")
})

db <- get_db(tissue = "LN")

test_that("get_db works with tissue", {
  expect_is(db, "data.frame")
  expect_is(db[["celltype"]], "character")
  expect_is(db[["symbol"]], "character")
  expect_is(db[["expression"]], "numeric")
  expect_equal(nrow(db), 12139)
})
