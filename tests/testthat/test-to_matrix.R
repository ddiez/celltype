context("to_matrix")

db <- get_db()
db <- to_matrix(db)

test_that("to_matrix works", {
  expect_is(db, "matrix")
  expect_is(db[1, 1], "numeric")
  expect_equal(dim(db), c(61, 217))
  expect_equal(rownames(db[1:3, , drop = FALSE]), c("BCR", "CCR7", "CD14"))
  expect_equal(colnames(db[, 1:3, drop = FALSE]), c("B.FO.LN", "B.FO.MLN", "B.FO.PC"))
})
