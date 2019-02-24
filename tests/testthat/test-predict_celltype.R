context("predict_celltype")

celltype <- suppressWarnings(predict_celltype(celltype::sce1))

test_that("predict_celltype works", {
  expect_is(celltype, "matrix")
  expect_type(celltype, "double")
  expect_equal(dim(celltype), c(199, 500))
  expect_equal(celltype[1, 1], 0.6096284, tolerance = 0.001)
})

db <- matrix(c(0, 1, 1, 0, .5, .5), ncol = 3, dimnames = list(c("Cd4", "Cd8a"), c("CD8+", "CD4+", "Other")))
celltype <- suppressWarnings(predict_celltype(celltype::sce1, db = db))

test_that("custom dictionary works", {
  expect_is(celltype, "matrix")
  expect_type(celltype, "double")
  expect_equal(dim(celltype), c(3, 500))
  expect_true(is.na(celltype[1, 1]))
  expect_equal(celltype[1, 2], -1)
  expect_equal(celltype[2, 2], 1)
})
