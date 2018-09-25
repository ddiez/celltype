context("predict_celltype")

celltype <- predict_celltype(celltype::sce1)

test_that("predict_celltype works", {
  expect_is(celltype, "matrix")
  expect_type(celltype, "double")
  expect_equal(dim(celltype), c(199, 500))
  expect_equal(celltype[1, 1], 0.6096284, tolerance = 0.001)
})
