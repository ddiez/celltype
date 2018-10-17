context("choose_celltype")

celltype <- predict_celltype(celltype::sce1)
celltype <- choose_celltype(celltype)

test_that("choose_celltype works", {
  expect_is(celltype, "data.frame")
  expect_type(celltype[["celltype"]], "character")
  expect_type(celltype[["cell_index"]], "character")
  expect_type(celltype[["correlation"]], "double")
  expect_equal(dim(celltype), c(500, 3))
  expect_identical(celltype[["celltype"]][1], "B.FO.PC")
})
