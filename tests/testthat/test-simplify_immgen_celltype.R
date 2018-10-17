context("simplify_immgen_celltype")

celltype <- predict_celltype(celltype::sce1)
celltype <- choose_celltype(celltype)
celltype <- simplify_immgen_celltype(celltype)

test_that("simplify_immgen_celltype works", {
  expect_is(celltype, "character")
  expect_equal(length(celltype), 500)
  expect_equal(celltype[1], "B")
})
