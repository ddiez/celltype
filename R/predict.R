#' predict_celltyp
#'
#' @export
predict_celltype <- function(x, db = NULL, name = "immgen", org = "human") {
  UseMethod("predict_celltype")
}

#' @rdname predict_celltype
#' @export
predict_celltype.SingleCellExperiment <- function(x, db = NULL, name = "immgen", org = "human", assay.name = "logcounts") {
  y <- assay(x, assay.name)
  rownames(y) <- rowData(x)[["symbol"]]
  predict_celltype(y, db = db, name = name, org = org)
}

#' @rdname predict_celltype
#' @export
predict_celltype.ExpressionSet <- function(x, db = NULL, name = "immgen", org = "human") {
  y <- exprs(x)
  fdata <- pData(featureData(x))
  rownames(y) <- fdata[["symbol"]]
  predict_celltype(x, db = db, name = name, org = org)
}

#' @rdname predict_celltype
#' @export
predict_celltype.matrix <- function(x, db = NULL, name = "immgen", org = "human") {
  if (is.null(db))
    db <- get_db(name, org) %>% to_matrix()

  x <- x[rownames(x) %in% rownames(db), ]
  db <- db[rownames(x), ]

  cor(db, x)
}


