#' predict_celltyp
#'
#' @param x object with count matrix.
#' @param org target organism.
#' @param name name of cell type dictionary.
#' @param db a matrix cell type dictionary.
#'
#' @export
predict_celltype <- function(x, org = "human", name = "immgen", db = NULL) {
  UseMethod("predict_celltype")
}

#' @rdname predict_celltype
#' @export
predict_celltype.SingleCellExperiment <- function(x, org = "human", name = "immgen", db = NULL, assay.name = "logcounts") {
  y <- assay(x, assay.name)
  rownames(y) <- rowData(x)[["symbol"]]
  predict_celltype(y, db = db, name = name, org = org)
}

#' @rdname predict_celltype
#' @export
predict_celltype.ExpressionSet <- function(x, org = "human", name = "immgen", db = NULL) {
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


