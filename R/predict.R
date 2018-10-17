#' predict_celltyp
#'
#' @param x object with count matrix.
#' @param org target organism.
#' @param name name of cell type dictionary.
#' @param tissue name of the tissue.
#' @param db a matrix cell type dictionary.
#' @param ... arguments passed down to methods.
#'
#' @export
predict_celltype <- function(x, org = "mouse", tissue = NULL, name = "immgen", db = NULL, ...) {
  UseMethod("predict_celltype")
}

#' @rdname predict_celltype
#' @param assay.name name of the assay to obtain expression matrix.
#' @export
predict_celltype.SingleCellExperiment <- function(x, org = "mouse", tissue = NULL, name = "immgen", db = NULL, assay.name = "logcounts", ...) {
  y <- assay(x, assay.name)
  rownames(y) <- rowData(x)[["symbol"]]
  predict_celltype(y, org = org, name = name, tissue = tissue, db = db)
}

#' @rdname predict_celltype
#' @export
predict_celltype.ExpressionSet <- function(x, org = "mouse", tissue = NULL, name = "immgen", db = NULL, ...) {
  y <- exprs(x)
  fdata <- pData(featureData(x))
  rownames(y) <- fdata[["symbol"]]
  predict_celltype(x, org = org, name = name, tissue = tissue, db = db)
}

#' @rdname predict_celltype
#' @export
predict_celltype.matrix <- function(x, org = "mouse", tissue = NULL, name = "immgen", db = NULL, ...) {
  if (is.null(db))
    db <- get_db(name, org = org, tissue = tissue) %>% to_matrix()

  x <- x[rownames(x) %in% rownames(db), , drop = FALSE]
  db <- db[rownames(x), , drop = FALSE]

  cor(db, x)
}

#' choose_celltype
#'
#' Make decision about cell type identity based on prediction matrix.
#'
#' @param x a matrix of cell type predictions.
#'
#' @export
choose_celltype <- function(x) {
  UseMethod("choose_celltype")
}

#' @rdname choose_celltype
#' @export
choose_celltype.matrix <- function(x) {
  sel.max <- apply(x, 2, function(j) {
    jj <- which.max(j)
    if(length(jj) == 0)
      jj <- NA_integer_
    jj
  })
  cellcor <- sapply(seq_len(ncol(x)), function(j) x[sel.max[j], j])
  tibble(cell_index = colnames(x), celltype = rownames(x)[sel.max], correlation = cellcor)
}

#' simplify_immgen_celltype
#'
#' Simplifies cell type names in the immgen dataset by picking the upper level
#' cell type in the hierarchy.
#'
#' @param x character vector with cell type names from Immgen.
#'
#' @export
simplify_immgen_celltype <- function(x) {
  UseMethod("simplify_immgen_celltype")
}

#' @rdname simplify_immgen_celltype
#' @export
simplify_immgen_celltype.character <- function(x) {
  sub("\\..*", "", x)
}
