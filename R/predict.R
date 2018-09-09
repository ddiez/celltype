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
  x <- to_tidy(x, row.name = "celltype", col.name = "cell_id", value = "correlation") %>%
    arrange(cell_id)

  # This is specific of immgen so we leave it out for now.
  # x <- x %>%
  #   mutate(celltype = sub("\\..*", "", celltype)) %>%
  #   group_by(cell_id, celltype) %>%
  #   summarize(correlation = max(correlation, na.rm = TRUE))

  x %>%
    group_by(cell_id) %>%
    slice(which.max(correlation))
}

#' fix_immgen_celltype
#'
#' Fixes the cell type names in the immgen dataset by picking the upper level
#' cell type in the hierarchy.
#'
#' @param x data.frame with cell type predictions.
#' @param colname name of column to fix.
#'
#' @export
fix_immgen_celltype <- function(x) {
  UseMethod("fix_immgen_celltype")
}

#' @rdname fix_immgen_celltype
#' @export
fix_immgen_celltype.SingleCellExperiment <- function(x, colname = "celltype_immgen") {
  fix_immgen_celltype(colData(x) %>% as.data.frame())
}


#' @rdname fix_immgen_celltype
#' @export
fix_immgen_celltype.data.frame <- function(x, colname = "celltype_immgen") {
  x %>% mutate(!!colname := sub("\\..*", "", x[[colname]])) %>% pull(colname)
}
