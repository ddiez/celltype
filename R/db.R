#' get_markers
#'
#' @param name name of database.
#'
#' @export
#'
get_markers <- function(name = "immgen") {
  name <- match.arg(name, c("immgen"))

  if (name == "immgen")
    markers
}

#' get_db
#'
#' @param name name of database.
#' @param org name of organism.
#'
#' @export
#'
get_db <- function(name = "immgen", org = "human") {
  name <- match.arg(name, c("immgen", "immnav", "mca_spleen", "mca_thymus"))
  org <- match.arg(org, c("human", "mouse"))

  db <- get(paste0(name, ".db"))

  db %>% select(celltype, symbol = !!org, expression)
#' Returns a list of the celltypes available in the specified dictionary.
#'
#' @param name name of dictionary.
#' @param tissue name of tissue.
#'
#' @export
get_celltype <- function(name, tissue = NULL) {
  db <- get_db(name, tissue = tissue)
  sort(unique(db[["celltype"]]))
}

#' to_matrix
#'
#' @param db database object.
to_matrix <- function(db) {
  db %>% spread("celltype", "expression") %>%
    as.data.frame() %>%
    column_to_rownames("symbol") %>%
    as.matrix()
}
