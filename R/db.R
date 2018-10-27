#' get_markers
#'
#' @param name name of database.
#'
#' @export
#'
get_markers <- function(name = "immgen") {
  name <- match.arg(name, c("immgen"))

  if (name == "immgen")
    celltype::markers
}

#' get_db
#'
#' @param name name of database.
#' @param org name of organism.
#' @param tissue name of tissue.
#'
#' @export
#'
get_db <- function(name = "immgen", org = "mouse", tissue = NULL) {
  name <- match.arg(name, c("immgen", "immnav", "mca"))
  org <- match.arg(org, c("human", "mouse"))

  db <- get(paste0(name, ".db"))

  if (! is.null(tissue)) {
    if (any(tissue %in% db[["tissue"]]))
      db <- db %>% filter(.data$tissue %in% !!tissue)
    else
      warning("tissue ", tissue, "not found in dictionary.")
  }

  db %>% select("celltype", "tissue", symbol = !!org, "expression")
}

#' Returns a list of the tissues available in the specified dictionary.
#'
#' @param name name of dictionary.
#'
#' @export
get_tissue <- function(name) {
  db <- get_db(name, tissue = NULL)
  sort(unique(db[["tissue"]]))
}

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
  db %>% select(- .data$tissue) %>%
    spread("celltype", "expression") %>%
    as.data.frame() %>%
    column_to_rownames("symbol") %>%
    as.matrix()
}
