#' get_markers
#'
#' @param name name of database.
#'
#' @export
#'
get_markers <- function(name = "immgen") {
  name <- match.arg(name, c("immgen"))#, "immnav", "mca.spleen"))

  if (name == "immgen")
    rdcq::markers
}

#' get_db
#'
#' @param name name of database.
#' @param org name of organism.
#'
#' @export
#'
get_db <- function(name = "immgen", org = "human") {
  name <- match.arg(name, c("immgen", "immnav", "mca.spleen"))
  org <- match.arg(org, c("human", "mouse"))

  if (name == "immgen") {
    library(rdcq)
    db <- celltype::db_immgen
  }

  if (name == "immnav") {
    db <- celltype::db_immnav
  }

  if (name == "mca.spleen") {

  }

  if (org == "human") {
    db <- db %>% select(-mouse) %>% dplyr::rename(symbol = human)
  } else {
    db <- db %>% select(-human) %>% dplyr::rename(symbol = mouse)
  }

  db
}

#' to_matrix
#'
#' @param db database object.
#' @export
to_matrix <- function(db) {
  db %>% spread("celltype", "expression") %>%
    as.data.frame() %>%
    column_to_rownames("symbol") %>%
    as.matrix()
}
