get_markers <- function(name = "immgen") {
  name <- match.arg(name, c("immgen"))#, "immnav", "mca.spleen"))

  if (name == "immgen")
    rdcq::markers
}

get_db <- function(name = "immgen") {
  name <- match.arg(name, c("immgen", "immnav", "mca.spleen"))

  if (name == "immgen") {
    library(rdcq)
    db <- rdcq::db_tidy %>% as_tibble() %>%
      mutate(human = as.character(marker)) %>%
      select(-marker) %>%
      left_join(get_markers(), by = "human")
  }

  if (name == "immnav") {

  }

  if (name == "mca.spleen") {

  }

  db
}
