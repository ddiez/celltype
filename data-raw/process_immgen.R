process_immgen <- function() {
  db <- rdcq::db_tidy %>% as_tibble() %>%
    mutate(human = as.character(marker)) %>%
    mutate(celltype = as.character(celltype)) %>%
    select(-marker) %>%
    left_join(get_markers(), by = "human")

  db %>% select(celltype, human, mouse, expression)
}

process_immgen()
