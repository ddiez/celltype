process_immgen <- function() {
  db <- rdcq::db_tidy %>% as_tibble() %>%
    mutate(human = as.character(marker)) %>%
    mutate(celltype = as.character(celltype)) %>%
    select(-marker) %>%
    left_join(get_markers(), by = "human")

  db %>% select(celltype, human, mouse, expression)
}

db <- process_immgen()
db

# fix celltype case.
immgen.db <- db %>% mutate(celltype = toupper(celltype))

# temporary tissue fix.
tissue <- strsplit(immgen.db$celltype, "\\.")
foo <- sapply(tissue, function(x) x[length(x)])
foo %>% unique()
sapply(tissue, length) %>% table()

immgen.db[sapply(tissue, length) == 2, ] %>% pull(celltype) %>% unique()
immgen.db[sapply(tissue, length) == 3, ] %>% pull(celltype) %>% unique()
immgen.db[sapply(tissue, length) == 4, ] %>% pull(celltype) %>% unique()
immgen.db[sapply(tissue, length) == 6, ] %>% pull(celltype) %>% unique()

immgen.db <- immgen.db %>% filter(! grepl("TCRBKO|OT1", celltype))

tissue <- strsplit(immgen.db$celltype, "\\.")
sapply(tissue, length) %>% table()

tissue <- sapply(tissue, function(x) x[length(x)])
tissue %>% unique()

immgen.db$tissue <- tissue

use_data(immgen.db, overwrite = TRUE)
