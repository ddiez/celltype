# process MCA data to be used as celltype dictionary.
library(celltype)
library(readr)
library(tidyr)
library(dplyr)

read_mca <- function(filename, mca.annot = NULL) {
  cell_index <- scan(filename, nlines = 1, what = "")

  x <- read_delim(filename, delim = " ", col_names = FALSE, skip = 1)
  colnames(x) <- c("symbol", cell_index)

  y <- x %>% gather(cell_index, count, -symbol)

  if (! is.null(mca.annot)) {
    y <- y %>% left_join(mca.annot, by = "cell_index")
  }

  y <- y %>% drop_na()
}

read_mca_annotations <- function(filename) {
  x <- read_csv(filename)
  x <- x %>% select(cell_index = Cell.name, tissue = Tissue, celltype = Annotation)
  #x <- x %>% filter(grepl("Spleen", tissue))
  x
}

### MCA annotations.
mca_annot <- read_mca_annotations("/Volumes/Biodev/db/expression/MCA/5435866/MCA_CellAssignments.csv")

### Spleen.
# read count data.
#f <- "/Volumes/Biodev/db/expression/MCA/5435866/500more_dge/Spleen_dge.txt.gz"
f <- "/Volumes/Biodev/db/expression/MCA/5435866/rmbatch_dge/Spleen_rm.batch_dge.txt.gz"

mca_spleen.db <- read_mca(f, mca.annot = mca_annot)
mca_spleen.db

mca_spleen.db <- mca_spleen.db %>%
  group_by(celltype, symbol) %>%
  summarize(expression = mean(count)) %>%
  ungroup() %>%
  mutate(expression = log10(expression + 1))

mca_spleen.db

mca_spleen.db <- mca_spleen.db %>%
  left_join(get_markers(), by = c("symbol" = "mouse")) %>%
  rename(mouse = symbol) %>%
  drop_na() %>%
  select(celltype, human, mouse, expression)

mca_spleen.db
devtools::use_data(mca_spleen.db, overwrite = TRUE)

### Thymus
# read count data.
f1 <- "/Volumes/Biodev/db/expression/MCA/5435866/rmbatch_dge/Thymus1_rm.batch_dge.txt.gz"
#f2 <- "/Volumes/Biodev/db/expression/MCA/5435866/rmbatch_dge/Thymus2_rm.batch_dge.txt.gz"
# Thymus_2 has no annotations.

mca_thymus.db <- read_mca(f1, mca.annot = mca_annot)

mca_thymus.db <- mca_thymus.db %>%
  group_by(celltype, symbol) %>%
  summarize(expression = mean(count)) %>%
  ungroup() %>%
  mutate(expression = log10(expression + 1))

mca_thymus.db

mca_thymus.db <- mca_thymus.db %>%
  left_join(get_markers(), by = c("symbol" = "mouse")) %>%
  rename(mouse = symbol) %>%
  drop_na() %>%
  select(celltype, human, mouse, expression)

mca_thymus.db

devtools::use_data(mca_thymus.db, overwrite = TRUE)

