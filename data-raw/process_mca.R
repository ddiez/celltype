# process MCA data to be used as celltype dictionary.

# read count data.
f <- "/Volumes/Biodev/db/expression/MCA/5435866/500more_dge/Spleen_dge.txt.gz"
f <- "/Volumes/Biodev/db/expression/MCA/5435866/rmbatch_dge/Spleen_rm.batch_dge.txt.gz"
cell_index <- scan(f, nlines = 1, what = "")

x <- read_delim(f, delim = " ", col_names = FALSE, skip = 1)
x[1:3, 1:3]

colnames(x) <- c("symbol", cell_index)
x[1:3, 1:3]
dim(x)

# read cell annotations.
cell_ann <- read_csv("/Volumes/Biodev/db/expression/MCA/5435866/MCA_CellAssignments.csv")
cell_ann <- cell_ann %>% select(cell_index = Cell.name, tissue = Tissue, celltype = Annotation)
cell_ann %>% filter(grepl("Spleen", tissue))

y <- x %>% gather(cell_index, count, -symbol)
y[1:10, ]

z <- y %>% left_join(cell_ann, by = "cell_index")
z[1:10, ]
tail(z)

z <- z %>% drop_na()
z
mca.db <- z %>% group_by(celltype, symbol) %>% summarize(expression = mean(count)) %>% ungroup()

mca.db %>% group_by(celltype) %>% summarize(count = n())
mca.db %>% group_by(symbol) %>% summarize(count = n())
table(mca.db$celltype)

mca.db <- mca.db %>%
  left_join(get_markers(), by = c("symbol" = "mouse")) %>%
  rename(mouse = symbol) %>%
  drop_na() %>%
  select(celltype, human, mouse, expression)

use_data(mca.db, overwrite = TRUE)
