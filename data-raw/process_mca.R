# process MCA data to be used as celltype dictionary.

# read count data.
f <- "/Volumes/Biodev/db/expression/MCA/5435866/500more_dge/Spleen_dge.txt.gz"
f <- "/Volumes/Biodev/db/expression/MCA/5435866/rmbatch_dge/Spleen_rm.batch_dge.txt.gz"
cell_index <- scan(f, nlines = 1, what = "")

x <- read_delim(f, delim = " ", col_names = FALSE, skip = 1)
x[1:3, 1:3]

colnames(x) <- c("symbol", h)
x[1:3, 1:3]
dim(x)

# read cell annotations.
cell_ann <- read_csv("/Volumes/Biodev/db/expression/MCA/5435866/MCA_CellAssignments.csv")
cell_ann %>% filter(grepl("Spleen", Tissue))
