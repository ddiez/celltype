immgen_ann <- readxl::read_xls("data-raw/immgen/ni.2587-S2.xls", skip = 2)
foo <- immgen_ann %>% select(
    celltype = `Sample class`,
    tissue = `Tissue`,
    description = `Description`
  )
foo <- foo %>% dplyr::distinct()
foo <- foo %>% mutate(celltype = toupper(celltype))
foo <- foo %>% filter(description != "CMP")
foo$celltype %in% immgen.db$celltype
all(immgen.db$celltype %in% foo$celltype)

foo[!foo$celltype %in% immgen.db$celltype, ]$celltype %>% unique()
immgen.db[!immgen.db$celltype %in% foo$celltype, ]$celltype %>% unique()

immgen.db2 <- immgen.db %>% left_join(foo)
immgen.db2
immgen.db2 <- immgen.db2 %>% select(celltype, tissue, human, mouse, expression)
immgen.db2

immgen.db2 %>% filter(tissue == "spleen")
immgen.db2 %>% filter(tissue == "lung")
immgen.db2 %>% filter(grepl("thymus", tissue)) %>% pull(tissue) %>% unique()

immgen.db2 %>% filter(grepl("node", tissue)) %>% pull(tissue) %>% unique()
immgen.db2 %>% filter(grepl("LN", tissue)) %>% pull(tissue) %>% unique()
