# Convert immuno-navigator database into dictionary suitable for celltype identification.
x <- read_rds("~/Dropbox/immnav_mouse.rds")
x
sel.markers.eg <- pData(featureData(x)) %>% filter(symbol %in% get_markers()$mouse) %>% pull(entrezgene)
y <- x[sel.markers.eg, ]
y
dim(y)
dim(get_markers())

pdata <- pData(y) %>% as_tibble(rownames = "samplename")
pdata

z <- exprs(y) %>% d10misc::to_tidy(row = "entrezgene", col = "samplename", value = "expression")
z

z <- z %>% left_join(pdata, by = "samplename")
z

fdata <- pData(featureData(y)) %>% as_tibble()
fdata

z <- z %>% left_join(fdata, by = "entrezgene")
z

# summarize.
db_immnav <- z %>% group_by(celltype, symbol) %>% summarize(expression = mean(expression)) %>% ungroup()
db_immnav

db_immnav <- db_immnav %>%
  left_join(get_markers(), by = c("symbol" = "mouse")) %>%
  rename(mouse = symbol) %>%
  select(celltype, human, mouse, expression)

