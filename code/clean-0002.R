# Clean dataset 0002

rm(list = ls())
source("code/helper.R")
db <- read.csv("data/raw/fe0002/Costa2015.csv", head = TRUE, sep = ";", stringsAsFactors = FALSE)

# Get rows and columns with iron data
id_col <- colnames(db)[grep("Fe", colnames(db))]
id_col <- id_col[-2]
idx <- which(!is.na(db[, id_col]), arr.ind = TRUE)
id_col <- id_col[unique(idx[, 2])]
id_row <- unique(idx[, 1])

# Contribution
length(id_row)

# Profile ####
pf <- db[id_row, c(
  "Código.PA", "Data.da.Coleta", "Datum", "Northing", "Easting", "UF", "Município", "Classe.de.Solos.Nível.3",
  "X1ª.Ocorrência", "Uso.Atual", "Litologia")]
pf <- pf[!duplicated(pf$Código.PA), ]

# Observation date
pf$Data.da.Coleta <- stringr::str_replace(pf$Data.da.Coleta, "/", "-")
pf$Data.da.Coleta <- stringr::str_replace(pf$Data.da.Coleta, "/", "-")

# Soil classification
pf$classe <- paste(pf$Classe.de.Solos.Nível.3, " ", pf$X1ª.Ocorrência, sep = "")
Encoding(pf$classe) <- "UTF-8"
pf$sibcs <- sapply(pf$classe, sibcsSymbol)

# Land use
id <- grep("pastagem", pf$Uso.Atual, ignore.case = TRUE)
pf$Uso.Atual[id] <- "CRIAÇÃO ANIMAL"
pf$Uso.Atual[6] <- "CRIAÇÃO ANIMAL"
pf$Uso.Atual[10] <- "PROTEÇÃO DA NATUREZA"

# Litology
id <- grep("aluvionar", pf$Litologia)
pf$Litologia[id] <- "Sedimento aluvial"

# Save temporary file
write.csv(pf, "data/raw/fe0002/tmp.csv")
rm(pf)

# Horizon ####
hz <- db[id_row, c(
  "Código.PA", "Código.Horizonte", "Símbolo.Horizonte", "Profundidade.Superior", "Profundidade.Inferior", 
  id_col)]
str(hz)

# Save temporary file
write.csv(hz, "data/raw/fe0002/tmp.csv")

# Remove temporary file
system("rm data/raw/fe0002/tmp.csv")
