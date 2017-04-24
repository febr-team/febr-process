# Contribuição 0003
# Responsável: Sistema de Informação de Solos Brasileiros
# Instituição: Embrapa Informática Agropecuária / Embrapa Solos

# Com esse script são processados os dados para manipulação manual

# Preparar ambiente de trabalho
rm(list = ls())
source("code/helper.R")
source("code/helper-utf8.R", encoding = "UTF-8")

# Carregar dados
db <- read.csv(
  "data/raw/fe0003/embrapa-pos-03.csv", head = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")

# Definir colunas que serão exportadas
cols_dataset <- c(
  "Título.do.Trabalho",
  "Autor",
  "Ano.de.Publicação",
  "Número",
  "Tipo.de.Publicação",
  "Nível.do.Levantamento.Pedológico",
  "Referência.Bibliográfica",
  "Código.Trabalho")
cols_observation <- c(
  "Código.PA",
  "Número.PA",
  "Número.de.Campo",
  "observation_date",
  "Datum",
  "y_coord",
  "x_coord",
  "UF",
  "Município", 
  "Localização.descritiva",
  "Situação.coleta.das.amostras",
  "Observações",
  "Informações.Complementares",
  "Classificação.Original",
  "Classificação.Atual",
  "Classificação.FAO.WRB",
  "Classificação.Soil.Taxonomy",
  "Litologia",
  "land_use",
  "Uso.Atual",
  "Responsável.is..pela.Descrição",
  "Tipo",
  "Material.de.Origem",
  "Altitude..m.",
  "Fase.de.Pedregosidade",
  "Fase.de.Vegetação.Primária",
  "Fase.de.Relevo", 
  "Fase.de.Substrato", 
  "Forma.de.Relevo", 
  "Relevo.Local", 
  "Relevo.Regional", 
  "Grupo.Geológico", 
  "Formação.Geológica", 
  "Cronologia", 
  "Gaussen",
  "Köppen",
  "Saturação.por.bases.ou.por.alumínio",
  "Caráter.Aluminico.Alitico",
  "Caráter.Sodico.Solodico",
  "Caráter.Sálico.Salino",
  "Caráter.Carbonático.Carbonato",
  "Caráter.Plíntico.Plânico",
  "Caráter.Concrecionário.Litoplíntico.Dúrico",
  "Caráter.Ácrico",
  "Caráter.Flúvico",
  "Caráter.Argilúvico",
  "Caráter.Coeso",
  "Caráter.Vértico",
  "Caráter.Epiáquico",
  "Caráter.Crômico",
  "Caráter.Ebânico",
  "Caráter.Rúvico",
  "Caráter.Êutrico",
  "Atividade.da.argila",
  "Horizonte.diagnóstico.superficial",
  "Horizonte.diagnóstico.subsuperficial",
  "Grupamento.de.classe.textural.superficial",
  "Grupamento.de.classe.textural.subsuperficial",
  "Classe.de.pedregosidade",
  "Classe.de.rochosidade",
  "Classe.de.drenagem",
  "Classe.de.erosão",
  "Frequência.de.Erosão...1ª.Ocorrência",
  "Frequência.de.Erosão...2ª.Ocorrência",
  "Forma.de.Erosão...1ª.Ocorrência",
  "Forma.de.Erosão...2ª.Ocorrência",
  "Profundidade.de.Erosão...1ª.Ocorrência",
  "Profundidade.de.Erosão...2ª.Ocorrência",
  "Ocorrência.Autogranulação",
  "Presença.de.Concreções",
  "Presença.de.Contato.Lítico",
  "Presença.de.Fase.Erodida",
  "Presença.de.Fase.Rochosa",
  "Presença.Gilgai",
  "Presença.Contato.lico.fragmentário",
  "Presença.de.Nódulos",
  "Presença.Petroplintita",
  "Ocorrência.Plintita")
cols_layer <- c(
  "Código.PA",
  "Símbolo.Horizonte",
  "Código.Horizonte",
  "Profundidade.Superior",
  "Profundidade.Inferior",
  "Ataque.sulfúrico...Fe2O3",
  "CDB...Ferro..g.kg.",
  "Oxalato.de.Amônio...Ferro",
  "Pirofosfato.de.Sódio...Ferro",
  "Microelementos...Ferro")

# Dividir dados por trabalho
db <- split(db, as.factor(db$Código.Trabalho))

# Salvar trabalhos em arquivos individuais por UF
# UF exportadas: RS
file_name <- names(db)
uf <- "RS"
lapply(1:length(db), function (i) {
  
  uf_id <- levels(as.factor(db[[i]]$UF))[which.max(summary(as.factor(db[[i]]$UF)))]
  
  if (uf_id == uf) {
    
    # dataset
    dataset <- unique(db[[i]][, cols_dataset])
    dataset <- cbind(
      dataset_id = paste("fe", dataset$Código.Trabalho, sep = ""),
      dataset_title = 
        paste("Conjunto de dados do ", dataset$Nível.do.Levantamento.Pedológico, "'",
              dataset$Título.do.Trabalho, "'", sep = ""),
      dataset_description = 
        paste("Conjunto de dados públicos do solo originalmente obtidos do Sistema de Informação de Solos ", 
              "Brasileiros (SISB), ",
              "construído e mantido pela Embrapa Solos (Rio de Janeiro) e Embrapa Informática Agropecuária ",
              "(Campinas), referente ao ", dataset$Nível.do.Levantamento.Pedológico, "'", 
              dataset$Título.do.Trabalho, "'. ", "Dados de localização espacial de observações do solo sem ",
              "coordenadas espaciais foram completados usando dados produzidos por Cooper et al. (2005) e ",
              "publicados no artigo 'A national soil profile database for Brazil available to international ",
              "scientists' do Soil Science Society of America Journal, ou então usando os dados de ",
              "localização descritiva para inferir sobre as coordenadas espaciais mais prováveis usando ",
              "serviços de mapas online como o Google Maps e o Google Earth. Erros e inconsistências nos ",
              "dados das coordenadas espaciais das observações foram corrigidos manualmente visualizando as ",
              "respectivas observações no Google Maps. Nos casos em que dados sobre o sistema de coordenadas ",
              "de referência não estava disponível, adotou-se o WGS 84 como datum padrão. Também foram ",
              "corrigidos erros e inconsistências, e realizadas atualizações no nome do município e código ",
              "da unidade federativa onde as observações foram realizadas. Dados do conteúdo de ferro ",
              "apresentando valores discrepantes foram corrigidos depois de consultar o relatório do ",
              "levantamento do solo onde originalmente foram publicados. A fim de preservar a conexão dos ",
              "dados com o SISB, usa-se o mesmo código de identificação daquele sistema para o conjunto de ",
              "dados, assim como o código de identificação de cada observação corresponde ao código do perfil",
              " do solo no SISB e o código das amostras corresponde ao código dos horizontes. Todas os ",
              "demais dados são mantidos como dados adicionais para facilitar o reuso do conjunto de dados ",
              "como um todo. ",
              "Nenhum item técnico-científico do SISB e seu respectivo banco de dados que seja fruto da ",
              "atividade intelectual, criativa, inovadora e inédita dos projetos conduzidos pela Embrapa ",
              "foi ou é usado para organizar estruturalmente a presente versão do conjunto de dados.",
              sep = ""
              ),
      publication_date = "xx-xx-2017",
      dataset_version = "2.0",
      dataset_license = "CC BY 4.0",
      organization_name = "",
      organization_url = "",
      organization_country = "Brasil",
      organization_city = "",
      organization_postal_code = "",
      organization_street_name = "",
      organization_street_number = "",
      author_name = gsub(",", ";", dataset$Autor),
      author_email = "",
      contributor_name = "Alessandro Samuel-Rosa; Diego Gris; Nícolas Augusto Rosin",
      contributor_email	= 
        "alessandrosamuelrosa@gmail.com; diegojgris@gmail.com; nicolasaugustorosin@gmail.com",
      contributor_organization = "Universidade Federal de Santa Maria (UFSM)",
      dataset_reference_1 = dataset$Referência.Bibliográfica,
      dataset_reference_2 = "https://www.bdsolos.cnptia.embrapa.br/consulta_publica.html",
      dataset_reference_3 = "http://doi.org/10.2136/sssaj2004.0140",
      subject = "Gênese, Morfologia e Classificação dos Solos",
      keywords = "",
      vcge_category = "Pesquisa científica e tecnologia"  
    )
    dataset <- t(dataset)
    colnames(dataset) <- "item"
    write.table(
      dataset,
      file = paste("data/raw/", file_name[i], "-", uf, "-dataset.csv", sep = ""), 
      sep = "\t", fileEncoding = "UTF-8")
    
    # observation
    observation <- unique(db[[i]][, cols_observation])
    colnames(observation)[c(1:3, 5, 8, 9)] <- 
      c("observation_id", "observation_id_book", "observation_id_field", "coord_system", "state_id", "city_id")
    observation <- cbind(
      observation[, 1:5],
      coord_accuracy = "",
      coord_source = "",
      country_id = "BR",
      sample_type = "SIMPLES",
      sample_number = as.character(1),
      sample_area = as.character(1),
      observation[, 6:ncol(observation)]
    )
    write.table(
      observation, 
      file = paste("data/raw/", file_name[i], "-", uf, "-observation.csv", sep = ""), 
      sep = "\t", fileEncoding = "UTF-8", row.names = FALSE)
    
    # layer
    layer <- db[[i]][, cols_layer]
    layer <- layer[with(layer, order(Código.PA, Código.Horizonte)), ]
    colnames(layer) <- 
      c("observation_id", "layer_name", "Código.Horizonte", "upper_depth", "lower_depth",
        "fe_sulfurico_xxx", "fe_ditionito_xxx", "fe_oxalato_xxx", "fe_pirofosfato_xxx", "fe_xxx_xxx")
    layer <- cbind(
      observation_id = layer$observation_id,
      layer_number = "",
      layer_name = layer$layer_name,
      sample_code = layer$Código.Horizonte,
      layer[, 4:ncol(layer)]
      )
    write.table(
      layer, 
      file = paste("data/raw/", file_name[i], "-", uf, "-layer.csv", sep = ""), 
      sep = "\t", fileEncoding = "UTF-8", row.names = FALSE) 
  }
})

# Carregar dados compilados por pesquisadores da Esalq
esalq <- read.csv(
  "data/raw/fe0003/esalq.csv", head = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")

# Atribuir informação da UF para dados da Esalq
states <- raster::shapefile("data/gis/states.shp")
tmp <- esalq[, c("latitude", "longitude")]
sp::coordinates(tmp) <- c("latitude", "longitude")[2:1]
sp::proj4string(tmp) <- sp::proj4string(states)
tmp <- sp::over(tmp, states)
esalq$UF <- tmp[, 1]

# Salvar aquivo da esalq com dados da UF escolhida
idx <- which(esalq$UF == uf)
tmp <- esalq[idx, ]
write.table(
  tmp, file = paste("data/raw/esalq-", uf, ".csv", sep = ""), sep = "\t", fileEncoding = "UTF-8", 
  row.names = FALSE)

