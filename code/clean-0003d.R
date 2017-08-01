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
  "Número.PA",
  "Código.PA",
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
  "Ocorrência.Plintita",
  "Link.para.Descrição.em.PDF")
cols_layer <- c(
  "Número.PA",
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

# Arquivos de dados extra
extra_files <- c("sisb-fisicas.csv", "sisb-morfologicas.csv")
extra_files <- paste("data/raw/fe0003/", extra_files, sep = "")
extra_files <- lapply(1:length(extra_files), function (i) {
  read.csv(extra_files[i], head = TRUE, sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")
})

# Salvar trabalhos em arquivos individuais por UF
# UF exportadas: RS, SC, PR, MS, MT, RO, AC, AM, 
# RR, AP, PA, TO, GO, BA, MA, PI, CE, DF, RN, PB,
# PE, AL, SE, ES, SP, MG
file_name <- names(db)
uf <- "RJ"
lapply(1:length(db), function (i) {
  # i <- 200
  uf_id <- levels(as.factor(db[[i]]$UF))[which.max(summary(as.factor(db[[i]]$UF)))]
  
  if (uf_id == uf) {
    
    # dataset
    dataset <- unique(db[[i]][, cols_dataset])
    dataset <- cbind(
      dataset_id = paste("ctb0", dataset$Código.Trabalho, sep = ""),
      dataset_titulo = 
        paste("Conjunto de dados do ", tolower(dataset$Nível.do.Levantamento.Pedológico), " '",
              dataset$Título.do.Trabalho, "'", sep = ""),
      dataset_descricao = 
        paste("Conjunto de dados públicos do solo originalmente obtidos do Sistema de Informação de Solos ", 
              "Brasileiros (SISB), ",
              "construído e mantido pela Embrapa Solos (Rio de Janeiro) e Embrapa Informática Agropecuária ",
              "(Campinas), referente ao ", tolower(dataset$Nível.do.Levantamento.Pedológico), " '", 
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
              "foi ou é usado para organizar estruturalmente a presente versão do conjunto de dados. ",
              "O emprego deste conjunto de dados para finalidades profissionais e/ou comerciais deve ser ",
              "precedido pelo contato com a Embrapa.",
              sep = ""
              ),
      dataset_versao = "2",
      dataset_licenca = "CC BY 4.0",
      publicacao_data = "xx-xx-2017",
      # organization_name = "Embrapa Informática Agropecuária / Embrapa Solos; INSERIR ORGANIZAÇÃO AUTORA",
      organizacao_nome = "",
      organizacao_url = "",
      organizacao_pais_id = "Brasil",
      organizacao_municipio_id = "",
      organizacao_codigo_postal = "",
      organizacao_rua_nome = "",
      organizacao_rua_numero = "",
      autor_nome = paste(gsub(",", ";", dataset$Autor), sep = ""),
      autor_email = "",
      contribuidor_nome = "Alessandro Samuel-Rosa",
      contribuidor_email	=  "alessandrosamuelrosa@gmail.com",
      contribuidor_organizacao = "Universidade Federal de Santa Maria (UFSM)",
      dataset_referencia_1 = dataset$Referência.Bibliográfica,
      dataset_referencia_2 = "https://www.bdsolos.cnptia.embrapa.br/consulta_publica.html",
      dataset_referencia_3 = "http://doi.org/10.2136/sssaj2004.0140",
      area_conhecimento = "Gênese, Morfologia e Classificação dos Solos",
      palavras_chave = "",
      categoria_vcge = "Pesquisa científica e tecnologia"  
    )
    dataset <- t(dataset)
    colnames(dataset) <- "item"
    write.table(
      dataset,
      file = paste("data/raw/ctb0", file_name[i], "-dataset.csv", sep = ""), 
      sep = "\t", fileEncoding = "UTF-8", dec = ",")
    
    # observation
    observation <- unique(db[[i]][, cols_observation])
    # colnames(observation)[c(1:3, 5, 8, 9)] <- 
      # c("observation_id", "observation_id_book", "observation_id_field", "coord_system", "state_id", 
        # "city_id")
    colnames(observation)[c(1, 2, 4, 5, 8, 9)] <- 
      c("observacao_id", "sisb_id", "observacao_data", "coord_sistema", "estado_id", "municipio_id")
    observation <- cbind(
      observation[, c(1:5, 7, 6)],
      coord_precisao = "",
      coord_fonte = "",
      pais_id = "BR",
      observation[, c(8, 9)],
      amostra_tipo = "SIMPLES",
      amostra_quanti = as.character(1),
      amostra_area = as.character(1),
      observation[, 10:ncol(observation)]
    )
    colnames(observation) <- gsub("y_coord", "coord_y", colnames(observation), fixed = TRUE)
    colnames(observation) <- gsub("x_coord", "coord_x", colnames(observation), fixed = TRUE)
    colnames(observation) <- gsub("...", ".", colnames(observation), fixed = TRUE)
    colnames(observation) <- gsub("..", ".", colnames(observation), fixed = TRUE)
    colnames(observation) <- gsub(".", "_", colnames(observation), fixed = TRUE)
    colnames(observation) <- tolower(colnames(observation))
    colnames(observation) <- gsub("_$", "", colnames(observation))
    colnames(observation) <- stringr::str_trim(colnames(observation))
    colnames(observation) <- gsub("_de_", "_", colnames(observation), fixed = TRUE)
    colnames(observation) <- gsub("_da_", "_", colnames(observation), fixed = TRUE)
    colnames(observation) <- 
      gsub("responsável_is_pela_descrição", "autor_descricao", colnames(observation), fixed = TRUE)
    colnames(observation) <- gsub("grupamento_", "", colnames(observation), fixed = TRUE)
    colnames(observation) <- gsub("ª", "", colnames(observation), fixed = TRUE)
    colnames(observation) <- gsub("_das_amostras$", "", colnames(observation))
    colnames(observation) <- gsub("_m$", "", colnames(observation))
    colnames(observation) <- gsub("_por_", "_", colnames(observation), fixed = TRUE)
    colnames(observation) <- gsub("^ocorrência_", "ocorre_", colnames(observation))
    colnames(observation) <- gsub("^presença_", "ocorre_", colnames(observation))
    colnames(observation) <- gsub("_contato_lico_", "_contato_litico_", colnames(observation), fixed = TRUE)
    colnames(observation) <- gsub("^frequência_", "freq_", colnames(observation))
    colnames(observation) <- gsub("horizonte_diagnóstico_", "hz_diag_", colnames(observation), fixed = TRUE)
    colnames(observation) <- gsub("^profundidade_", "prof_", colnames(observation))
    colnames(observation) <- iconv(colnames(observation), from = "UTF-8", to = 'ASCII//TRANSLIT')
    colnames(observation) <- 
      gsub("informacoes_complementares", "extra_info", colnames(observation), fixed = TRUE)
    colnames(observation) <- gsub("_para_", "_", colnames(observation), fixed = TRUE)
    
    write.table(
      observation, 
      file = paste("data/raw/ctb0", file_name[i], "-observacao.csv", sep = ""), 
      sep = "\t", fileEncoding = "UTF-8", row.names = FALSE, dec = ",")
    
    # layer
    drop_cols <- 
      c("Data.da.Coleta", "land_use.y", "litology.y", "Link.para.Descrição.em.PDF", "Sistema.de.Coordenada",
        "Latitude.Graus", "Latitude.Minutos", "Latitude.Segundos", "Latitude.Hemisfério", "Longitude.Graus",
        "Longitude.Minutos", "Longitude.Segundos", "Longitude.Hemisfério", "Northing", "Easting", 
        "Informações.Complementares.1", "Classe.de.Solos.Nível.3", "X1ª.Ocorrência", "X2ª.Ocorrência", 
        "X3ª.Ocorrência", "X4ª.Ocorrência", "Classe.de.Solos.Nível.2", "Classe.de.Solos.Nível.1", 
        "Descrição.Original", "X", "litology", "profile_id", cols_observation, cols_layer, cols_dataset)
    drop_cols_idx <- unique(match(drop_cols, colnames(db[[i]])))
    layer <- cbind(db[[i]][, cols_layer], db[[i]][, -drop_cols_idx])
    layer <- layer[with(layer, order(Número.PA, Código.Horizonte)), ]
    colnames(layer) <- 
      c("observacao_id", "camada_nome", "amostra_codigo", "profund_sup", "profund_inf",
        "fe_sulfurico_xxx", "fe_ditionito_xxx", "fe_oxalato_xxx", "fe_pirofosfato_xxx", "fe_xxx_xxx", 
        colnames(layer)[-c(1:10)])
    layer <- cbind(
      observacao_id = layer$observacao_id,
      camada_numero = "",
      camada_nome = layer$camada_nome,
      amostra_codigo = layer$amostra_codigo,
      layer[, 4:ncol(layer)]
      )
    layer <- cbind(
      layer, 
      extra_files[[1]][
        match(layer$amostra_codigo, extra_files[[1]]$Código.Horizonte), 
        -na.omit(match(drop_cols, colnames(extra_files[[1]])))
        ],
      extra_files[[2]][
        match(layer$amostra_codigo, extra_files[[2]]$Código.Horizonte), 
        -na.omit(match(drop_cols[-match("Descrição.Original", drop_cols)], colnames(extra_files[[2]])))
        ]
      )
    colnames(layer) <- tolower(colnames(layer))
    colnames(layer) <- gsub("...", ".", colnames(layer), fixed = TRUE)
    colnames(layer) <- gsub("..", ".", colnames(layer), fixed = TRUE)
    colnames(layer) <- gsub(".", "_", colnames(layer), fixed = TRUE)
    colnames(layer) <- gsub("_$", "", colnames(layer))
    colnames(layer) <- stringr::str_trim(colnames(layer))
    colnames(layer) <- gsub("_ou$", "", colnames(layer))
    colnames(layer) <- gsub("^cor_da_amostra_", "cor_", colnames(layer))
    colnames(layer) <- gsub("^grau_de_consistência_", "consistencia_", colnames(layer))
    colnames(layer) <- gsub("^molhada_p", "consistencia_molhada_p", colnames(layer))
    colnames(layer) <- gsub("_grau_de_desenvolvimento$", "_desenvolvimento", colnames(layer))
    colnames(layer) <- gsub("^grau_de_desenvolvimento_", "estrutura_desenvolvimento_", colnames(layer))
    colnames(layer) <- gsub("ª", "", colnames(layer), fixed = TRUE)
    colnames(layer) <- gsub("^composição_granulométrica_da_terra_fina_", "terra_fina_", colnames(layer))
    colnames(layer) <- gsub("^frações_da_amostra_total_", "total_", colnames(layer))
    colnames(layer) <- gsub("informações", "info", colnames(layer), fixed = TRUE)
    colnames(layer) <- gsub("^microelementos_", "micro_", colnames(layer))
    colnames(layer) <- gsub("cálcio", "ca", colnames(layer), fixed = TRUE)
    colnames(layer) <- gsub("magnésio", "mg", colnames(layer), fixed = TRUE)
    colnames(layer) <- gsub("potássio", "k", colnames(layer), fixed = TRUE)
    colnames(layer) <- gsub("sódio", "na", colnames(layer), fixed = TRUE)
    colnames(layer) <- gsub("_hidrogênio_", "_", colnames(layer), fixed = TRUE)
    colnames(layer) <- gsub("_alumínio_trocável_", "_", colnames(layer), fixed = TRUE)
    colnames(layer) <- gsub("fósforo_assimilável_", "p_assim_", colnames(layer), fixed = TRUE)
    colnames(layer) <- gsub("carbono_orgânico", "c_org", colnames(layer), fixed = TRUE)
    colnames(layer) <- gsub("nitrogênio_total", "n_total", colnames(layer), fixed = TRUE)
    colnames(layer) <- gsub("alumínio", "al", colnames(layer), fixed = TRUE)
    colnames(layer) <- 
      gsub("complexo_sortivo_saturação_por_al_100_al3_s_al3", "sat_al", colnames(layer), fixed = TRUE)
    colnames(layer) <- gsub("complexo_sortivo_valor_t_s_h_al3", "ctc", colnames(layer), fixed = TRUE)
    colnames(layer) <-
      gsub("complexo_sortivo_valor_v_delta_saturação_por_bases_100_s_t", "sat_bases", colnames(layer), 
           fixed = TRUE)
    colnames(layer) <- 
      gsub("complexo_sortivo_valor_s_ca2_mg2_k_na", "soma_bases", colnames(layer), fixed = TRUE)
    colnames(layer) <- gsub("complexo_sortivo_", "trocavel_", colnames(layer), fixed = TRUE)
    colnames(layer) <- gsub("ataque_sulfúrico_", "sulfurico_", colnames(layer), fixed = TRUE)
    colnames(layer) <- gsub("^cdb_", "ditionito_", colnames(layer))
    colnames(layer) <- gsub("^oxalato_de_amônio_", "oxalato_", colnames(layer))
    colnames(layer) <- gsub("^pirofosfato_de_na_", "pirofosfato_", colnames(layer))
    colnames(layer) <- iconv(colnames(layer), from = "UTF-8", to = 'ASCII//TRANSLIT')
    colnames(layer) <- gsub("^tamanho_", "estrutura_tamanho_", colnames(layer))
    colnames(layer) <- gsub("^forma_", "estrutura_forma_", colnames(layer))
    write.table(
      layer, 
      file = paste("data/raw/ctb0", file_name[i], "-camada.csv", sep = ""), 
      sep = "\t", fileEncoding = "UTF-8", row.names = FALSE, dec = ",")
  }
})

# # Carregar dados compilados por pesquisadores da Esalq
# esalq <- read.csv(
#   "data/raw/fe0003/esalq.csv", head = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")
# # esalq_layer <- read.csv(
#   # "data/raw/fe0003/esalq-layer.csv", head = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")
# 
# # Atribuir informação da UF para dados da Esalq
# states <- raster::shapefile("data/gis/states.shp")
# tmp <- esalq[, c("latitude", "longitude")]
# sp::coordinates(tmp) <- c("latitude", "longitude")[2:1]
# sp::proj4string(tmp) <- sp::proj4string(states)
# tmp <- sp::over(tmp, states)
# esalq$UF <- tmp[, 1]
# 
# # Salvar aquivos da esalq com dados da UF escolhida
# idx <- which(esalq$UF == uf)
# tmp <- esalq[idx, ]
# write.table(
#   tmp, file = paste("data/raw/esalq-", uf, ".csv", sep = ""), sep = "\t", fileEncoding = "UTF-8", 
#   row.names = FALSE)
# # idx <- esalq_layer$esalq_id %in% tmp$esalq_id
# # write.table(
#   # esalq_layer[idx, ], file = paste("data/raw/esalq-layer-", uf, ".csv", sep = ""), sep = "\t", 
#   # fileEncoding = "UTF-8", row.names = FALSE)
# rm(list = ls())
# gc()
# 
# # ctb0770
# esalq_layer <- read.csv(
# "data/raw/fe0003/esalq-layer.csv", head = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")
# ctb <- gsheet::gsheet2tbl(
#   "https://docs.google.com/spreadsheets/d/1OzXAgcC4h-mIGe2B358qVAu33d8vO536JHT8aMVNq8Y/edit#gid=1809478575")
# idx <- esalq_layer$esalq_id %in% ctb$observation_id[agrepl("BR", ctb$observation_id)]
# tmp <- esalq_layer[idx, ]
# write.table(
#   tmp, file = paste("data/raw/esalq-layer-tmp.csv", sep = ""), sep = "\t",
#   fileEncoding = "UTF-8", row.names = FALSE)
