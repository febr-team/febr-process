# Classificação do uso da terra ####
keys <- list(
  'AGRICULTURA' = c(
    "arroz", "algodão", "aveia", "abacaxi", "agave", "arrado",
    "banana", "batata", "bananeira", "batatinha",
    "cupuaçu", "cana-de-açúcar", "citrus", "castanha", "coco", "café", "cultura", "cacau", "cafezal",
    "coqueiro", "cultivo", "caju", "cultura de subsistência", "cultura anual", "centeio", "capoeira",
    "desmatado", "derrubada",
    "extrativismo", "experimentação", "erva-mate",
    "fruticultura", "feijão", "fumo", "fava",
    "girassol", "gradeada",
    "horticultura",
    "juta",
    "lavoura", "laranja",
    "mata explorada", "mata explorada",
    "mata em exploração", "mata em exploração", 
    "mamona", "mandioca", "milho", "macaxeira", "manga",
    "olericultura",
    "pomar", "pousio", "plantio", "piaçava", "pimenta", "preparo para plantio",
    "roça",
    "sapotí", "soja", "sisal",
    "trigo", "tomate",
    "vinhedo"
  ),
  'CRIAÇÃO ANIMAL' = c(
    "azevém", "algaroba",
    "bovino", "braquiária",
    "capim", "campo nativo", "capim-colonião", "criação", "extensiva", "capim-gordura", 
    "capim-elefante", "caprino", "capim-angola", "capim-guiné", "capim sempre-verde", "capim-jaraguá",
    "campo natural",
    "desmatado", "derrubada",
    "equino", "extensiva",
    "forragem",
    "gado", "gramínea", "grama",
    "leite",
    "ovino",
    "palma", "pastagem", "pecuária", "pasto", "pecuária extensiva", "pastagem natural", "pecuária na caatinga",
    "pastagem artificial", "pastagem plantada", "pecuária no cerrado", "pecuária extensiva no cerrado"
  ),
  # 'OUTRO' = c(
  # ),
  'PROTEÇÃO DA NATUREZA' = c(
    "cobertura vegetal natural", "cerrado", "cobertura natural", "caatinga",
    "floresta", "floresta remanescente", "floresta nativa",
    "mata natural", "mata secundária", "mata atlântica", "mata",
    "nenhum", "não utilizado", "não aproveitado", "não constatado", "não utilizado agrícolamente",
    "não observado",
    "parque nacional",
    "reserva florestal", "reserva", 
    "savana", "sem utilização", "sem uso", "sem uso agrícola", "sem uso aparente",
    "vegetação natural", "vegetação secundária", "vegetação nativa"
  ),
  'SILVICULTURA' = c(
    "carvão",
    "eucalipto",
    "florestamento",
    "horto florestal",
    "lenha",
    "madeira", 
    "pinus", "pinheiro", "plantio de eucalipto",
    "reflorestamento", "reflorestamento de pinus", "reflorestamento de eucalipto",
    "seringueira"
  ),
  'URBANO' = c(
    "área de empréstimo", "extração areia", "área urbana",
    "exploração mineral",
    "loteamento",    
    "mineração",
    "recreação",
    "urbana"
  )
)
guessClass <-
  function (obj, keywords, max.distance = 0.1) {
    out <- 
      sapply(keywords, function (x) {
        rowSums(
          sapply(x, function (y) {
            agrepl(pattern = y, x = obj, ignore.case = TRUE, max.distance = max.distance)
          })
        )
      })
    out <- apply(out, 1, function (x) round(x / sum(x), 4))
    return (out)
  }
