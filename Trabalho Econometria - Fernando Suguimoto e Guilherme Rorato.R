# ── 0. Instalar os pacotes ──────────────────────────────────────────────────

pacotes <- c("PNADcIBGE", "survey", "sandwich", "lmtest", "car", "flextable", "officer")
faltam <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]

if (length(faltam) > 0L) {
  install.packages(faltam, repos = "https://cloud.r-project.org")
}

# Necessário para a importação em sessão limpa.
library(PNADcIBGE)

# ── 1. Importar a PNAD Contínua ─────────────────────────────────────────────

if (!exists("pnadc_raw", inherits = FALSE)) {
  pnadc_raw <- PNADcIBGE::get_pnadc(
    year = 2025,
    quarter = 4,
    design = FALSE,
    labels = FALSE
  )
}

pnadc_raw <- as.data.frame(pnadc_raw)

# ── 2. Conferir e preparar as colunas ──────────────────────────────────────

print(dim(pnadc_raw))
print(head(pnadc_raw))
print(names(pnadc_raw))

variaveis <- c(
  idade = "V2009",
  ocupacao = "VD4002",
  rendimento_mensal = "VD4016",
  horas_semanais = "V4039",
  anos_estudo = "VD3005",
  sexo = "V2007",
  cor_raca = "V2010",
  uf = "UF",
  situacao_domicilio = "V1022",
  upa = "UPA",
  peso = "V1028",
  estrato = "Estrato",
  nivel_instrucao = "VD3004"
)

faltantes <- setdiff(c("Ano", "Trimestre", unname(variaveis)), names(pnadc_raw))

if (length(faltantes) > 0L) {
  stop("Colunas ausentes na base: ", paste(faltantes, collapse = ", "))
}

print(with(pnadc_raw, table(Ano, Trimestre, useNA = "ifany")))

stopifnot(
  "A base precisa conter apenas dados do 4º trimestre de 2025." =
    nrow(pnadc_raw) > 0L &&
    isTRUE(all(pnadc_raw$Ano == 2025 & pnadc_raw$Trimestre == 4))
)

base_bruta <- pnadc_raw[, unname(variaveis), drop = FALSE]
names(base_bruta) <- names(variaveis)

for (coluna in names(base_bruta)) {
  valores <- base_bruta[[coluna]]
  
  if (!is.numeric(valores)) {
    texto <- trimws(as.character(valores))
    texto[!is.na(texto) & texto == ""] <- NA_character_
    numeros <- suppressWarnings(as.numeric(texto))
    invalidos <- !is.na(texto) & is.na(numeros)
    
    if (any(invalidos)) {
      stop(
        "A coluna ", coluna, " contém códigos não numéricos: ",
        paste(head(unique(texto[invalidos]), 5), collapse = ", ")
      )
    }
    
    base_bruta[[coluna]] <- numeros
  }
}

str(base_bruta)

# ── 3. Aplicar os filtros e calcular o rendimento por hora ─────────────────

base_idade <- subset(base_bruta, idade >= 25 & idade <= 64)
base_ocupada <- subset(base_idade, ocupacao == 1)
print(table(base_ocupada$horas_semanais, useNA = "ifany"))

base_rendimento <- subset(base_ocupada, is.finite(rendimento_mensal))
base_horas <- subset(base_rendimento, horas_semanais >= 1 & horas_semanais <= 120)

base_horas$horas_mensais <- base_horas$horas_semanais * (52 / 12)
base_horas$rendimento_hora <-
  base_horas$rendimento_mensal / base_horas$horas_mensais

# Zero é um código válido de escolaridade.
base_escolaridade <- subset(base_horas, !is.na(anos_estudo))

# V2010 = 9 indica cor ou raça ignorada.
base_escolaridade$cor_raca[which(base_escolaridade$cor_raca == 9)] <- NA

controles <- c("idade", "sexo", "cor_raca", "uf", "situacao_domicilio")
dados_controles <- base_escolaridade[, controles, drop = FALSE]
base_regressao <- base_escolaridade[complete.cases(dados_controles), , drop = FALSE]

# ── 4. Tabela de perdas amostrais ──────────────────────────────────────────

etapas <- c(
  "Base importada",
  "Idade entre 25 e 64 anos",
  "Pessoas ocupadas",
  "Rendimento mensal disponível e finito",
  "Horas habituais entre 1 e 120 por semana",
  "Escolaridade informada",
  "Controles completos"
)

observacoes <- c(
  nrow(base_bruta), nrow(base_idade), nrow(base_ocupada),
  nrow(base_rendimento), nrow(base_horas), nrow(base_escolaridade),
  nrow(base_regressao)
)

perdas_amostrais <- data.frame(
  etapa = etapas,
  observacoes = observacoes,
  excluidas_na_etapa = c(NA_integer_, -diff(observacoes))
)

print(perdas_amostrais, row.names = FALSE)
print(colSums(is.na(dados_controles)))
stopifnot("A amostra ficou vazia." = nrow(base_regressao) > 0L)

print(summary(base_regressao$rendimento_hora))
print(c(
  rendimento_mensal_zero = sum(base_regressao$rendimento_mensal == 0),
  rendimento_mensal_negativo = sum(base_regressao$rendimento_mensal < 0),
  escolaridade_zero = sum(base_regressao$anos_estudo == 0)
))

# ── 5. Estimar os modelos por MQO ──────────────────────────────────────────

dados_modelo <- base_regressao
n_esperado <- tail(perdas_amostrais$observacoes, 1)

stopifnot(
  nrow(dados_modelo) == n_esperado,
  "O log exige rendimento por hora positivo e finito." =
    all(is.finite(dados_modelo$rendimento_hora) & dados_modelo$rendimento_hora > 0)
)

dados_modelo$sexo <- relevel(factor(dados_modelo$sexo), ref = "1")
dados_modelo$cor_raca <- relevel(factor(dados_modelo$cor_raca), ref = "1")
dados_modelo$uf <- relevel(factor(dados_modelo$uf), ref = "35")
dados_modelo$situacao_domicilio <- relevel(
  factor(dados_modelo$situacao_domicilio), ref = "1"
)

modelo_log <- lm(
  log(rendimento_hora) ~ anos_estudo + idade + I(idade^2) +
    sexo + cor_raca + uf + situacao_domicilio,
  data = dados_modelo,
  na.action = na.fail,
  singular.ok = FALSE
)

modelo_nivel <- update(modelo_log, rendimento_hora ~ .)

n_modelos <- c(
  tabela_de_perdas = n_esperado,
  modelo_log = nobs(modelo_log),
  modelo_nivel = nobs(modelo_nivel)
)
print(n_modelos)

stopifnot(
  all(n_modelos == n_esperado),
  identical(rownames(model.frame(modelo_log)), rownames(model.frame(modelo_nivel)))
)

print(summary(modelo_log))
print(summary(modelo_nivel))

beta_log <- coef(modelo_log)[["anos_estudo"]]
beta_nivel <- coef(modelo_nivel)[["anos_estudo"]]

print(c(
  beta_escolaridade_log = beta_log,
  diferenca_percentual = 100 * expm1(beta_log),
  diferenca_em_reais_por_hora = beta_nivel
))

# ── 6. Diagnósticos do modelo principal ────────────────────────────────────

inferencia_upa <- function(modelo, dados) {
  stopifnot(
    "As linhas do modelo e da base estão desalinhadas." =
      identical(rownames(model.frame(modelo)), rownames(dados)),
    "Há UPAs ausentes ou não finitas." = all(is.finite(dados$upa))
  )
  
  grupos <- length(unique(dados$upa))
  stopifnot("São necessárias pelo menos duas UPAs." = grupos >= 2L)
  
  matriz <- sandwich::vcovCL(modelo, cluster = dados$upa, type = "HC1")
  
  list(
    grupos = grupos,
    matriz = matriz,
    testes = lmtest::coeftest(modelo, vcov. = matriz, df = grupos - 1L)
  )
}

principal_upa <- inferencia_upa(modelo_log, dados_modelo)
vcov_hc1 <- sandwich::vcovHC(modelo_log, type = "HC1")

cat("\nNúmero de UPAs:", principal_upa$grupos, "\n")
cat("\nComparação dos três erros-padrão\n")
print(cbind(
  estimativa = coef(modelo_log),
  convencional = sqrt(diag(vcov(modelo_log))),
  HC1 = sqrt(diag(vcov_hc1)),
  agrupado_UPA = sqrt(diag(principal_upa$matriz))
), digits = 6)

cat("\nCoeficientes com erros-padrão HC1\n")
print(lmtest::coeftest(modelo_log, vcov. = vcov_hc1))

cat("\nCoeficientes com erros-padrão agrupados por UPA\n")
print(principal_upa$testes)

termos_idade <- c("idade", "I(idade^2)")
nomes_coef <- names(coef(modelo_log))
posicoes_idade <- match(termos_idade, nomes_coef)
stopifnot(!anyNA(posicoes_idade))

restricoes_idade <- diag(length(nomes_coef))[posicoes_idade, , drop = FALSE]
colnames(restricoes_idade) <- nomes_coef
rownames(restricoes_idade) <- paste(termos_idade, "= 0")

cat("\nTeste F de idade e idade² com HC1\n")
print(car::linearHypothesis(
  modelo_log, restricoes_idade, rhs = c(0, 0),
  vcov. = vcov_hc1, test = "F"
))

cat("\nTeste F de idade e idade² por UPA\n")
print(car::linearHypothesis(
  modelo_log, restricoes_idade, rhs = c(0, 0),
  vcov. = principal_upa$matriz, test = "F", error.df = principal_upa$grupos - 1L
))

percentis_rendimento <- quantile(
  dados_modelo$rendimento_hora,
  probs = c(0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99)
)
print(percentis_rendimento)

p1 <- percentis_rendimento[["1%"]]
p99 <- percentis_rendimento[["99%"]]
colunas_extremos <- c("rendimento_mensal", "horas_semanais", "rendimento_hora")

abaixo_p1 <- dados_modelo[
  dados_modelo$rendimento_hora < p1, colunas_extremos, drop = FALSE
]
acima_p99 <- dados_modelo[
  dados_modelo$rendimento_hora > p99, colunas_extremos, drop = FALSE
]

cat("\nCasos abaixo do P1 e acima do P99\n")
print(c(abaixo_P1 = nrow(abaixo_p1), acima_P99 = nrow(acima_p99)))
print(summary(abaixo_p1))
print(head(abaixo_p1[order(abaixo_p1$rendimento_hora), ], 20))
print(summary(acima_p99))
print(head(acima_p99[order(-acima_p99$rendimento_hora), ], 20))

pasta_graficos <- "graficos_diagnostico"
dir.create(pasta_graficos, showWarnings = FALSE)
residuos_log <- residuals(modelo_log)
ajustados_log <- fitted(modelo_log)

cat("\nMediana dos resíduos por anos de estudo\n")
print(round(tapply(residuos_log, dados_modelo$anos_estudo, median), 3))

png(file.path(pasta_graficos, "residuos_por_escolaridade.png"),
    width = 1800, height = 1100, res = 180)
boxplot(
  residuos_log ~ factor(anos_estudo), data = dados_modelo,
  outline = FALSE, las = 1, col = "grey90",
  xlab = "Anos de estudo", ylab = "Resíduos do modelo em log",
  main = "Resíduos por anos de estudo"
)
abline(h = 0, lty = 2, col = "gray40")
invisible(dev.off())

png(file.path(pasta_graficos, "residuos_vs_ajustados.png"),
    width = 1800, height = 1100, res = 180)
plot(
  ajustados_log, residuos_log, pch = 16, cex = 0.35, las = 1,
  col = grDevices::adjustcolor("gray30", alpha.f = 0.15),
  xlab = "Valores ajustados do log do rendimento por hora",
  ylab = "Resíduos do modelo em log", main = "Resíduos contra valores ajustados"
)
abline(h = 0, lty = 2, col = "gray40")
lines(lowess(ajustados_log, residuos_log), col = "red3", lwd = 2)
invisible(dev.off())

png(file.path(pasta_graficos, "residuos_por_idade.png"),
    width = 2000, height = 1100, res = 180)
boxplot(
  residuos_log ~ factor(idade), data = dados_modelo,
  outline = FALSE, xaxt = "n", las = 1, col = "grey90",
  xlab = "Idade (anos)", ylab = "Resíduos do modelo em log",
  main = "Resíduos por idade"
)
idades <- sort(unique(dados_modelo$idade))
marcas <- seq(1L, length(idades), by = 5L)
axis(1, at = marcas, labels = idades[marcas])
abline(h = 0, lty = 2, col = "gray40")
invisible(dev.off())

cat("\nGráficos salvos em:", normalizePath(pasta_graficos), "\n")

# ── 7. Robustez ───────────────────────────────────────────────────────────

dados_robustez <- subset(dados_modelo, !is.na(nivel_instrucao))
stopifnot(
  nrow(dados_robustez) > 0L,
  all(dados_robustez$nivel_instrucao %in% 1:7)
)

cat("\nObservações com VD3004:", nrow(dados_robustez),
    "de", nrow(dados_modelo), "\n")

dados_robustez$etapa <- cut(
  dados_robustez$nivel_instrucao,
  breaks = c(0, 2, 4, 6, 7),
  labels = c("Sem fundamental completo", "Fundamental", "Médio", "Superior")
)

# 7.1. Tabela cruzada e critério descritivo de sobreposição.
# O código 16 de anos de estudo reúne 16 anos ou mais.
cruzamento_escolaridade <- with(dados_robustez, table(anos_estudo, etapa))
cat("\nAnos de estudo por etapa mais alta concluída\n")
print(cruzamento_escolaridade)

contagens <- unclass(cruzamento_escolaridade)

for (etapa in 2:4) {
  antes <- rowSums(contagens[, seq_len(etapa - 1L), drop = FALSE])
  depois <- rowSums(contagens[, etapa:4, drop = FALSE])
  percentual_menor <- 100 * pmin(antes, depois) / (antes + depois)
  
  sobreposicao <- data.frame(
    anos_estudo = rownames(contagens),
    antes = antes,
    depois = depois,
    menor_grupo_percentual = round(percentual_menor, 2),
    atende_criterio = pmin(antes, depois) >= 100L & percentual_menor >= 5
  )
  
  cat("\nSobreposição para conclusão de", colnames(contagens)[etapa], "\n")
  print(sobreposicao[antes > 0 & depois > 0, ], row.names = FALSE)
}

# 7.2. Modelos na mesma amostra e teste F conjunto.
dados_robustez$concluiu_fundamental <- as.integer(dados_robustez$nivel_instrucao >= 3)
dados_robustez$concluiu_medio <- as.integer(dados_robustez$nivel_instrucao >= 5)
dados_robustez$concluiu_superior <- as.integer(dados_robustez$nivel_instrucao == 7)

modelo_linear_comparavel <- update(modelo_log, data = dados_robustez)
modelo_etapas <- update(
  modelo_linear_comparavel,
  . ~ . + concluiu_fundamental + concluiu_medio + concluiu_superior
)

stopifnot(identical(
  rownames(model.frame(modelo_linear_comparavel)),
  rownames(model.frame(modelo_etapas))
))

linear_comparavel_upa <- inferencia_upa(modelo_linear_comparavel, dados_robustez)
etapas_upa <- inferencia_upa(modelo_etapas, dados_robustez)

cat("\nCoeficientes do modelo com etapas, agrupados por UPA\n")
print(etapas_upa$testes)

teste_etapas <- car::linearHypothesis(
  modelo_etapas,
  c("concluiu_fundamental = 0", "concluiu_medio = 0", "concluiu_superior = 0"),
  vcov. = etapas_upa$matriz, test = "F", error.df = etapas_upa$grupos - 1L
)

cat("\nTeste F conjunto dos indicadores de conclusão\n")
print(teste_etapas)
cat("O teste F rejeita a 5%:", teste_etapas[2, "Pr(>F)"] < 0.05, "\n")

# 7.3. Previsões para combinações observadas, com os controles fixos.
perfil <- as.data.frame(
  cruzamento_escolaridade, responseName = "n", stringsAsFactors = TRUE
)
perfil <- subset(perfil, n > 0)
perfil$anos_estudo <- as.numeric(as.character(perfil$anos_estudo))
perfil$idade <- median(dados_robustez$idade)
perfil$sexo <- factor("1", levels = levels(dados_robustez$sexo))
perfil$cor_raca <- factor("1", levels = levels(dados_robustez$cor_raca))
perfil$uf <- factor("35", levels = levels(dados_robustez$uf))
perfil$situacao_domicilio <- factor("1", levels = levels(dados_robustez$situacao_domicilio))

codigo_etapa <- match(as.character(perfil$etapa), levels(dados_robustez$etapa))
perfil$concluiu_fundamental <- as.integer(codigo_etapa >= 2L)
perfil$concluiu_medio <- as.integer(codigo_etapa >= 3L)
perfil$concluiu_superior <- as.integer(codigo_etapa == 4L)

perfil$log_previsto_linear <- predict(modelo_linear_comparavel, newdata = perfil)
perfil$log_previsto_etapas <- predict(modelo_etapas, newdata = perfil)
perfil$diferenca_percentual <- 100 * expm1(
  perfil$log_previsto_etapas - perfil$log_previsto_linear
)

comparacao_perfis <- perfil[
  order(perfil$anos_estudo, perfil$etapa),
  c("anos_estudo", "etapa", "n", "log_previsto_linear",
    "log_previsto_etapas", "diferenca_percentual")
]
diferencas_relevantes <- subset(comparacao_perfis, abs(diferenca_percentual) >= 5)

cat("\nComparação das previsões por ano e etapa observada\n")
print(comparacao_perfis, row.names = FALSE, digits = 4)
cat("\nCombinações com diferença de pelo menos 5% em magnitude\n")
print(diferencas_relevantes, row.names = FALSE, digits = 4)

# 7.4. Sensibilidade ao corte no rendimento por hora.
dados_sem_extremos <- subset(dados_modelo, rendimento_hora >= p1 & rendimento_hora <= p99)
modelo_sem_extremos <- update(modelo_log, data = dados_sem_extremos)
sem_extremos_upa <- inferencia_upa(modelo_sem_extremos, dados_sem_extremos)

cat("\nSensibilidade aos extremos: coeficiente de anos de estudo\n")
print(rbind(
  amostra_principal = principal_upa$testes["anos_estudo", ],
  sem_extremos = sem_extremos_upa$testes["anos_estudo", ]
))
print(c(
  observacoes_principal = nobs(modelo_log),
  observacoes_sem_extremos = nobs(modelo_sem_extremos)
))

# ── 8. Gerar a tabela de resultados revisada em Word ───────────────────────

mesma_amostra <- nobs(modelo_linear_comparavel) == nobs(modelo_log)

rotulos <- c(
  anos_estudo = "Anos de estudo",
  concluiu_fundamental = "Concluiu o fundamental",
  concluiu_medio = "Concluiu o médio",
  concluiu_superior = "Concluiu o superior",
  idade = "Idade",
  "I(idade^2)" = "Idade²",
  sexo2 = "Mulher",
  cor_raca2 = "Preta",
  cor_raca3 = "Amarela",
  cor_raca4 = "Parda",
  cor_raca5 = "Indígena",
  situacao_domicilio2 = "Rural"
)

formatar <- function(x, casas) {
  formatC(x, format = "f", digits = casas, decimal.mark = ",", big.mark = ".")
}

coluna_modelo <- function(modelo, inferencia, amostra) {
  testes <- inferencia$testes
  presentes <- names(rotulos) %in% rownames(testes)
  resultados <- testes[names(rotulos)[presentes], , drop = FALSE]
  
  p <- resultados[, 4]
  estrelas <- ifelse(
    p < 0.01, "***",
    ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", ""))
  )
  
  linhas <- rep("", length(rotulos))
  indices_presentes <- which(presentes)
  
  for (i in seq_along(indices_presentes)) {
    linha <- indices_presentes[i]
    termo <- names(rotulos)[linha]
    casas <- if (termo == "I(idade^2)") 7L else 4L
    
    linhas[linha] <- paste0(
      formatar(resultados[i, 1], casas), estrelas[i], "\n(",
      formatar(resultados[i, 2], casas), ")"
    )
  }
  
  c(
    linhas, "Sim", formatar(nobs(modelo), 0),
    formatar(summary(modelo)$r.squared, 3),
    formatar(inferencia$grupos, 0), amostra
  )
}

rotulo_amostra <- if (mesma_amostra) "Principal" else "Comum a (1) e (2)"

tabela_resultados <- data.frame(
  variavel = c(
    unname(rotulos), "Dummies de UF", "Observações", "R²",
    "Número de UPAs", "Amostra"
  ),
  m1 = coluna_modelo(modelo_linear_comparavel, linear_comparavel_upa, rotulo_amostra),
  m2 = coluna_modelo(modelo_etapas, etapas_upa, rotulo_amostra),
  m3 = coluna_modelo(modelo_sem_extremos, sem_extremos_upa, "P1 a P99"),
  stringsAsFactors = FALSE
)

notas_tabela <- c(
  "Variável dependente: log natural do rendimento do trabalho principal por hora.",
  "Erros-padrão agrupados por UPA entre parênteses.",
  "Significância: * p < 0,10, ** p < 0,05 e *** p < 0,01.",
  "Categorias de referência: homem, pessoa branca, São Paulo e área urbana.",
  paste(
    "Coluna (2): cada indicador cumulativo mede o salto adicional em relação",
    "à etapa anterior, além da tendência por anos de estudo.",
    "O ponto de partida é o grupo sem fundamental completo."
  ),
  "Coluna (3): exclui rendimentos por hora estritamente abaixo do P1 e acima do P99."
)

if (!mesma_amostra) {
  notas_tabela <- c(
    notas_tabela,
    paste(
      "As colunas (1) e (2) usam a amostra comum com VD3004 válida.",
      "A coluna (3) parte da amostra original antes do corte por rendimento."
    )
  )
}

notas_tabela <- c(
  notas_tabela,
  "Fonte: microdados da PNAD Contínua, 4º trimestre de 2025, IBGE."
)

print(tabela_resultados, row.names = FALSE)

tabela_word <- flextable::flextable(tabela_resultados)
tabela_word <- flextable::set_header_labels(
  tabela_word,
  variavel = "Variável",
  m1 = "(1) Linear",
  m2 = "(2) Com etapas",
  m3 = "(3) Sem extremos"
)
tabela_word <- flextable::theme_booktabs(tabela_word)
tabela_word <- flextable::fontsize(tabela_word, size = 9, part = "all")
tabela_word <- flextable::align(
  tabela_word, j = c("m1", "m2", "m3"), align = "center", part = "all"
)
tabela_word <- flextable::width(tabela_word, j = "variavel", width = 2.15)
tabela_word <- flextable::width(tabela_word, j = c("m1", "m2", "m3"), width = 1.28)
tabela_word <- flextable::add_footer_lines(tabela_word, values = notas_tabela)
tabela_word <- flextable::fontsize(tabela_word, size = 8, part = "footer")

documento <- officer::read_docx()
documento <- officer::body_add_fpar(
  documento,
  officer::fpar(
    officer::ftext(
      "Tabela 1 – Escolaridade e rendimento por hora",
      prop = officer::fp_text(font.size = 11, bold = TRUE)
    )
  )
)
documento <- flextable::body_add_flextable(
  documento, value = tabela_word, align = "left"
)

arquivo_tabela <- "Tabela-Resultados-PNAD-Revisada.docx"
print(documento, target = arquivo_tabela)
cat("\nTabela revisada salva em:", normalizePath(arquivo_tabela), "\n")

# ── 9. Números para a discussão da coluna (1) ──────────────────────────────

cat("\n=== Números para a discussão da coluna (1) ===\n")

modelo_discussao <- modelo_linear_comparavel
testes_discussao <- linear_comparavel_upa$testes

# 9.1. Perfil por idade e intervalo de confiança pelo método delta.
termos_idade <- c("idade", "I(idade^2)")
b_idade <- coef(modelo_discussao)[["idade"]]
b_idade2 <- coef(modelo_discussao)[["I(idade^2)"]]
faixa_idade <- range(dados_robustez$idade)
inclinacoes <- b_idade + 2 * b_idade2 * faixa_idade

cat("\n--- Perfil previsto por idade ---\n")
print(data.frame(
  idade = faixa_idade,
  inclinacao_em_log = round(inclinacoes, 6)
), row.names = FALSE)

if (b_idade2 < 0) {
  idade_maximo <- -b_idade / (2 * b_idade2)
  
  # Gradiente de g(b1, b2) = -b1 / (2*b2) em relação a (b1, b2).
  gradiente <- c(-1 / (2 * b_idade2), b_idade / (2 * b_idade2^2))
  vcov_idade <- linear_comparavel_upa$matriz[termos_idade, termos_idade]
  variancia_delta <- drop(crossprod(gradiente, vcov_idade %*% gradiente))
  erro_padrao_maximo <- sqrt(variancia_delta)
  
  critico_95 <- qt(0.975, df = linear_comparavel_upa$grupos - 1L)
  intervalo_maximo <- idade_maximo + c(-1, 1) * critico_95 * erro_padrao_maximo
  
  print(round(c(
    idade_maximo = idade_maximo,
    erro_padrao_delta = erro_padrao_maximo,
    limite_inferior_95 = intervalo_maximo[1],
    limite_superior_95 = intervalo_maximo[2]
  ), 4))
  
  cat(
    "Estimativa pontual do máximo dentro da faixa observada:",
    idade_maximo >= faixa_idade[1] && idade_maximo <= faixa_idade[2], "\n"
  )
  cat(
    "Intervalo inteiro dentro da faixa observada:",
    intervalo_maximo[1] >= faixa_idade[1] &&
      intervalo_maximo[2] <= faixa_idade[2], "\n"
  )
} else {
  cat("O termo quadrático não é negativo. O perfil não tem máximo interior.\n")
}

# 9.2. Contagens por cor ou raça na amostra da coluna (1).
codigos_cor <- c("1", "2", "3", "4", "5")
grupos_cor <- c("Branca (referência)", "Preta", "Amarela", "Parda", "Indígena")
contagens_cor <- table(factor(dados_robustez$cor_raca, levels = codigos_cor))

cat("\n--- Cor ou raça na amostra da coluna (1) ---\n")
print(data.frame(
  grupo = grupos_cor,
  observacoes = as.integer(contagens_cor),
  percentual_amostra = round(
    100 * as.integer(contagens_cor) / nobs(modelo_discussao), 2
  )
), row.names = FALSE)

# 9.3. Diferenças percentuais exatas dos indicadores da coluna (1).
# Referências: homem, pessoa branca e área urbana, respectivamente.
termos_dummies <- c(
  "sexo2", "cor_raca2", "cor_raca3", "cor_raca4", "cor_raca5",
  "situacao_domicilio2"
)
saida_dummies <- testes_discussao[termos_dummies, , drop = FALSE]

cat("\n--- Diferenças percentuais na coluna (1) ---\n")
print(data.frame(
  categoria = unname(rotulos[termos_dummies]),
  referencia = c("Homem", rep("Branca", 4), "Urbana"),
  coeficiente = round(saida_dummies[, 1], 5),
  diferenca_percentual = round(100 * expm1(saida_dummies[, 1]), 2),
  erro_padrao_upa = round(saida_dummies[, 2], 5),
  valor_p = format.pval(saida_dummies[, 4], digits = 3, eps = 1e-16)
), row.names = FALSE)