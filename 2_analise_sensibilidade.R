# GRÁFICOS DE SOBOL POR FASE FENOLÓGICA
# 3 gráficos separados: VG, RP, FG
# Todas as variáveis de cada fase + AD_UM no gráfico VG
library(sensitivity)
library(lme4)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(scales)

mod_misto <- readRDS(
  "/Users/mariavitoriadiastorres/Downloads/modelo_misto_simb.rds"
)

vars_climaticas <- c(
  "VG_T2M_mean", "VG_T2M_median", "VG_T2M_iqr", "VG_T2M_q10",
  "VG_T2M_MIN_cumulative", "VG_T2M_MAX_cumulative",
  "VG_RH2M_iqr", "VG_RH2M_cumulative", "VG_RH2M_q90",
  "VG_GWETROOT_mean", "VG_GWETROOT_median", "VG_GWETROOT_iqr", "VG_GWETROOT_cumulative",
  "VG_ALLSKY_SFC_PAR_TOT_q90",
  "VG_EVPTRNS_mean", "VG_EVPTRNS_q90",
  "VG_WS2M_mean", "VG_WS2M_q90",
  "VG_CDD0_median", "VG_CDD0_iqr", "VG_CDD0_cumulative",
  "RP_T2M_MIN_cumulative", "RP_T2M_MIN_q90", "RP_T2M_MIN_q10",
  "RP_T2M_MAX_mean", "RP_T2M_MAX_iqr", "RP_T2M_MAX_cumulative", "RP_T2M_MAX_q90",
  "RP_PRECTOTCORR_mean", "RP_PRECTOTCORR_median", "RP_PRECTOTCORR_iqr", "RP_PRECTOTCORR_q10",
  "RP_RH2M_cumulative",
  "RP_GWETROOT_median",
  "RP_ALLSKY_SFC_PAR_TOT_mean", "RP_ALLSKY_SFC_PAR_TOT_iqr",
  "RP_ALLSKY_SFC_PAR_TOT_cumulative", "RP_ALLSKY_SFC_PAR_TOT_q90",
  "RP_EVPTRNS_mean", "RP_EVPTRNS_cumulative",
  "RP_WS2M_iqr",
  "RP_CDD0_mean",
  "FG_T2M_MIN_iqr", "FG_T2M_MIN_cumulative", "FG_T2M_MIN_q10",
  "FG_PRECTOTCORR_median", "FG_PRECTOTCORR_iqr",
  "FG_RH2M_mean", "FG_RH2M_median", "FG_RH2M_cumulative", "FG_RH2M_q90",
  "FG_GWETROOT_median",
  "FG_ALLSKY_SFC_PAR_TOT_iqr",
  "FG_EVPTRNS_q90",
  "FG_WS2M_cumulative", "FG_WS2M_q90", "FG_WS2M_q10",
  "FG_CDD0_iqr"
)

# Adicionar AD_UM
vars_todas <- c(vars_climaticas, "AD_UM")

cat("Número total de variáveis:", length(vars_todas), "\n")

final_data <- readxl::read_excel(
  "/Users/mariavitoriadiastorres/Downloads/dados_dezembro.xlsx"
)

vars_climaticas_base <- c(
  "T2M", "T2M_MAX", "T2M_MIN", "PRECTOTCORR",
  "GWETROOT", "RH2M", "ALLSKY_SFC_PAR_TOT",
  "EVPTRNS", "WS2M", "CDD0"
)

padrao_manter  <- paste(vars_climaticas_base, collapse = "|")
cols_clim_ok   <- grepl(padrao_manter, names(final_data))
cols_clim_todas <- grepl("^(veg_|repro_|gf_)", names(final_data))
final_data <- final_data[, !cols_clim_todas | cols_clim_ok]
names(final_data) <- gsub("^veg_",   "VG_", names(final_data))
names(final_data) <- gsub("^repro_", "RP_", names(final_data))
names(final_data) <- gsub("^gf_",    "FG_", names(final_data))

final_data <- final_data %>%
  mutate(
    Simb_dom = case_when(
      grepl("Latossolo", Simb) ~ "Latossolo",
      grepl("Neossolo",  Simb) ~ "Neossolo",
      TRUE ~ "Argissolo_e_Outros"
    ),
    Simb_dom = factor(Simb_dom)
  ) %>%
  group_by(TRIAL, GEN) %>%
  filter(n() == 4) %>%
  ungroup() %>%
  mutate(across(where(is.character), as.factor)) %>%
  group_by(TRIAL, GEN) %>%
  summarise(
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
    across(where(is.factor),  ~ first(.x)),
    .groups = "drop"
  ) %>%
  filter(!is.na(GY), GY > 900, !is.na(Simb), !is.na(AD_UM))


# MÉDIA E DESVIO-PADRÃO DE TODAS AS VARIÁVEIS


stats_vars <- final_data %>%
  select(all_of(vars_todas)) %>%
  summarise(across(everything(), list(
    media = \(x) mean(x, na.rm = TRUE),
    dp    = \(x) sd(x,   na.rm = TRUE)
  )))


# FUNÇÃO DO MODELO


modelo_sobol <- function(X) {
  X <- as.data.frame(X)
  colnames(X) <- vars_todas
  
  # Padronizar todas as variáveis
  for (v in vars_todas) {
    mu <- stats_vars[[paste0(v, "_media")]]
    dp <- stats_vars[[paste0(v, "_dp")]]
    X[[v]] <- (X[[v]] - mu) / dp
  }
  
  # Fixar solo na categoria de referência
  X$Simb_dom <- factor(
    "Argissolo_e_Outros",
    levels = levels(final_data$Simb_dom)
  )
  
  pred <- predict(mod_misto, newdata = X,
                  re.form = NA, allow.new.levels = TRUE)
  return(as.numeric(pred))
}

# GERAR AMOSTRAS
n_sobol <- 5000
set.seed(2025)

gerar_matriz <- function(seed) {
  set.seed(seed)
  as.data.frame(mapply(function(v) {
    mu <- stats_vars[[paste0(v, "_media")]]
    dp <- stats_vars[[paste0(v, "_dp")]]
    rnorm(n_sobol, mean = mu, sd = dp)
  }, vars_todas))
}

X1 <- gerar_matriz(2025)
X2 <- gerar_matriz(2026)
X3 <- gerar_matriz(2027)
colnames(X1) <- colnames(X2) <- colnames(X3) <- vars_todas


# RODAR SOBOL - ESTIMADOR DE OWEN
message("Rodando Sobol com Owen... pode demorar alguns minutos.")

set.seed(2028)
sobol_owen <- sensitivity::sobolowen(
  model = modelo_sobol,
  X1 = X1, X2 = X2, X3 = X3,
  nboot = 100,
  conf  = 0.95,
  varest = 2
)

resultado_owen <- data.frame(
  Variavel    = rownames(sobol_owen$S),
  S1          = sobol_owen$S$original,
  ST          = sobol_owen$T$original,
  IC_low_S1   = sobol_owen$S$`min. c.i.`,
  IC_high_S1  = sobol_owen$S$`max. c.i.`,
  IC_low_ST   = sobol_owen$T$`min. c.i.`,
  IC_high_ST  = sobol_owen$T$`max. c.i.`
) %>%
  arrange(desc(ST)) %>%
  mutate(Ranking = row_number())


if (!exists("resultado_owen")) {
  stop("O objeto resultado_owen não foi encontrado. 
       Rode o script sobol_clima_agua.R primeiro.")
}


# FUNÇÃO PARA CRIAR GRÁFICO POR FASE
criar_grafico_fase <- function(fase, titulo, cor_s1, cor_st) {
  
  # Filtrar variáveis da fase (+ AD_UM na fase VG)
  if (fase == "VG") {
    vars_fase <- resultado_owen %>%
      filter(grepl("^VG_", Variavel) | Variavel == "AD_UM")
  } else {
    vars_fase <- resultado_owen %>%
      filter(grepl(paste0("^", fase, "_"), Variavel))
  }
  
  # Ordenar por ST
  vars_fase <- vars_fase %>%
    arrange(desc(ST))
  
  # Preparar dados para o gráfico
  dados_grafico <- vars_fase %>%
    select(Variavel, S1, ST, IC_low_S1, IC_high_S1, IC_low_ST, IC_high_ST) %>%
    pivot_longer(cols = c(S1, ST),
                 names_to = "Tipo",
                 values_to = "Valor") %>%
    mutate(
      IC_low = case_when(
        Tipo == "S1" ~ IC_low_S1,
        Tipo == "ST" ~ IC_low_ST
      ),
      IC_high = case_when(
        Tipo == "S1" ~ IC_high_S1,
        Tipo == "ST" ~ IC_high_ST
      ),
      IC_low_plot  = pmax(IC_low, 0),
      IC_high_plot = pmin(IC_high, max(Valor, na.rm = TRUE) * 1.5),
      Tipo = recode(Tipo,
                    S1 = "First Order (S1)",
                    ST = "Total Effect (ST)"
      )
    )
  
  # Ordenar variáveis por ST
  ordem <- vars_fase %>% arrange(ST) %>% pull(Variavel)
  dados_grafico$Variavel <- factor(dados_grafico$Variavel, levels = ordem)
  
  # Altura dinâmica baseada no número de variáveis
  n_vars <- nrow(vars_fase)
  
  # Gráfico
  g <- ggplot(dados_grafico,
              aes(x = Valor, y = Variavel, fill = Tipo)) +
    geom_col(position = position_dodge(width = 0.7),
             width = 0.65, color = "black") +
    geom_errorbar(aes(xmin = IC_low_plot, xmax = IC_high_plot),
                  position = position_dodge(width = 0.7),
                  width = 0.3, linewidth = 0.4) +
    scale_fill_manual(values = c(
      "First Order (S1)" = cor_s1,
      "Total Effect (ST)" = cor_st
    )) +
    scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 0.70)
    ) +
    labs(
      title    = titulo,
      subtitle = paste(""),
      x        = "Sensitivity Index",
      y        = NULL,
      fill     = "Index"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title         = element_text(face = "bold", size = 15),
      plot.subtitle      = element_text(size = 11, color = "gray40"),
      legend.position    = "bottom",
      panel.grid.major.y = element_blank(),
      axis.text.y        = element_text(face = "bold", color = "black", size = 10)
    )
  
  return(list(grafico = g, n_vars = n_vars))
}


# Fase Vegetativa (VG) + AD_UM
res_VG <- criar_grafico_fase(
  fase   = "VG",
  titulo = "",
  cor_s1 = "#2ca02c",
  cor_st = "#1f77b4"
)

# Fase Reprodutiva (RP)
res_RP <- criar_grafico_fase(
  fase   = "RP",
  titulo = "",
  cor_s1 = "#2ca02c",
  cor_st = "#1f77b4"
)

# Fase de Enchimento de Grãos (FG)
res_FG <- criar_grafico_fase(
  fase   = "FG",
  titulo = "",
  cor_s1 = "#2ca02c",
  cor_st = "#1f77b4"
)


print(res_VG$grafico)
print(res_RP$grafico)
print(res_FG$grafico)



if (!dir.exists("resultados_sobol")) dir.create("resultados_sobol")

# Altura proporcional ao número de variáveis
altura_VG <- max(6, res_VG$n_vars * 0.4)
altura_RP <- max(6, res_RP$n_vars * 0.4)
altura_FG <- max(6, res_FG$n_vars * 0.4)

ggsave("resultados_sobol/SOBOL_FASE_VG.png",
       res_VG$grafico,
       width = 12, height = altura_VG, dpi = 300)

ggsave("resultados_sobol/SOBOL_FASE_RP.png",
       res_RP$grafico,
       width = 12, height = altura_RP, dpi = 300)

ggsave("resultados_sobol/SOBOL_FASE_FG.png",
       res_FG$grafico,
       width = 12, height = altura_FG, dpi = 300)

cat("\nGráficos salvos em 'resultados_sobol/':\n")
cat("- SOBOL_FASE_VG.png (", res_VG$n_vars, "variáveis)\n")
cat("- SOBOL_FASE_RP.png (", res_RP$n_vars, "variáveis)\n")
cat("- SOBOL_FASE_FG.png (", res_FG$n_vars, "variáveis)\n")