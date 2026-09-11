# ==============================================================================
# ANÁLISE DE SENSIBILIDADE - PRCC
# Partial Rank Correlation Coefficient
# Produtividade do Arroz de Terras Altas
# ==============================================================================

library(sensitivity)
library(lme4)
library(dplyr)
library(ggplot2)
library(readxl)
library(scales)

mod_misto <- readRDS(
  "/Users/mariavitoriadiastorres/Downloads/modelo_misto_simb.rds"
)


# DEFINIR AS VARIÁVEIS (climáticas + AD_UM)

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

vars_todas <- c(vars_climaticas, "AD_UM")

final_data <- readxl::read_excel(
  "/Users/mariavitoriadiastorres/Downloads/dados_dezembro.xlsx"
)

vars_climaticas_base <- c(
  "T2M", "T2M_MAX", "T2M_MIN", "PRECTOTCORR",
  "GWETROOT", "RH2M", "ALLSKY_SFC_PAR_TOT",
  "EVPTRNS", "WS2M", "CDD0"
)

padrao_manter   <- paste(vars_climaticas_base, collapse = "|")
cols_clim_ok    <- grepl(padrao_manter, names(final_data))
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


# CALCULAR MÉDIA E DESVIO-PADRÃO
stats_vars <- final_data %>%
  select(all_of(vars_todas)) %>%
  summarise(across(everything(), list(
    media = \(x) mean(x, na.rm = TRUE),
    dp    = \(x) sd(x,   na.rm = TRUE)
  )))


# GERAR AMOSTRAS NA ESCALA ORIGINAL
n_prcc <- 5000
set.seed(2025)

X_amostras <- as.data.frame(mapply(function(v) {
  mu <- stats_vars[[paste0(v, "_media")]]
  dp <- stats_vars[[paste0(v, "_dp")]]
  rnorm(n_prcc, mean = mu, sd = dp)
}, vars_todas))
colnames(X_amostras) <- vars_todas

# Padronizar para entrar no modelo
X_pad <- X_amostras
for (v in vars_todas) {
  mu <- stats_vars[[paste0(v, "_media")]]
  dp <- stats_vars[[paste0(v, "_dp")]]
  X_pad[[v]] <- (X_pad[[v]] - mu) / dp
}

# Fixar solo na categoria de referência
X_pad$Simb_dom <- factor(
  "Argissolo_e_Outros",
  levels = levels(final_data$Simb_dom)
)

# Predizer usando apenas efeitos fixos
Y_pred <- predict(mod_misto, newdata = X_pad,
                  re.form = NA, allow.new.levels = TRUE)

cat("Predições geradas:", length(Y_pred), "\n")
cat("Resumo das predições:\n")
print(summary(Y_pred))


# CALCULAR PRCC
# Converter para ranks
Y_rank <- rank(Y_pred)
X_rank <- apply(X_amostras, 2, rank)

# Calcular PRCC via correlação parcial
# Usando regressão linear nos ranks
calcular_prcc <- function(X_rank, Y_rank) {
  
  n_vars <- ncol(X_rank)
  prcc_valores <- numeric(n_vars)
  prcc_pvalor  <- numeric(n_vars)
  
  for (i in 1:n_vars) {
    
    # Variável de interesse
    xi <- X_rank[, i]
    
    # Demais variáveis (covariáveis)
    x_outras <- X_rank[, -i, drop = FALSE]
    
    # Resíduo de xi sobre as demais
    fit_xi <- lm(xi ~ x_outras)
    res_xi <- residuals(fit_xi)
    
    # Resíduo de Y sobre as demais
    fit_y <- lm(Y_rank ~ x_outras)
    res_y <- residuals(fit_y)
    
    # Correlação entre os resíduos = PRCC
    cor_test <- cor.test(res_xi, res_y, method = "pearson")
    prcc_valores[i] <- cor_test$estimate
    prcc_pvalor[i]  <- cor_test$p.value
  }
  
  data.frame(
    Variavel  = colnames(X_rank),
    PRCC      = prcc_valores,
    p_valor   = prcc_pvalor,
    Significativo = ifelse(prcc_pvalor < 0.05, "Sim", "Não"),
    Direcao   = ifelse(prcc_valores > 0, "Positivo", "Negativo")
  )
}

message("Calculando PRCC... isso pode levar alguns minutos.")
resultado_prcc <- calcular_prcc(X_rank, Y_rank)

# Ordenar por |PRCC|
resultado_prcc <- resultado_prcc %>%
  arrange(desc(abs(PRCC)))

print(resultado_prcc, row.names = FALSE)


top20_prcc <- resultado_prcc %>%
  slice_head(n = 20)

g_prcc <- ggplot(top20_prcc,
                 aes(x = PRCC,
                     y = reorder(Variavel, PRCC),
                     fill = Direcao)) +
  geom_col(width = 0.7, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "gray40", linewidth = 0.8) +
  scale_fill_manual(values = c(
    "Positivo" = "#2ca02c",
    "Negativo" = "#d62728"
  )) +
  scale_x_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, by = 0.2)
  ) +
  labs(
    title    = "Análise de Sensibilidade — PRCC",
    subtitle = "",
    x        = "PRCC (−1 a +1)",
    y        = NULL,
    fill     = "Direção do efeito"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 15),
    plot.subtitle      = element_text(size = 11, color = "gray40"),
    legend.position    = "bottom",
    panel.grid.major.y = element_blank(),
    axis.text.y        = element_text(face = "bold", color = "black")
  )

print(g_prcc)

# ==============================================================================
# 10. SALVAR
# ==============================================================================

if (!dir.exists("resultados_sobol")) dir.create("resultados_sobol")

ggsave("resultados_sobol/PRCC_ARROZ.png",
       g_prcc, width = 12, height = 9, dpi = 300)

write.csv(resultado_prcc,
          "resultados_sobol/resultado_prcc.csv",
          row.names = FALSE)

cat("\nAnálise PRCC concluída!\n")
cat("Resultados salvos em 'resultados_sobol/'\n")

cat("\nTop 10 variáveis por |PRCC|:\n")
print(resultado_prcc %>%
        slice_head(n = 10) %>%
        select(Variavel, PRCC, p_valor, Significativo),
      row.names = FALSE)

cat("\nVariáveis com efeito positivo significativo:",
    sum(resultado_prcc$PRCC > 0 & resultado_prcc$Significativo == "Sim"), "\n")
cat("Variáveis com efeito negativo significativo:",
    sum(resultado_prcc$PRCC < 0 & resultado_prcc$Significativo == "Sim"), "\n")
cat("Variáveis não significativas:",
    sum(resultado_prcc$Significativo == "Não"), "\n")