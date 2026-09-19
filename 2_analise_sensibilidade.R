# SOBOL SENSITIVITY ANALYSIS - ALL PHASES
# 3 panels joined vertically: VG, RP, FG

library(sensitivity)
library(lme4)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(scales)
library(patchwork)


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


stats_vars <- final_data %>%
  select(all_of(vars_todas)) %>%
  summarise(across(everything(), list(
    media = \(x) mean(x, na.rm = TRUE),
    dp    = \(x) sd(x,   na.rm = TRUE)
  )))

modelo_sobol <- function(X) {
  X <- as.data.frame(X)
  colnames(X) <- vars_todas
  for (v in vars_todas) {
    mu <- stats_vars[[paste0(v, "_media")]]
    dp <- stats_vars[[paste0(v, "_dp")]]
    X[[v]] <- (X[[v]] - mu) / dp
  }
  X$Simb_dom <- factor("Argissolo_e_Outros",
                       levels = levels(final_data$Simb_dom))
  pred <- predict(mod_misto, newdata = X,
                  re.form = NA, allow.new.levels = TRUE)
  return(as.numeric(pred))
}


n_sobol <- 5000

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

message("Running Sobol (Owen estimator)... this may take a few minutes.")
set.seed(2028)
sobol_owen <- sensitivity::sobolowen(
  model  = modelo_sobol,
  X1 = X1, X2 = X2, X3 = X3,
  nboot  = 100,
  conf   = 0.95,
  varest = 2
)

resultado_owen <- data.frame(
  Variavel   = rownames(sobol_owen$S),
  S1         = sobol_owen$S$original,
  ST         = sobol_owen$T$original,
  IC_low_S1  = sobol_owen$S$`min. c.i.`,
  IC_high_S1 = sobol_owen$S$`max. c.i.`,
  IC_low_ST  = sobol_owen$T$`min. c.i.`,
  IC_high_ST = sobol_owen$T$`max. c.i.`
) %>%
  arrange(desc(ST)) %>%
  mutate(Ranking = row_number())


# Limite X igual para todos os painéis
LIMITE_X <- 0.70

criar_painel <- function(fase, label_fase, mostrar_legenda = FALSE) {
  
  if (fase == "VG") {
    vars_fase <- resultado_owen %>%
      filter(grepl("^VG_", Variavel) | Variavel == "AD_UM")
  } else {
    vars_fase <- resultado_owen %>%
      filter(grepl(paste0("^", fase, "_"), Variavel))
  }
  
  vars_fase <- vars_fase %>% arrange(desc(ST))
  
  dados_grafico <- vars_fase %>%
    select(Variavel, S1, ST, IC_low_S1, IC_high_S1, IC_low_ST, IC_high_ST) %>%
    pivot_longer(cols = c(S1, ST), names_to = "Tipo", values_to = "Valor") %>%
    mutate(
      IC_low  = case_when(Tipo == "S1" ~ IC_low_S1, Tipo == "ST" ~ IC_low_ST),
      IC_high = case_when(Tipo == "S1" ~ IC_high_S1, Tipo == "ST" ~ IC_high_ST),
      IC_low_plot  = pmax(IC_low, 0),
      IC_high_plot = pmin(IC_high, LIMITE_X * 0.98),
      Tipo = recode(Tipo,
                    S1 = "First Order (S1)",
                    ST = "Total Effect (ST)")
    )
  
  ordem <- vars_fase %>% arrange(ST) %>% pull(Variavel)
  dados_grafico$Variavel <- factor(dados_grafico$Variavel, levels = ordem)
  
  g <- ggplot(dados_grafico,
              aes(x = Valor, y = Variavel, fill = Tipo)) +
    geom_col(position = position_dodge(width = 0.7),
             width = 0.65, color = "black", linewidth = 0.3) +
    geom_errorbar(aes(xmin = IC_low_plot, xmax = IC_high_plot),
                  position = position_dodge(width = 0.7),
                  width = 0.25, linewidth = 0.4) +
    scale_fill_manual(values = c(
      "First Order (S1)"  = "#2ca02c",
      "Total Effect (ST)" = "#1f77b4"
    )) +
    scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, LIMITE_X),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = label_fase,
      x     = "Sensitivity Index",
      y     = NULL,
      fill  = "Index"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title         = element_text(face = "bold", size = 13,
                                        color = "black",
                                        margin = margin(b = 4)),
      legend.position    = ifelse(mostrar_legenda, "bottom", "none"),
      panel.grid.major.y = element_blank(),
      panel.border       = element_rect(color = "gray80", fill = NA),
      axis.text.y        = element_text(face = "bold", color = "black", size = 9),
      axis.title.x       = element_text(size = 11),
      plot.margin        = margin(8, 10, 5, 10)
    )
  
  return(list(grafico = g, n_vars = nrow(vars_fase)))
}


res_VG <- criar_painel("VG", "Vegetative Phase (VG) + Soil Water",
                       mostrar_legenda = FALSE)
res_RP <- criar_painel("RP", "Reproductive Phase (RP)",
                       mostrar_legenda = FALSE)
res_FG <- criar_painel("FG", "Grain Filling Phase (FG)",
                       mostrar_legenda = TRUE)

grafico_final <- res_VG$grafico /
  res_RP$grafico /
  res_FG$grafico +
  plot_annotation(
    title    = "",
    subtitle = paste0(""),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 11, color = "gray40")
    )
  )

print(grafico_final)

if (!dir.exists("resultados_sobol")) dir.create("resultados_sobol")

altura_total <- res_VG$n_vars * 0.38 +
  res_RP$n_vars * 0.38 +
  res_FG$n_vars * 0.38 + 3

ggsave("resultados_sobol/SOBOL_TODAS_AS_FASES.png",
       grafico_final,
       width  = 13,
       height = altura_total,
       dpi    = 300)

cat("\nSaved: resultados_sobol/SOBOL_TODAS_AS_FASES.png\n")
cat("VG:", res_VG$n_vars, "| RP:", res_RP$n_vars,
    "| FG:", res_FG$n_vars, "variables\n")
