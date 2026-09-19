# VIOLIN PLOT - 9 GRÁFICOS
# 3 variáveis mais importantes por fase (VG, RP, FG)
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(caret)


# Função de importância para modelo misto
varImp.merMod <- function(object, ...) {
  summ <- summary(object)$coefficients
  out  <- data.frame(Overall = abs(summ[, "t value"]))
  rownames(out) <- rownames(summ)
  return(out)
}

dados_plot <- final_data_original
names(dados_plot) <- names(dados_plot) |>
  str_replace_all("^veg_",   "VG_") |>
  str_replace_all("^repro_", "RP_") |>
  str_replace_all("^gf_",    "FG_")

importancia_obj <- varImp(mod_misto)

df_importancia <- importancia_obj |>
  tibble::rownames_to_column("Variavel") |>
  dplyr::rename(Importancia = Overall) |>
  dplyr::filter(str_detect(Variavel, "^(VG_|RP_|FG_)"))

top3_VG <- df_importancia |>
  filter(str_detect(Variavel, "^VG_")) |>
  arrange(desc(Importancia)) |>
  slice_head(n = 3) |>
  pull(Variavel)

top3_RP <- df_importancia |>
  filter(str_detect(Variavel, "^RP_")) |>
  arrange(desc(Importancia)) |>
  slice_head(n = 3) |>
  pull(Variavel)

top3_FG <- df_importancia |>
  filter(str_detect(Variavel, "^FG_")) |>
  arrange(desc(Importancia)) |>
  slice_head(n = 3) |>
  pull(Variavel)

vars_9 <- c(top3_VG, top3_RP, top3_FG)

message("Top 3 por fase:")
cat("VG:", top3_VG, "\n")
cat("RP:", top3_RP, "\n")
cat("FG:", top3_FG, "\n")

vars_presentes <- intersect(vars_9, colnames(dados_plot))

df_box <- dados_plot |>
  dplyr::select(all_of(vars_presentes)) |>
  pivot_longer(cols = everything(),
               names_to = "Variavel",
               values_to = "Valor") |>
  dplyr::mutate(
    # Adicionar unidade
    Variavel_Unidade = case_when(
      str_detect(Variavel, "PRECTOTCORR")        ~ paste(Variavel, "(mm)"),
      str_detect(Variavel, "EVPTRNS")            ~ paste(Variavel, "(mm)"),
      str_detect(Variavel, "T2M")                ~ paste(Variavel, "(°C)"),
      str_detect(Variavel, "RH2M")               ~ paste(Variavel, "(%)"),
      str_detect(Variavel, "WS2M")               ~ paste(Variavel, "(m/s)"),
      str_detect(Variavel, "ALLSKY_SFC_PAR_TOT") ~ paste(Variavel, "(MJ/m²)"),
      str_detect(Variavel, "GWETROOT")           ~ paste(Variavel, "(m³/m³)"),
      str_detect(Variavel, "CDD0")               ~ paste(Variavel, "(Graus-dia)"),
      TRUE ~ Variavel
    ),
    # Identificar fase para colorir
    Fase = case_when(
      str_detect(Variavel, "^VG_") ~ "Vegetative Phase",
      str_detect(Variavel, "^RP_") ~ "Reproductive Phase",
      str_detect(Variavel, "^FG_") ~ "Grain Filling"
    ),
    Fase = factor(Fase, levels = c(
      "Vegetative Phase" ,   
      "Reproductive Phase" ,  
      "Grain Filling" 
    ))
  )

# Garantir ordem: VG (linha 1), RP (linha 2), FG (linha 3)
ordem_vars <- c(
  paste(top3_VG, sapply(top3_VG, function(v) {
    case_when(
      str_detect(v, "PRECTOTCORR")        ~ "(mm)",
      str_detect(v, "EVPTRNS")            ~ "(mm)",
      str_detect(v, "T2M")                ~ "(°C)",
      str_detect(v, "RH2M")               ~ "(%)",
      str_detect(v, "WS2M")               ~ "(m/s)",
      str_detect(v, "ALLSKY_SFC_PAR_TOT") ~ "(MJ/m²)",
      str_detect(v, "GWETROOT")           ~ "(m³/m³)",
      str_detect(v, "CDD0")               ~ "(Graus-dia)",
      TRUE ~ ""
    )
  })),
  paste(top3_RP, sapply(top3_RP, function(v) {
    case_when(
      str_detect(v, "PRECTOTCORR")        ~ "(mm)",
      str_detect(v, "EVPTRNS")            ~ "(mm)",
      str_detect(v, "T2M")                ~ "(°C)",
      str_detect(v, "RH2M")               ~ "(%)",
      str_detect(v, "WS2M")               ~ "(m/s)",
      str_detect(v, "ALLSKY_SFC_PAR_TOT") ~ "(MJ/m²)",
      str_detect(v, "GWETROOT")           ~ "(m³/m³)",
      str_detect(v, "CDD0")               ~ "(Graus-dia)",
      TRUE ~ ""
    )
  })),
  paste(top3_FG, sapply(top3_FG, function(v) {
    case_when(
      str_detect(v, "PRECTOTCORR")        ~ "(mm)",
      str_detect(v, "EVPTRNS")            ~ "(mm)",
      str_detect(v, "T2M")                ~ "(°C)",
      str_detect(v, "RH2M")               ~ "(%)",
      str_detect(v, "WS2M")               ~ "(m/s)",
      str_detect(v, "ALLSKY_SFC_PAR_TOT") ~ "(MJ/m²)",
      str_detect(v, "GWETROOT")           ~ "(m³/m³)",
      str_detect(v, "CDD0")               ~ "(Graus-dia)",
      TRUE ~ ""
    )
  }))
)

df_box$Variavel_Unidade <- factor(df_box$Variavel_Unidade,
                                  levels = unique(df_box$Variavel_Unidade))


cores_fase <- c(
  "Vegetative Phase"     = "#01665e",
  "Reproductive Phase"    = "#8da0cb",
  "Grain Filling" = "#c7eae5"
)

grafico_violino_9 <- ggplot(df_box,
                            aes(x = "", y = Valor, fill = Fase)) +
  geom_jitter(color = "gray40", alpha = 0.5,
              width = 0.15, size = 1.5) +
  geom_violin(alpha = 0.7, trim = FALSE,
              color = "black", linewidth = 0.6) +
  geom_boxplot(width = 0.1, fill = "white", color = "black",
               outlier.shape = NA, alpha = 0.8, linewidth = 0.6) +
  facet_wrap(~ Variavel_Unidade, scales = "free_y",
             ncol = 3) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  scale_fill_manual(values = cores_fase) +
  labs(
    title  = "",
    subtitle = "",
    x      = NULL,
    y      = "Observed Values",
    fill   = "Phenological Stage"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position    = "bottom",
    strip.background   = element_rect(fill = "gray30"),
    strip.text         = element_text(face = "bold", color = "white", size = 10),
    axis.text.x        = element_blank(),
    axis.ticks.x       = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(size = 11, color = "gray40")
  )

print(grafico_violino_9)

if (!dir.exists("figuras")) dir.create("figuras")

ggsave("figuras/VIOLIN_PLOT_9GRAFICOS.png",
       grafico_violino_9,
       width = 14, height = 12,
       dpi = 300)

cat("Violin plot salvo em figuras/VIOLIN_PLOT_9GRAFICOS.png\n")