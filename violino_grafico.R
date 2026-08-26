# grafico de violino 

library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(caret)

varImp.merMod <- function(object, ...) {
  summ <- summary(object)$coefficients
  out  <- data.frame(Overall = abs(summ[, "t value"]))
  rownames(out) <- rownames(summ)
  return(out)
}

dados_plot <- final_data_original
names(dados_plot) <- names(dados_plot) |>
  str_replace_all("^veg_", "VG_") |>
  str_replace_all("^repro_", "RP_") |>
  str_replace_all("^gf_", "FG_")

importancia_obj <- varImp(mod_misto)

vars_interesse <- importancia_obj |>
  tibble::rownames_to_column("Variavel") |>
  dplyr::rename(Importancia = Overall) |>
  dplyr::filter(str_detect(Variavel, "^(VG_|RP_|FG_)")) |>
  dplyr::arrange(desc(Importancia)) |>
  dplyr::slice_head(n = 6) |>
  dplyr::pull(Variavel)

message("Top 6 variáveis:")
print(vars_interesse)

vars_presentes <- intersect(vars_interesse, colnames(dados_plot))

df_box <- dados_plot |>
  dplyr::select(all_of(vars_presentes)) |>
  pivot_longer(cols = everything(), names_to = "Variavel", values_to = "Valor") |>
  dplyr::mutate(
    Variavel_Unidade = case_when(
      str_detect(Variavel, "PRECTOTCORR")       ~ paste(Variavel, "(mm)"),
      str_detect(Variavel, "EVPTRNS")           ~ paste(Variavel, "(mm)"),
      str_detect(Variavel, "T2M")               ~ paste(Variavel, "(°C)"),
      str_detect(Variavel, "RH2M")              ~ paste(Variavel, "(%)"),
      str_detect(Variavel, "WS2M")              ~ paste(Variavel, "(m/s)"),
      str_detect(Variavel, "ALLSKY_SFC_PAR_TOT") ~ paste(Variavel, "(MJ/m²)"),
      str_detect(Variavel, "CDD0")              ~ paste(Variavel, "(Graus-dia)"),
      TRUE ~ Variavel
    )
  )

grafico_violino <- ggplot(df_box, aes(x = "", y = Valor, fill = Variavel_Unidade)) +
  geom_jitter(color = "gray40", alpha = 0.5, width = 0.15, size = 1.5) +
  geom_violin(alpha = 0.7, trim = FALSE, color = "black", linewidth = 0.6) +
  geom_boxplot(width = 0.1, fill = "white", color = "black",
               outlier.shape = NA, alpha = 0.8, linewidth = 0.6) +
  facet_wrap(~ Variavel_Unidade, scales = "free_y", ncol = 3) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  scale_fill_viridis_d(option = "mako", direction = -1) +
  labs(x = NULL, y = "Observed Values") +
  theme_bw(base_size = 14) +
  theme(
    legend.position    = "none",
    strip.background   = element_rect(fill = "gray30"),
    strip.text         = element_text(face = "bold", color = "white", size = 11),
    axis.text.x        = element_blank(),
    axis.ticks.x       = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(grafico_violino)

ggsave("figuras/VIOLIN_PLOT_CLIMA.png", grafico_violino, width = 12, height = 7, dpi = 300)
cat("Violin plot salvo em figuras/VIOLIN_PLOT_CLIMA.png\n")