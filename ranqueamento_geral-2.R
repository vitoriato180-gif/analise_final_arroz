library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)

# ==============================================================================
# EXTRAÇÃO DOS BLUPs COM INTERVALO DE CONFIANÇA (95%)
# ==============================================================================

# postVar = TRUE extrai a variância condicional de cada BLUP
blups_todos <- as.data.frame(ranef(mod_misto, postVar = TRUE))

# Filtrar apenas efeito de genótipo
ic_gen <- blups_todos %>%
  filter(grpvar == "GEN") %>%
  select(GEN = grp, BLUP = condval, SE = condsd) %>%
  mutate(
    # Intervalo de confiança 95%
    IC_low  = BLUP - 1.96 * SE,
    IC_high = BLUP + 1.96 * SE,
    Desempenho = ifelse(BLUP >= 0, "Above Average", "Below Average"),
    Label = sprintf("%+.1f", BLUP)
  ) %>%
  arrange(desc(BLUP))

# ==============================================================================
# GRÁFICO COM INTERVALO DE CONFIANÇA
# ==============================================================================

grafico_geral <- ggplot(ic_gen,
                        aes(x = reorder(GEN, BLUP),
                            y = BLUP,
                            fill = Desempenho)) +
  
  # Barras
  geom_col(width = 0.6, color = "black", linewidth = 0.2) +
  
  # Intervalo de confiança
  geom_errorbar(aes(ymin = IC_low, ymax = IC_high),
                width = 0.25,
                linewidth = 0.7,
                color = "black") +
  
  scale_fill_manual(values = c(
    "Above Average" = "#4CAF50",
    "Below Average" = "#F44336"
  )) +
  
  geom_text(aes(label = Label,
                y = ifelse(BLUP >= 0, IC_high + 20, IC_low - 20),
                hjust = ifelse(BLUP >= 0, 0, 1)),
            size = 3.5, fontface = "bold") +
  
  scale_y_continuous(
    limits = c(min(ic_gen$IC_low) * 1.15,
               max(ic_gen$IC_high) * 1.15)
  ) +
  
  coord_flip() +
  
  geom_hline(yintercept = 0, linewidth = 1) +
  
  labs(
    title    = "",
    subtitle = "",
    x        = "Genotype",
    y        = "BLUP (kg/ha)"
  ) +
  
  theme_minimal() +
  theme(
    legend.position    = "none",
    plot.title         = element_text(face = "bold", size = 16),
    plot.subtitle      = element_text(color = "gray40", size = 12),
    axis.text.y        = element_text(face = "bold", color = "black"),
    panel.grid.major.y = element_blank()
  )

print(grafico_geral)

if (!dir.exists("figuras")) {
  dir.create("figuras")
  message("Pasta 'figuras' criada com sucesso!")
}

ggsave("figuras/RANQUEAMENTO_GERAL_BLUP_IC.png",
       grafico_geral,
       width = 10, height = 8, dpi = 300)

cat("\nGráfico salvo em figuras/RANQUEAMENTO_GERAL_BLUP_IC.png\n")

# Mostrar tabela com BLUPs e ICs
cat("\nTabela de BLUPs com Intervalos de Confiança (95%):\n")
print(ic_gen %>% select(GEN, BLUP, SE, IC_low, IC_high, Desempenho),
      row.names = FALSE)
