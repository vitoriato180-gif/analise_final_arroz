library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)


#  EXTRAÇÃO DOS BLUPS DO MODELO
blups_todos <- as.data.frame(ranef(mod_misto, postVar = FALSE))

# 1. RANQUEAMENTO GERAL (Apenas BLUP do Genótipo)
ic_gen <- blups_todos %>%
  filter(grpvar == "GEN") %>%
  select(GEN = grp, BLUP = condval) %>%
  mutate(
    Desempenho = ifelse(BLUP >= 0, "Acima da Média", "Abaixo da Média"),
    Label = sprintf("%+.1f", BLUP) # O número 1 indica 1 casa decimal.0 se quiser inteiro.
  ) %>%
  arrange(desc(BLUP))

grafico_geral <- ggplot(ic_gen, aes(x = reorder(GEN, BLUP), y = BLUP, fill = Desempenho)) +
  geom_col(width = 0.7, color = "black", linewidth = 0.2) + 
  geom_text(aes(label = Label, hjust = ifelse(BLUP >= 0, -0.2, 1.2)), 
            size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("Acima da Média" = "#4CAF50", "Abaixo da Média" = "#F44336")) +
  scale_y_continuous(limits = c(min(ic_gen$BLUP) * 1.2, max(ic_gen$BLUP) * 1.2)) +
  coord_flip() +
  labs(
    title = "",
    subtitle = "",
    x = "Genotype",
    y = "BLUP"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none", 
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12),
    axis.text.y = element_text(face = "bold", color = "black"),
    panel.grid.major.y = element_blank()
  ) +
  geom_hline(yintercept = 0, linewidth = 1)

print(grafico_geral)

if(!dir.exists("figuras")) {
  dir.create("figuras")
  message("Pasta 'figuras' criada com sucesso!")
}
ggsave("figuras/RANQUEAMENTO_GERAL_BLUP_PURO.png", grafico_geral, width = 10, height = 8, dpi = 300)