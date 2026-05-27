library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)

# Extrai os BLUPs do modelo
blups_todos <- as.data.frame(ranef(mod_misto, postVar = FALSE))

# Preparando a tabela para o Heatmap
ic_genloc <- blups_todos %>%
  filter(grpvar == "GEN:LOC") %>%
  separate(grp, into = c("GEN", "LOC"), sep = ":") %>%
  select(GEN, LOC, BLUP_int = condval) %>%
  left_join(
    blups_todos %>% filter(grpvar == "GEN") %>% select(GEN = grp, BLUP_gen = condval),
    by = "GEN"
  ) %>%
  mutate(
    # BLUP Total no ambiente = BLUP Geral do Genótipo + BLUP da Interação
    BLUP_Total = BLUP_gen + BLUP_int
  ) %>%
  ungroup() %>%
  complete(GEN, LOC) %>% # Preenche com NA se algum genótipo não estiver em algum local
  # O PULO DO GATO: Separar a coluna LOC de volta em Cidade e Estado (ST)
  separate(LOC, into = c("Cidade", "ST"), sep = "_", remove = FALSE)

# Gerando o Heatmap
hm_blup <- ggplot(ic_genloc, aes(x = Cidade, y = GEN)) +
  
  # 1. Heatmap principal
  geom_tile(aes(fill = BLUP_Total), color = "gray90", linewidth = 0.5) + 
  
  scale_fill_gradient2(
    low = "#F44336",        
    mid = "gray85",         
    high = "#4CAF50",       
    midpoint = 0,            
    na.value = "white",     
    name = "BLUP\n(kg/ha)"
  ) +
  
  geom_tile(data = subset(ic_genloc, is.na(BLUP_Total)), 
            aes(color = "missing data"), fill = "white", linewidth = 0.5) +
  
  scale_color_manual(
    name = "", 
    values = c("missing data" = "gray60"), 
    guide = guide_legend(override.aes = list(fill = "white", linewidth = 0.5))
  ) +
  
  # 2. O COMANDO MÁGICO: Cria os blocos por Estado no topo
  facet_grid(~ ST, scales = "free_x", space = "free_x") +
  
  labs(
    title = "",
    subtitle = "",
    x = "Municipalities",
    y = "Genotype"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    # Posição da legenda (mudei para o topo para ficar idêntico à imagem de referência)
    legend.position = "top", 
    legend.direction = "horizontal",
    
    # Textos dos eixos (Cidades a 90 graus para caberem lado a lado sem sobrepor)
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold", size = 10),
    axis.text.y = element_text(face = "bold", size = 11),
    
    # Estética dos Estados no eixo superior (as caixinhas brancas com texto)
    strip.text = element_text(face = "bold", size = 12, color = "black"),
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.8),
    
    # Bordas pretas ao redor de cada bloco de Estado (igual à imagem)
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.spacing = unit(0.1, "lines"), # Deixa os estados bem grudadinhos
    
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "gray40", size = 11),
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold", size = 11)
  )

print(hm_blup)
if(!dir.exists("figuras")) {
  dir.create("figuras")
  message("Pasta 'figuras' criada com sucesso!")
}
ggsave("figuras/HEATMAP_BLUP_ESTADOS.png", hm_blup, width = 12, height = 8, dpi = 300)