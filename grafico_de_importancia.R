library(caret)
library(dplyr)
library(ggplot2)
library(stringr)

varImp.merMod <- function(object, ...) {
  summ <- summary(object)$coefficients
  out <- data.frame(Overall = abs(summ[, "t value"]))
  rownames(out) <- rownames(summ)
  return(out)
}

message("\nGerando Gráficos de Importância Relativa...")
importancia_obj <- varImp(mod_misto)

df_raw <- importancia_obj %>%
  tibble::rownames_to_column("Variavel") %>%
  rename(Score = Overall) %>%
  filter(Variavel != "(Intercept)") # Remove Intercepto da conta 100%

df_detalhado <- df_raw %>%
  mutate(
    Grupo = case_when(
      str_detect(Variavel, "^Simb_dom") ~ "Soil",
      str_detect(Variavel, "^VG_") ~ "Climate: Vegetative",
      str_detect(Variavel, "^RP_") ~ "Climate: Reproductive",
      str_detect(Variavel, "^FG_") ~ "Climate: Filling Grain",
      TRUE ~ "Others" 
    )
  ) %>%
  # Fator para garantir a ordem biológica cronológica nos gráficos
  mutate(
    Grupo = factor(Grupo, levels = c("Soil", "Climate: Vegetative", "Climate: Reproductive", "Climate: Filling Grain", "Others"))
  )

soma_absoluta_tudo <- sum(df_detalhado$Score)

df_calculado <- df_detalhado %>%
  mutate(
    Porcentagem_Natural = (Score / soma_absoluta_tudo) * 100,
    Label = sprintf("%.1f%%", Porcentagem_Natural)
  )

cores_oficiais <- c(
  "Soil" = "#8c510a",                   # Marrom escuro (Terra)
  "Climate: Vegetative" = "#01665e",    # Verde escuro
  "Climate: Reproductive" = "#8da0cb",  # Verde médio
  "Climate: Filling Grain" = "#c7eae5"   , # Verde água clarinho
  "Others" = "#d9d9d9"                  # Cinza neutro
)

# --- GRÁFICO 1: VISÃO MACRO ---
resumo_global <- df_calculado %>%
  group_by(Grupo) %>%
  summarise(Score_Total = sum(Score), .groups = 'drop') %>%
  mutate(
    Porcentagem = (Score_Total / sum(Score_Total)) * 100,
    Label = sprintf("%.1f%%", Porcentagem) 
  )

p_global <- ggplot(resumo_global, aes(x = Porcentagem, y = reorder(Grupo, desc(Grupo)), fill = Grupo)) +
  geom_col(alpha = 0.9, width = 0.6, color = "black") +
  geom_text(aes(label = Label), hjust = -0.2, fontface = "bold", size = 4.5, color = "black") +
  scale_fill_manual(values = cores_oficiais) +
  scale_x_continuous(limits = c(0, max(resumo_global$Porcentagem) * 1.15)) +
  labs(title = "Relative Efficiency ", x = "Relative Importance (%)", y = NULL) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 16),
        panel.grid.major.y = element_blank(), axis.text.y = element_text(face = "bold", color = "black"))

if(!dir.exists("figuras")) {
  dir.create("figuras")
  message("Pasta 'figuras' criada com sucesso!")
}
ggsave("figuras/IMPORTANCIA_MACRO.png", p_global, width = 8, height = 5, dpi = 300)


library(caret)
library(dplyr)
library(ggplot2)
library(stringr)

# 1. FUNÇÃO DO varImp PARA O MODELO MISTO
varImp.merMod <- function(object, ...) {
  summ <- summary(object)$coefficients
  out <- data.frame(Overall = abs(summ[, "t value"]))
  rownames(out) <- rownames(summ)
  return(out)
}

# 2. EXTRAÇÃO E FILTRO (APENAS CLIMA)
importancia_obj <- varImp(mod_misto)

df_raw <- importancia_obj %>%
  tibble::rownames_to_column("Variavel") %>%
  rename(Score = Overall) %>%
  # O PULO DO GATO: Filtra para manter APENAS o que é clima logo de cara
  filter(str_detect(Variavel, "^VG_|^RP_|^FG_"))

# 3. CLASSIFICAÇÃO DOS GRUPOS
df_detalhado <- df_raw %>%
  mutate(
    Grupo = case_when(
      str_detect(Variavel, "^VG_") ~ "Climate: Vegetative",
      str_detect(Variavel, "^RP_") ~ "Climate: Reproductive",
      str_detect(Variavel, "^FG_") ~ "Climate: Filling Grain"
    )
  ) %>%
  # Fator para garantir a ordem biológica cronológica nos gráficos
  mutate(
    Grupo = factor(Grupo, levels = c("Climate: Vegetative", "Climate: Reproductive", "Climate: Filling Grain"))
  )

# ==============================================================================
# 4. A CONTA (Agora os 100% são distribuídos apenas entre o clima)
# ==============================================================================
soma_absoluta_tudo <- sum(df_detalhado$Score)

df_calculado <- df_detalhado %>%
  mutate(
    Porcentagem_Natural = (Score / soma_absoluta_tudo) * 100,
    Label = sprintf("%.1f%%", Porcentagem_Natural)
  )

# Conferência no Console
conferencia <- df_calculado %>%
  group_by(Grupo) %>%
  summarise(Soma_da_Fase = sum(Porcentagem_Natural), .groups = 'drop')

print(conferencia)
message("Soma de tudo no modelo (Apenas Clima): ", round(sum(conferencia$Soma_da_Fase), 1), "%")

# ==============================================================================
# 5. GERANDO O GRÁFICO (Com facet_grid)
# ==============================================================================
# Cores corrigidas (Sem os espaços extras no início do nome)
cores_fases <- c(
  "Climate: Vegetative" = "#01665e",    # Verde escuro
  "Climate: Reproductive" = "#8da0cb",  # Verde médio
  "Climate: Filling Grain" = "#d9d9d9"     
)

grafico_natural <- ggplot(df_calculado, aes(x = Porcentagem_Natural, 
                                            y = reorder(Variavel, Porcentagem_Natural), 
                                            fill = Grupo)) +
  geom_col(alpha = 0.9, width = 0.7, color = "black") + # Adicionei borda preta para destacar o lightyellow
  geom_text(aes(label = Label), hjust = -0.2, size = 3.5, fontface = "bold", color = "black") +
  
  scale_fill_manual(values = cores_fases) +
  
  facet_grid(Grupo ~ ., scales = "free_y", space = "free_y") +
  
  scale_x_continuous(limits = c(0, max(df_calculado$Porcentagem_Natural) * 1.25)) +
  
  labs(
    title = "Relative Importance",
    subtitle = "",
    x = "Relative Importance (%)",
    y = NULL
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 11),
    strip.text = element_text(face = "bold", size = 12, color = "white"),
    strip.background = element_rect(fill = "gray30"),
    panel.grid.major.y = element_blank()
  )

print(grafico_natural)

# Salva o gráfico
if(!dir.exists("figuras")) {
  dir.create("figuras")
  message("Pasta 'figuras' criada com sucesso!")
}
ggsave("figuras/IMPORTANCIA_VARIAVEIS_DETALHADO_CLIMA.png", grafico_natural, width = 10, height = 10, dpi = 300)
