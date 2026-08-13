rm(list = ls())
load("3_modelo_final.RData") 
set.seed(2025)

#install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(kgc)
library(caret)
library(stringr)

# FILTRAR OS 24 LOCAIS DO MODELO
library(dplyr)

locais_modelo <- final_data_original |>
  dplyr::group_by(TRIAL, GEN) |>
  dplyr::filter(dplyr::n() == 4) |>
  dplyr::ungroup() |>
  dplyr::group_by(TRIAL, GEN) |>
  dplyr::summarise(
    GY       = mean(GY, na.rm = TRUE),
    LOCATION = dplyr::first(LOCATION),
    Simb     = dplyr::first(Simb),
    AD_UM    = dplyr::first(AD_UM),
    .groups  = "drop"
  ) |>
  dplyr::filter(!is.na(GY), GY > 900, !is.na(Simb), !is.na(AD_UM)) |>
  dplyr::pull(LOCATION) |>
  unique()

print(length(locais_modelo))
print(locais_modelo)

dados_mapa <- final_data_original |>
  dplyr::filter(!is.na(LATITUDE) & !is.na(LONGITUDE)) |>
  dplyr::filter(LOCATION %in% locais_modelo) |>
  dplyr::group_by(LOCATION, LATITUDE, LONGITUDE) |>
  dplyr::summarise(n_experimentos = dplyr::n_distinct(TRIAL), .groups = "drop")

print(nrow(dados_mapa))
print(dados_mapa)

dados_sf <- st_as_sf(dados_mapa, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
estados <- ne_states(country = "Brazil", returnclass = "sf")

# Filtrar Sul e Sudeste
sul_sudeste <- c("PR", "SC", "RS", "SP", "RJ", "MG", "ES")

estados_filtrados <- estados %>%
  filter(!(postal %in% sul_sudeste)) %>%
  mutate(regiao = case_when(
    postal %in% c("AM", "RR", "PA", "AC", "AP", "RO", "TO") ~ "North",
    postal %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA") ~ "Northeast",
    postal %in% c("GO", "MT", "MS", "DF")                    ~ "Midwest"
  ))


mapa_ensaios <- ggplot() +
  
  geom_sf(data = estados_filtrados, aes(fill = regiao), 
          color = "black", linewidth = 0.3) +
  
  scale_fill_manual(
    name = "Region",
    values = c(
      "North"     = "#D4EDD4",
      "Northeast" = "#FFFFE0",
      "Midwest"   = "#E6D8AD"
    )
  ) +
  
  geom_sf(data = dados_sf, aes(size = n_experimentos), 
          color = "#00008B", alpha = 0.6) +
  
  scale_size_continuous(
    range = c(3, 10), 
    name  = "Number of Trials",
    breaks = c(1, 4, 8, 12, 16)
  ) +
  
  theme_minimal() +
  labs(title = "Distribution of Rice Experiments") +
  theme(
    plot.title  = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text   = element_blank(),
    axis.ticks  = element_blank(),
    panel.grid  = element_blank()
  )

print(mapa_ensaios)

if(!dir.exists("figuras")) dir.create("figuras")

ggsave("figuras/MAPA_ENSAIOS_BRASIL_ENGLISH.png", 
       mapa_ensaios, width = 8, height = 8, dpi = 300)


print(dados_mapa)
summary(dados_mapa$n_experimentos)

library(dplyr)

# Mostra o que está sendo plotado no mapa
dados_mapa %>%
  select(LOCATION, n_experimentos) %>%
  arrange(desc(n_experimentos)) %>%
  print(n = Inf)


final_data_original %>%
  filter(LOCATION == "SINOP") %>%
  distinct(TRIAL) %>%
  print(n = Inf)


############################################################################################################
# MAPA DE CLASSIFICAÇÃO CLIMÁTICA (Köppen-Geiger)

library(dplyr)
library(ggplot2)
library(sf)
library(kgc)

# CRUZAR OS 24 LOCAIS COM A TABELA DE KÖPPEN
data("climatezones")

dados_koppen <- dados_mapa %>%
  mutate(
    Lat_round = round(LATITUDE * 2) / 2 + 0.25,
    Lon_round = round(LONGITUDE * 2) / 2 + 0.25
  ) %>%
  left_join(
    climatezones,
    by = c("Lat_round" = "Lat", "Lon_round" = "Lon")
  )

cat("NAs na classificação:", sum(is.na(dados_koppen$Cls)), "\n")
print(table(dados_koppen$Cls))

# CONVERTER PARA SF
dados_koppen_sf <- st_as_sf(dados_koppen,
                            coords = c("LONGITUDE", "LATITUDE"),
                            crs = 4326)


cores_koppen <- c(
  "Af" = "#0000FF",
  "Am" = "#0078C8",
  "Aw" = "#96C8FF"
)

mapa_koppen <- ggplot() +
  
  geom_sf(data = estados_filtrados, aes(fill = regiao),
          color = "black", linewidth = 0.3) +
  
  scale_fill_manual(
    name = "Region",
    values = c(
      "North"     = "#D4EDD4",
      "Northeast" = "#FFFFE0",
      "Midwest"   = "#E6D8AD"
    )
  ) +
  
  geom_sf(data = dados_koppen_sf,
          aes(color = Cls, size = n_experimentos),
          alpha = 0.8) +
  
  scale_color_manual(
    name = "Köppen-Geiger\nClassification",
    values = cores_koppen,
    labels = c(
      "Af" = "Af (Tropical Rainforest)",
      "Am" = "Am (Tropical Monsoon)",
      "Aw" = "Aw (Tropical Savanna)"
    )
  ) +
  
  scale_size_continuous(
    range = c(3, 10),
    name = "Number of Trials",
    breaks = c(1, 4, 8, 12, 16)
  ) +
  
  guides(
    size = guide_legend(override.aes = list(color = "#0078C8"))
  ) +
  
  theme_minimal() +
  labs(title = "Köppen-Geiger Climate Classification") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

print(mapa_koppen)

ggsave("figuras/MAPA_KOPPEN.png", mapa_koppen,
       width = 8, height = 8, dpi = 300)


########################################################################################################
# MAPA DO SOLO

library(dplyr)
library(ggplot2)
library(sf)

# PREPARAR DADOS DE SOLO COM COORDENADAS
dados_solo <- final_data_original %>%
  filter(LOCATION %in% locais_modelo) %>%
  filter(!is.na(LATITUDE) & !is.na(LONGITUDE)) %>%
  distinct(LOCATION, LATITUDE, LONGITUDE, Simb_dom)

# Converter para sf
dados_solo_sf <- st_as_sf(dados_solo,
                          coords = c("LONGITUDE", "LATITUDE"),
                          crs = 4326)


cores_solo <- c(
  "Latossolo"          = "#C8A951",
  "Neossolo"           = "#C87941",
  "Argissolo_e_Outros" = "#7B4F2E"
)


mapa_solo <- ggplot() +
  
  geom_sf(data = estados_filtrados, aes(fill = regiao),
          color = "black", linewidth = 0.3) +
  
  scale_fill_manual(
    name = "Region",
    values = c(
      "North"     = "#D4EDD4",
      "Northeast" = "#FFFFE0",
      "Midwest"   = "#E6D8AD"
    )
  ) +
  
  geom_sf(data = dados_solo_sf,
          aes(color = Simb_dom, size = Simb_dom),
          alpha = 0.9) +
  
  scale_color_manual(
    name = "Soil Classification",
    values = cores_solo,
    labels = c(
      "Latossolo"          = "Latossolo",
      "Neossolo"           = "Neossolo",
      "Argissolo_e_Outros" = "Argissolo and Others"
    )
  ) +
  
  scale_size_manual(
    name = "Soil Classification",
    values = c(
      "Latossolo"          = 6,
      "Neossolo"           = 6,
      "Argissolo_e_Outros" = 6
    ),
    labels = c(
      "Latossolo"          = "Latossolo",
      "Neossolo"           = "Neossolo",
      "Argissolo_e_Outros" = "Argissolo and Others"
    )
  ) +
  
  theme_minimal() +
  labs(title = "Soil Classification") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

print(mapa_solo)

if(!dir.exists("figuras")) dir.create("figuras")
ggsave("figuras/MAPA_SOLO.png", mapa_solo,
       width = 8, height = 8, dpi = 300)

########################################################################################################
# grafico de box plot

library(dplyr)
library(ggplot2)

# 1. PREPARAR DADOS
dados_box <- final_data_original %>%
  filter(LOCATION %in% locais_modelo) %>%
  filter(!is.na(GY)) %>%
  mutate(ST = factor(ST, levels = c("GO", "MA", "MT", "PA", "PI", "RO", "TO")))

# 2. BOXPLOT
boxplot_prod <- ggplot(dados_box, aes(x = LOC, y = GY, fill = ST)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5, alpha = 0.8) +
  facet_grid(~ ST, scales = "free_x", space = "free_x") +
  scale_fill_brewer(palette = "Set2", name = "State") +
  labs(
    title = "",
    x     = "Location",
    y     = "Grain Yield (kg/ha)"
  ) +
  theme_bw() +
  theme(
    plot.title   = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 9),
    strip.text   = element_text(face = "bold", size = 11),
    legend.position = "none"
  )

print(boxplot_prod)

# 3. SALVAR
ggsave("figuras/BOXPLOT_PRODUTIVIDADE.png", boxplot_prod,
       width = 14, height = 6, dpi = 300)

cat("Boxplot salvo em figuras/BOXPLOT_PRODUTIVIDADE.png\n")

#############################################################################################################
# grafico de importancia geral

library(caret)
library(dplyr)
library(ggplot2)
#install.packages("stringr")
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
      str_detect(Variavel, "^VG_") ~ "Climate (Vegetative Fase)",
      str_detect(Variavel, "^RP_") ~ "Climate (Reproductive Fase)",
      str_detect(Variavel, "^FG_") ~ "Climate (Filling Grain Fase)",
      TRUE ~ "Soil water available" 
    )
  ) %>%
  # Fator para garantir a ordem biológica cronológica nos gráficos
  mutate(
    Grupo = factor(Grupo, levels = c(
      "Soil", 
      "Climate (Vegetative Fase)", 
      "Climate (Reproductive Fase)", 
      "Climate (Filling Grain Fase)", 
      "Soil water available"
    ))
  )
soma_absoluta_tudo <- sum(df_detalhado$Score)

df_calculado <- df_detalhado %>%
  mutate(
    Porcentagem_Natural = (Score / soma_absoluta_tudo) * 100,
    Label = sprintf("%.1f%%", Porcentagem_Natural)
  )

cores_oficiais <- c(
  "Soil" = "#8c510a",                   # Marrom escuro (Terra)
  "Climate (Vegetative Fase)" = "#01665e",    # Verde escuro
  "Climate (Reproductive Fase)" = "#8da0cb",  # Verde médio
  "Climate (Filling Grain Fase)" = "#c7eae5"   , # Verde água clarinho
  "Soil water available" = "#d9d9d9"                  # Cinza neutro
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

#############################################################################################################
# grafico detalhado de importancia 

library(caret)
library(dplyr)
library(ggplot2)
library(stringr)

# FUNÇÃO DO varImp PARA O MODELO MISTO
varImp.merMod <- function(object, ...) {
  summ <- summary(object)$coefficients
  out <- data.frame(Overall = abs(summ[, "t value"]))
  rownames(out) <- rownames(summ)
  return(out)
}

# EXTRAÇÃO E FILTRO (APENAS CLIMA)
importancia_obj <- varImp(mod_misto)

df_raw <- importancia_obj %>%
  tibble::rownames_to_column("Variavel") %>%
  rename(Score = Overall) %>%
  # O PULO DO GATO: Filtra para manter APENAS o que é clima logo de cara
  filter(str_detect(Variavel, "^VG_|^RP_|^FG_"))

# CLASSIFICAÇÃO DOS GRUPOS
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

if(!dir.exists("figuras")) {
  dir.create("figuras")
  message("Pasta 'figuras' criada com sucesso!")
}
ggsave("figuras/IMPORTANCIA_VARIAVEIS_DETALHADO_CLIMA.png", grafico_natural, width = 10, height = 10, dpi = 300)
