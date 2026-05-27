rm(list = ls())
gc()

set.seed(2025)

packages <- c("dplyr", "readxl", "ggplot2", "sf", "geobr")

for(pkg in packages){
  if(!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

final_data <- read_excel(file.choose())

# Contando o número de experimentos por Local/Coordenada
dados_mapa <- final_data %>%
  filter(!is.na(LATITUDE) & !is.na(LONGITUDE)) %>%
  group_by(LOCATION, LATITUDE, LONGITUDE) %>%
  summarise(n_experimentos = n(), .groups = "drop")

# Transformando os dados para o formato espacial (sf)
dados_sf <- st_as_sf(dados_mapa, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)


# Carregar shapefile dos estados (geobr)
# (Aviso: Pode demorar alguns segundos para baixar os dados na primeira vez)
estados <- read_state(code_state = "all", year = 2020)

# O PEDIDO DO ALEXANDRE: Tirar Sul e Sudeste do mapa
# Mantemos "Sul" e "Sudeste" em português aqui porque é como está na base do geobr
estados_filtrados <- estados %>%
  filter(!(name_region %in% c("Sul", "Sudeste")))

# Criando o mapa
mapa_ensaios <- ggplot() +
  
  # Camada 1: Mapa do Brasil APENAS com Norte, Nordeste e Centro-Oeste
  geom_sf(data = estados_filtrados, aes(fill = name_region), color = "black", linewidth = 0.3) +
  
  scale_fill_manual(
    name = "Region", # Título da legenda da região
    values = c(
      "Norte"        = "#D4EDD4",
      "Nordeste"     = "#FFFFE0",
      "Centro Oeste" = "#E6D8AD"  
    ),
    labels = c(
      "Norte"        = "North",
      "Nordeste"     = "Northeast",
      "Centro Oeste" = "Midwest" # Midwest é o termo inglês mais elegante para Centro-Oeste
    )
  ) +
  
  # Camada 2: Bolhas proporcionais ao número de experimentos
  geom_sf(data = dados_sf, aes(size = n_experimentos), color = "#00008B", alpha = 0.6) +
  
  # Ajustando a escala das bolhas e traduzindo o título
  scale_size_continuous(range = c(3, 10), name = "Number of Experiments") +
  
  theme_minimal() +
  labs(
    title = "Distribution of Rice Experiments",
    subtitle = ""
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

print(mapa_ensaios)

# Opcional: Código para salvar o mapa em alta resolução
if(!dir.exists("figuras")) {
  dir.create("figuras")
  message("Pasta 'figuras' criada com sucesso!")
}
ggsave("figuras/MAPA_ENSAIOS_BRASIL_ENGLISH.png", mapa_ensaios, width = 8, height = 8, dpi = 300)
