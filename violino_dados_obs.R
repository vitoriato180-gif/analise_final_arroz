# ============================================================
# SCRIPT: VIOLIN PLOT DAS TOP 6 VARIÁVEIS COM UNIDADES
# ============================================================
rm(list=ls()) 
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(readxl)
library(caret)
library(lme4)
library(lmerTest)

# ============================================================
# 1. CARREGAR E PADRONIZAR DADOS
# ============================================================
message("Selecione o arquivo '.xlsx' com os dados originais")
final_data <- read_excel(file.choose())
dados_plot <- final_data

# O PULO DO GATO: Forçando a renomeação imediata no banco de dados
names(dados_plot) <- names(dados_plot) %>%
  str_replace_all("^veg_", "VG_") %>%
  str_replace_all("^repro_", "RP_") %>%
  str_replace_all("^gf_", "FG_")

message("Nomes padronizados com sucesso!")

message("Selecione o modelo '.rds' (ou ignore caso já esteja carregado)")
mod_misto <- readRDS(file.choose())

# ============================================================
# 2. EXTRAIR IMPORTÂNCIA (varImp)
# ============================================================
varImp.merMod <- function(object, ...) {
  summ <- summary(object)$coefficients
  out  <- data.frame(Overall = abs(summ[, "t value"]))
  rownames(out) <- rownames(summ)
  return(out)
}

importancia_obj <- varImp(mod_misto)

# Pega as 6 variáveis climáticas mais importantes
vars_interesse <- importancia_obj %>%
  tibble::rownames_to_column("Variavel") %>%
  rename(Importancia = Overall) %>%
  filter(str_detect(Variavel, "^(VG_|RP_|FG_)")) %>%
  arrange(desc(Importancia)) %>%
  slice_head(n = 6) %>%
  pull(Variavel)

message("\nTop 6 variáveis selecionadas pelo varImp:")
print(vars_interesse)

# ============================================================
# 3. PREPARAR DADOS E INSERIR UNIDADES AGRONÔMICAS
# ============================================================
vars_presentes <- intersect(vars_interesse, colnames(dados_plot))

if(length(vars_presentes) == 0) {
  stop("ERRO: Nenhuma variável encontrada em dados_plot. Verifique a nomenclatura.")
}

df_box <- dados_plot %>%
  select(all_of(vars_presentes)) %>%
  pivot_longer(cols = everything(), names_to = "Variavel", values_to = "Valor") %>%
  
  # Identifica o tipo de clima e adiciona a unidade para o gráfico
  mutate(
    Variavel_Unidade = case_when(
      str_detect(Variavel, "PRECTOTCORR") ~ paste(Variavel, "(mm)"),
      str_detect(Variavel, "EVPTRNS") ~ paste(Variavel, "(mm)"),
      str_detect(Variavel, "T2M") ~ paste(Variavel, "(°C)"),
      str_detect(Variavel, "RH2M") ~ paste(Variavel, "(%)"),
      str_detect(Variavel, "WS2M") ~ paste(Variavel, "(m/s)"),
      str_detect(Variavel, "ALLSKY_SFC_PAR_TOT") ~ paste(Variavel, "(MJ/m²)"), 
      str_detect(Variavel, "CDD0") ~ paste(Variavel, "(Graus-dia)"),
      TRUE ~ Variavel 
    )
  )

# ============================================================
# 4. GERAR O GRÁFICO DE VIOLINO + BOXPLOT
# ============================================================
grafico_violino <- ggplot(df_box, aes(x = "", y = Valor, fill = Variavel_Unidade)) +
  
  # 1. Fundo: Os pontos reais dos ensaios (jitter)
  geom_jitter(color = "gray40", alpha = 0.5, width = 0.15, size = 1.5) +
  
  # 2. Meio: O Violino (a silhueta da distribuição climática)
  geom_violin(alpha = 0.7, trim = FALSE, color = "black", linewidth = 0.6) +
  
  # 3. Frente: O Boxplot embutido (Bem estreito e branco para contraste)
  geom_boxplot(width = 0.1, fill = "white", color = "black", 
               outlier.shape = NA, alpha = 0.8, linewidth = 0.6) +
  
  # Painéis separados
  facet_wrap(~ Variavel_Unidade, scales = "free_y", ncol = 3) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) + 
  
  scale_fill_viridis_d(option = "mako", direction = -1) + 
  
  labs(
    title    = "",
    subtitle = "",
    x        = NULL,
    y        = "Observed Values"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position  = "none",
    strip.background = element_rect(fill = "gray30"),
    strip.text       = element_text(face = "bold", color = "white", size = 11),
    axis.text.x      = element_blank(),
    axis.ticks.x     = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(grafico_violino)

if(!dir.exists("figuras")) {
  dir.create("figuras")
  message("Pasta 'figuras' criada com sucesso!")
}
ggsave("figuras/VIOLIN_PLOT_CLIMA.png", grafico_violino, width = 12, height = 7, dpi = 300)
