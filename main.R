# Lista de pacotes
packages <- c("shiny", "svDialogs", "readr", "writexl", "readxl", "tidyr", 
              "tidyverse", "ggplot2", "ggpubr", "scales", "dplyr", "broom", 
              "matrixStats", "purrr", "stringr", "plotly", "ggthemes", "flux")

# Instalar pacotes que ainda não estão instalados
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(shiny)
library(svDialogs)
library(readr)
library(writexl)
library(readxl)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(scales)
library(dplyr)
library(broom)
library(matrixStats)
library(purrr)
library(stringr)
library(plotly)
library(ggthemes)
library(flux)

selecionar_diretorio <- function() {
  diretorio <- setwd(dlg_dir(default = getwd())$res)
  return(diretorio)
}

transformar_arquivos_em_dataframe <- function(lista_arquivos) {  # nolint
  all_excel_files <- diretorio %>%
    list.files(pattern = '\\.xls', full.names = TRUE)   %>% #Não troque por lista_arquivos, por algum motivo não funciona #nolint
    set_names(lista_arquivos) %>% 
    map_dfr(read_excel, sheet = 2, .id = "filename") %>% 
    mutate(filename = basename(filename)) %>% 
    pivot_wider(names_from = filename, values_from = L01S03R01)
  #Conta a partir do tempo 0.9
  delayed_lum_data <- filter(all_excel_files, Time > 0.9)
  return(delayed_lum_data)
}

converter_wider_para_longer <- function(wider) {
  longer <- wider %>%
    pivot_longer(cols = names(wider)[-1],
                 names_to = "Amostra",
                 values_to = "Photons")  %>%
    mutate(Concentracao = paste0(substr(Amostra, 1, 1), "g/L"))
  return(longer)
}

pegar_carac_amostras <- function(lista_arquivos) {
  db_carac_amostras <- data.frame(concentracao = as.numeric(str_extract(basename(lista_arquivos), "\\d+"))) #nolint
  db_carac_amostras <- aggregate(replicatas ~ concentracao, #nolint
                          data = transform(db_carac_amostras, replicatas = 1), sum)#nolint
  return(db_carac_amostras)
}

criar_db_conc_replicatas <- function(db_fotons_wider) {
  nomes_arquivos <- names(db_fotons_wider)[-1]
  concentracao <- as.numeric(str_extract(basename(nomes_arquivos), "\\d+"))
  replicata <- (as.numeric(
    str_extract(
      basename(nomes_arquivos), "\\d+\\.\\d+"))-concentracao)*10
  
  db_conc_replicatas <- data.frame(
    Concentracao = concentracao,
    Replicata = replicata
  )
  return(db_conc_replicatas)
}

calcular_area_sob_curvas <- function(db_wider, inter_tempo){
  num_de_colunas <- ncol(db_wider) 
  tempo_invervalo = db_wider$Time[inter_tempo]
  areas_sob_curva = c()
  
  for(i in 2:num_de_colunas){
    if(inherits(db_wider[inter_tempo, i], c("numeric"))){
      fotons = db_wider[inter_tempo, i]
    }else{
    fotons = pull(db_wider[inter_tempo, i])
    }
    area_sob_curva = auc(tempo_invervalo, fotons)
    
    # Cria um vetor com todos os valores de areas_sob_curva
    areas_sob_curva <- c(areas_sob_curva, area_sob_curva)
  }
  return(areas_sob_curva)
}

gerar_db_areas <- function(db_wider, inter_tempo){
  
  db_areas_sob_curvas <- criar_db_conc_replicatas(db_wider)
  db_areas_sob_curvas$Area = calcular_area_sob_curvas(db_wider, inter_tempo)
  
  return(db_areas_sob_curvas)
}

calcular_media_replicatas<- function(db_fotons_wider, concentracao){
  amostras <-  select(db_fotons_wider, starts_with(concentracao))
  media <- data.frame(
    Time = db_fotons_wider$Time,
    Media = rowMeans(amostras)
    )
  return(media)
}

calcular_idls <- function(inter_tempo){
  
  media_replicatas_controle <- calcular_media_replicatas(db_fotons_wider, "0")
  plot(media_replicatas_controle)
  media_valores_controle <- mean(media_replicatas_controle$Media)
  db_media_valores <- criar_db_conc_replicatas(db_fotons_wider)
  db_media_valores$Valores_Media <- 0
  db_media_valores$Valores_IDL <- 0
  
  for(col in 1:ncol(db_fotons_wider[-1])){
    db_media_valores[col,3] <- mean(unlist(db_fotons_wider[-1][col]))
    media_amostra <- db_media_valores[col,3]
    db_media_valores[col,4] <- 
      100*((media_valores_controle - media_amostra)/media_valores_controle)
  }
  
  return(db_media_valores$Valores_IDL)
}

gerar_db_idls <- function(db_fotons_wider){
  db_idl <- criar_db_conc_replicatas(db_fotons_wider)
  idl = calcular_idls(inter_tempo)
  db_idl$idl = idl
  
  return(db_idl)
}


diretorio <- selecionar_diretorio()
lista_arquivos <- list.files(pattern = '\\.xls', full.names = TRUE)

db_carac_amostras <- pegar_carac_amostras(lista_arquivos) # nolint
db_fotons_wider <- transformar_arquivos_em_dataframe(lista_arquivos) # nolint
db_fotons_longer <- converter_wider_para_longer(db_fotons_wider) #nolint

inicio_tempo <- which(db_fotons_wider$Time == 1.1)
fim_tempo <- which(db_fotons_wider$Time == 59.9)

inter_tempo <- c(inicio_tempo:fim_tempo)
db_areas_sob_curvas <- gerar_db_areas(db_fotons_wider, inter_tempo)

ggplot(data = db_fotons_longer, aes(x = Time, y = Photons, group = Amostra, color = Concentracao)) +
  labs(title = "Número de fótons", x = "Tempo", y = "Fótons") + geom_line()

db_idl <- gerar_db_idls(db_fotons_wider)

# Definir os dados
x <- db_idl$Concentracao
y <- db_idl$idl

# Plotar o gráfico
ggplot(db_idl, aes(x = Concentracao, y = idl)) +
  # Adicione pontos de dispersão
  geom_point() +
  # Adicione um ajuste polinomial de segundo grau
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  # Ajuste o número de marcas nos eixos x e y
  scale_x_continuous(breaks = seq(min(db_idl$Concentracao), max(db_idl$Concentracao), by = 1)) +
 
  # Adicione rótulos aos eixos
  labs(x = "Concentração", y = "IDL(%)") +
  # Adicione um título ao gráfico
  ggtitle("Ajuste Polinomial de Concentração vs. IDL(%)")
