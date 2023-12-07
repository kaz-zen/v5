#install.packages("shiny")
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

TEMPO_MAX <- 60
TEMPO_MIN <- 1

ui <- fluidPage( 

  sidebarLayout(
    sidebarPanel(
      textOutput("texto_tela_diretorio"), p(),
      actionButton("selecionar_diretorio", "Selecionar diretório"),
      actionButton("plotar_grafico_linhas", "Gerar gráfico das curvas"),
      actionButton("plotar_grafico_area", "Gerar gráfico da área"),
      actionButton("plotar_grafico_inibicao", "Gerar gráfico da média das replicatas"), #nolint
      p(),
      sliderInput("sliderRange", "Selecione um valor para o cálculo da área sob a curva:", #nolint
                  min = TEMPO_MIN,
                  max = TEMPO_MAX,
                  value = c(TEMPO_MIN, TEMPO_MAX),
                  step = 0.1) #nolint
    ),
    mainPanel(
      br(),
      img(src = "unicamp.png", align = "right", width = "7%"),
      br(), br(), br(),
      tabsetPanel(
        tabPanel("Fótons Por Concentração",
                 plotlyOutput("grafico_fotonsconc", height = "750px")),
        tabPanel("Área Sob Curva",
                 plotlyOutput("grafico_area_sob_curva", height = "750px")),
        tabPanel("Inibição",
                 plotlyOutput("grafico_inibicao", height = "750px"))
   )
  ),
  )
)