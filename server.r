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

server <- function(input,output,session){

  output$texto_tela_diretorio <- renderText({
    if (input$selecionar_diretorio > 0) {
      diretorio <- selecionar_diretorio()
      return(paste("Diretório selecionado: ", diretorio))
    }
  })

  observeEvent(input$selecionar_diretorio, {
    if (input$selecionar_diretorio > 0) {
      diretorio <- selecionar_diretorio()
      return(paste("Diretório selecionado: ", diretorio))
    }
  })
}
