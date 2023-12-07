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

server <- function(input,output,session){
  
    rv_databaseFotonsPorConcentracao <- reactiveVal()
    rv_databaseAreasSobCurvaDoIntervalo <- reactiveVal()
    df_rv_listaArquivos <- reactiveVal()
    diretorio <- ''
    #Botão De Selecionar Diretório
    
    observeEvent(input$plotAll, {

        output$textoDiretorio <- renderText({'Nenhum diretório selecionado'})
  observeEvent(input$selecionarDiretorio, {
      #Seleciona o diretório
      diretorio <- dlg_dir(default = getwd())$res
      
      #Renderiza o local do diretorio na tela
      output$textoDiretorio <- renderText({diretorio})
      
        }
      )
  
      if (is.null(rv_diretorio())) {
        showModal(modalDialog(
          title = "Aviso",
          "Por favor, escolha um diretório antes de gerar os gráficos.",
          footer = NULL,
          easyClose = TRUE
        ))
        return()
      }
      
      all_excel_files <- rv_listaArquivos() %>%
        set_names(rv_listaArquivos()) %>%
        map_dfr(read_excel, sheet = 2, .id = "filename") %>%
        mutate(filename = str_extract(basename(filename), "\\d+\\.\\d+")) %>%
        pivot_wider(names_from = filename, values_from = L01S03R01)
        delayed_lum_data_data <- filter(all_excel_files, Time > 0.9)
        rv_databaseFotonsPorConcentracao(delayed_lum_data_data)
      
      })
    #Plota Area
    observeEvent(input$plotArea, {
      
      
      


      
      # Store the area data
      rv_databaseAreasSobCurvaDoIntervalo(Areas_df)
      
    })
    
    #Renderiza o plot de linha
    output$plots <- renderPlotly({
      if (is.null(rv_databaseFotonsPorConcentracao())) {
        return()
      }
      
      delayed_lum_data_long <- rv_databaseFotonsPorConcentracao() %>%
        pivot_longer(cols = -Time, names_to = "Amostra", values_to = "Photons") %>%
        mutate(Concentracao = paste0(substr(Amostra, 1, 1), "g/L"))
      
      g <- ggplot(data = delayed_lum_data_long, aes(x = Time, y = Photons, group = Amostra, color = Concentracao)) +
        labs(title = "Número de fótons", x = "Tempo", y = "Fótons") + geom_line()
      
      ggplotly(g)
    })
    
    output$area_plot <- renderPlotly({
      if (is.null(rv_databaseAreasSobCurvaDoIntervalo())) {
        return()
      }
      
      area_data <- rv_databaseAreasSobCurvaDoIntervalo()
      bar <- ggplot(data = area_data, aes(x = Arquivo, y = Area, fill = Concentracao)) +
        geom_bar(stat = "identity") +
        labs(title = "Áreas dos gráficos",
             y = "Fótons x tempo")+
        theme_minimal()
      ggplotly(bar)
      
    })
                                      
  
}