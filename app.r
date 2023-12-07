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

source('ui.R', local = TRUE)
source('server.R')


shinyApp(
  ui = ui,
  server = server
)