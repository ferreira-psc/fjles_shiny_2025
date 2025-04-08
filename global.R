# SCRIPT DE DEFINIÇÃO DO AMBIENTE GLOBAL

#### PACOTES ####
if (!require('pacman')) install.packages('pacman'); library('pacman')
p_load(shinydashboard, shiny, htmltools, DT, scales, ggplot2,
       plotly, geobr, sf,leaflet, bslib, dygraphs, readxl,
       tidyverse, readr,  devtools, lubridate, arrow, here,
       data.table, shinyjs, bslib, leaflet.extras, 
       shinyWidgets, stringi, logging, rio)

#### CORES ####
  cores_fjles <- c("#ED1B2F", "#71BF44", "#1B65A6", "#FF7F00", "#FFFF33")

  cores_fjles_expandida <- c(
    "#BF1526", "#ED1B2F", "#FF5566",  
    "#52922E", "#71BF44", "#A3E076",  
    "#144D80", "#1B65A6", "#4C9EE6",  
    "#CC6600", "#FF7F00", "#FFA64D", 
    "#CCCC29", "#FFFF33", "#FFFF99" 
    )
  
  cores_fjles_claro <- c("#FF5566", "#A3E076", "#4C9EE6","#FFA64D","#FFFF99", "purple")
  
#### DADOS ####
  
  # Importação dos dados tratados
  data <-import_list(here("data/dados_tratados.xlsx"))
  
  for (obj in names(data)) {
    cleaned_name <- janitor::make_clean_names(obj, case = "snake")
    assign(cleaned_name, data[[obj]])
  }
  
  # Recarregar os arquivos Parquet e reconstruir as geometrias
  emp_estado <- read_parquet("data/emp_estado.parquet") %>%
    mutate(geom= st_as_sfc(geom))  
  
  emp_regiao <- read_parquet("data/emp_regiao.parquet") %>%
    mutate(geom= st_as_sfc(geom))
  
  fund_estado <- read_parquet("data/fund_estado.parquet") %>%
    mutate(geom= st_as_sfc(geom))
  
  fund_regiao <- read_parquet("data/fund_regiao.parquet") %>%
    mutate(geom= st_as_sfc(geom))