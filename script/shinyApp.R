#
# É esperado que este script crie um dashboard criado usando o Shiny do R studio. 
# Você pode rodar o código clicando em "Run App". 
# A UI (interface do usuário) define o layout e IDs de outputs do dashboard.
# O server define a estrutura das visualizações que serão criadas
# É preciso carregar os pacotes e bases de dados utilizadas fora dos ambientes de 
# UI e Server.
#

# Instalando pacotes
if (!require('pacman')) install.packages('pacman'); library('pacman')
p_load(shinydashboard, shiny, htmltools, DT, scales,ggplot2, plotly,geobr, sf, 
       leaflet, bslib, dygraphs, readxl, dplyr, tidyverse, tidyr,readr, deflateBR, 
       devtools, lubridate, arrow, here,data.table, shinyjs, rio, snakecase, janitor)

# Função para extrair a última data de atualização da base de dados
# através da data do arquivo na pasta
#get_last_modified <- function(file_path) {
 # file_info <- file.info(file_path)
 # return(file_info$mtime)
#}

data <-import_list(here("data/dados_empresas.xlsx"))

for (obj in names(data)) {
  cleaned_name <- janitor::make_clean_names(obj, case = "snake")
  assign(cleaned_name, data[[obj]])
}

codebook_1 <- codebook_1 %>% select(-4)
codebook_2 <- codebook_2 %>% select(-4)

# Cores para os gráficos
cores_fjles <- c("#ED1B2F", "#71BF44", "#1B65A6", "#FF7F00", "#FFFF33")

# adicionando a logo como título do dash
titulo <- tags$a(href = 'https://fundacaojles.org.br/',
                 tags$img(src = 'logo_dpto.png', height = '55', width = '100'),
                 style = "display: flex; justify-content: center;
                          align-items: center; height: 100%;")

# Define a UI para o aplicativo Shiny
ui <- dashboardPage(
  dashboardHeader(title = titulo,
                  titleWidth = 200),
  dashboardSidebar(width = 200,
                   sidebarMenu(
                     menuItem("Ações", tabName = "page1",
                              menuSubItem("Dados", tabName = "page2")
                              ),
                     menuItem("Categoria das Empresas", tabName = "page3",
                              menuSubItem("Dados", tabName = "page4")
                              )
                     )
                   ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .main-sidebar {
          position: fixed;
        }
        .main-header {
          position: fixed;
          width:100%;
        }
        .content-wrapper {
          margin-top: 70px;
        }
        .skin-blue .main-header .navbar {
          background-color: #192A3D;
          height: 70px;
        }
        .skin-blue .main-header .logo {
          background-color: #192A3D;
          height: 70px;
        }
        .skin-blue .main-sidebar {
          background-color: #192A3D;
          padding-top: 70px
        }
        .main-header .navbar .sidebar-toggle {
          display: flex;
          justify-content: center;
          align-items: center;
          height: 100%;
        }
      ")
                 )
      ),
    useShinyjs(), 
    tabItems(
      tabItem(tabName = "page1",
              fluidRow(
                )
              ), 
      tabItem(tabName = "page2", 
              fluidRow(
                box(width = 12,
                    title = "Banco de Dados",
                    dataTableOutput("table1")
                    ),
                box(width=12,
                    actionButton("toggle1", "Livro de códigos", icon = icon("plus"),
                                 style = "background-color: transparent; border: none;padding-left: 10px;"),
                    div(id = "table_div1", style = "display: none; overflow: hidden;max-height: 0px; transition: max-height 0.5s ease-out;",
                        dataTableOutput("table2")
                        )
                    )
                )
              ), 
      tabItem(tabName = "page3",
              fluidRow(
                )
              ),
      tabItem(tabName = "page4", 
              fluidRow(
                box(width = 12,
                    title = "Banco de Dados",
                    dataTableOutput("table3")
                    ),
                box(width=12,
                    actionButton("toggle2", "Livro de códigos", icon = icon("plus"),
                               style = "background-color: transparent; border: none; padding-left: 10px;"),
                    div(id = "table_div2", style = "display: none; overflow: hidden; max-height: 0px; transition: max-height 0.5s ease-out;", 
                      dataTableOutput("table4")
                      )
                    )
                )
              )
      )
    )
  )


server <- function(input, output, session) {
  
  output$table1 <- renderDataTable({
    DT::datatable(acoes,
                  rownames = FALSE,
                  options = list(
                    scrollY = "300px",
                    scrollX = TRUE,
                    paging = FALSE
                    )
                  )
    })
  
  # Variável reativa para controlar a visibilidade
  table_visible1 <- reactiveVal(FALSE)

  observeEvent(input$toggle1, {
    table_visible1(!table_visible1())  
    if (table_visible1()) {
      updateActionButton(session, "toggle1", icon = icon("minus"))  
      shinyjs::show("table_div1")  
      runjs('$("#table_div1").css("max-height", "500px");') 
    } else {
      updateActionButton(session, "toggle1", icon = icon("plus"))  
      runjs('$("#table_div1").css("max-height", "0px");')  
    }
  })

  output$table2 <- renderDataTable({
    DT::datatable(codebook_1,
                  rownames = FALSE,
                  options = list(
                    scrollY = "300px",
                    paging = FALSE
                    )
                  )
    }) 
  
  table_visible2 <- reactiveVal(FALSE)
  
  observeEvent(input$toggle2, {
    table_visible2(!table_visible2())  
    if (table_visible2()) {
      updateActionButton(session, "toggle2", icon = icon("minus"))  
      shinyjs::show("table_div2")  
      runjs('$("#table_div2").css("max-height", "500px");') 
    } else {
      updateActionButton(session, "toggle2", icon = icon("plus"))  
      runjs('$("#table_div2").css("max-height", "0px");')  
    }
  })
  
  output$table3 <- renderDataTable({
    DT::datatable(categorias_empresas,
                  rownames = FALSE,
                  options = list(
                    scrollY = "300px",
                    scrollX = TRUE,
                    paging = FALSE
                    )
                  )
  })
  
  output$table4 <- renderDataTable({
    DT::datatable(codebook_2,
                  rownames = FALSE,
                  options = list(
                    scrollY = "300px",
                    paging = FALSE
                    )
                  )
    }) 
  
    #file_path <- "data/base_final_harm_shiny_pagamento.csv"  # Caminho para a base de dados do dash
    
    # informação da última data de atualização da base de dados
    #output$last_updated <- renderText({
     # last_modified <- get_last_modified(file_path)
      #paste("Base de dados atualizada pela última vez em:", last_modified)
    #})
  }

# Executa o aplicativo Shiny
shinyApp(ui = ui, server = server)