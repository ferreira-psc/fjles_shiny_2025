# SCRIPT DE DEFINIÇÃO DA INTERFACE DO USUÁRIO
ui <- dashboardPage(
  dashboardHeader(title = tags$a(
                    href = 'https://fundacaojles.org.br/',
                    tags$img(src = 'logo_dpto.png',
                             height = '120',
                             width = '120'),
                    class = "logo-container"
                    ),
                  titleWidth = 230
                  ),
  dashboardSidebar(width = 230,
                   sidebarMenu(
                     
                     menuItem("Fundações", tabName = "fundações", 
                              menuSubItem("Ações", tabName = "acoes_fundacoes")
                     ),
                     menuItem("Empresas", tabName = "empresas",
                              menuSubItem("Perfil das empresas", tabName = "perfil_empresas"),
                              menuSubItem("Ações", tabName = "acoes_empresas"),
                              menuSubItem("Outras informações relevantes", tabName = "cruzamentos_empresas")
                     )
                   )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"), #IMPORTA CSS
        tags$script(src = "custom.js") #IMPORTA JS
      ), 
    tabItems(
      tabItem(tabName = "acoes_fundacoes", #PÁGINA 1 - AÇÕES DE FUNDAÇÕES
              fluidRow(
                box(
                  width = 12, 
                  leafletOutput("mapa_fund"),
                  div(id = "tabela_execucoes_mapa_fund",
                      actionButton("fechar_tabela_mapa_fund", "X", class = "btn-danger"),
                      DTOutput("tabela_mapa_fund") 
                  ), em("*Foi considerada como presente na região qualquer ação realizada em pelo menos um estado daquela área.
                  Além disso, cada iniciativa poderia ter sido implementada em um ou mais estados e regiões,
                  o que explica a discrepância entre a soma das ações por região e o total de 387 ações analisadas.
                        Dentre as quais, 85 delas não possuem informações de onde foram realizadas.")
                )),
              fluidRow(
                box(width = 6, 
                           selectInput("grafico_fund", "Ações de Fundações por", 
                                       choices = c("Tipo", "Mecanismo", "Objetivo",  #ESCOLHA DO GRÁFICO PELO INPUT DO USUÁRIO
                                                   "ODS", "Elo da cadeia do alimento", "Ano de financiamento")
                           )
                    ),
                  valueBoxOutput("total_fund", width = 6)
                  ),
              fluidRow(
                box(width = 12,
                  uiOutput("dynamic_plot_fund") #OUTPUTS DOS GRÁFICOS DEFINIDOS PELO INPUT (GRÁFICOS DE BARRAS)
              )
              ),
              fluidRow(
                box(width = 12, #OUTPUT DA TABELA DE BANCO DE DADOS 
                    title = "Banco de Dados",
                    dataTableOutput("table_fund"),
                    br(),
                    dropdownButton( 
                      label = "Download", 
                      circle = FALSE, 
                      status = "primary",
                      icon = icon("download"),
                      downloadButton("download_csv_fund", "CSV"), #DOWNLOAD DE ARQUIVO EM CSV
                      downloadButton("download_xlsx_fund", "Excel") #DOWNLOAD DE ARQUIVO EM XLSX
                    )
                ),
                box(width=12, #OUTPUT DA TABELA DE BANCO DE DADOS E BOTÃO DE DOWNLOAD
                    actionButton("toggle_fund_code", "Livro de códigos", #BOTÃO DE ABRIR/FECHAR A DIV DE EXPOSIÇÃO DO OUTPUT
                                 icon = icon("plus"),
                                 class = "btn-transparent"),
                    div(id = "table_div_fund_code", class = "collapsible-content", #DIV
                        dataTableOutput("table_fund_code"),
                        br(),
                        downloadButton("download_xlsx_fund_code", "Download") #DOWNLOAD DE ARQUIVO EM XLSX
                    )
                )
              )
      ),
      tabItem(tabName = "perfil_empresas", #PÁGINA 2 - PERFIL DAS EMPRESAS
              fluidRow(box(width = 6, 
                           selectInput("grafico_perfil", "Empresas", #ESCOLHA DO GRÁFICO PELO INPUT DO USUÁRIO
                                       choices = c("Por setor", 
                                                   "Que possuem fundações, por setor",
                                                   "Por atuação nos elos da cadeia do alimento", 
                                                   "Por priorização da segurança alimentar por ano")
                           )
              ), valueBoxOutput("total_perfil", width = 6)
              ),
              fluidRow(
                box(width = 12,
                    uiOutput("dynamic_plot_perfil") #OUTPUTS DOS GRÁFICOS DEFINIDOS PELO INPUT (GRÁFICOS DE BARRAS)
                )
              ),
              fluidRow(
                box(width = 12, #OUTPUT DA TABELA DE BANCO DE DADOS 
                    title = "Banco de Dados",
                    dataTableOutput("table_perfil"),
                    br(),
                    dropdownButton( 
                      label = "Download", 
                      circle = FALSE, 
                      status = "primary",
                      icon = icon("download"),
                      downloadButton("download_csv_perfil", "CSV"), #DOWNLOAD DE ARQUIVO EM CSV
                      downloadButton("download_xlsx_perfil", "Excel") #DOWNLOAD DE ARQUIVO EM XLSX
                    )
                ),
                box(width=12, #OUTPUT DA TABELA DE BANCO DE DADOS E BOTÃO DE DOWNLOAD
                    actionButton("toggle_perfil_code", "Livro de códigos", #BOTÃO DE ABRIR/FECHAR A DIV DE EXPOSIÇÃO DO OUTPUT
                                 icon = icon("plus"),
                                 class = "btn-transparent"),
                    div(id = "table_div_perfil_code", class = "collapsible-content", #DIV
                        dataTableOutput("table_perfil_code"),
                        br(),
                        downloadButton("download_xlsx_perfil_code", "Download") #DOWNLOAD DE ARQUIVO EM XLSX
                    )
                )
              )
      ),
      tabItem(tabName = "acoes_empresas", #PÁGINA 3 - AÇÕES DE EMPRESAS 
              fluidRow(
                box(
                  width = 12, 
                  leafletOutput("mapa_emp"), # Mapa de ações por região/estado
                  div(id = "tabela_execucoes_mapa_emp",
                      actionButton("fechar_tabela_mapa_emp", "X", class = "btn-danger"),
                      DTOutput("tabela_mapa_emp") # Tabela de ações por estado
                  ), em("*Foi considerada como presente na região qualquer ação realizada em pelo menos um estado daquela área.
                  Além disso, cada iniciativa poderia ter sido implementada em um ou mais estados e regiões,
                  o que explica a discrepância entre a soma das ações por região e o total de 696 ações analisadas.
                        Dentre as quais, 262 não possuem informações de onde foram realizadas.")
                )),
              fluidRow(
                box(width = 6, 
                           selectInput("grafico_emp", "Ações de Empresas por", 
                                       choices = c("Tipo", "Mecanismo", "Objetivo",  #ESCOLHA DO GRÁFICO PELO INPUT DO USUÁRIO
                                                   "ODS", "Elo da cadeia do alimento", "ESG",
                                                   "Partes interessadas", "Ano de financiamento", "Grupos em maior risco de insegurança alimentar e nutricional")
                           )
              ), valueBoxOutput("total_emp", width = 6)
              ),
              fluidRow(
                box(width = 12,
                    uiOutput("dynamic_plot_emp") #OUTPUTS DOS GRÁFICOS DEFINIDOS PELO INPUT (GRÁFICOS DE BARRAS)
              )
              ),
              fluidRow(
                box(width = 12, #OUTPUT DA TABELA DE BANCO DE DADOS 
                    title = "Banco de Dados",
                    dataTableOutput("table_emp"),
                    br(),
                    dropdownButton( 
                      label = "Download", 
                      circle = FALSE, 
                      status = "primary",
                      icon = icon("download"),
                      downloadButton("download_csv_emp", "CSV"), #DOWNLOAD DE ARQUIVO EM CSV
                      downloadButton("download_xlsx_emp", "Excel") #DOWNLOAD DE ARQUIVO EM XLSX
                      )
                    ),
                box(width=12, #OUTPUT DA TABELA DE BANCO DE DADOS E BOTÃO DE DOWNLOAD
                    actionButton("toggle_emp_code", "Livro de códigos", #BOTÃO DE ABRIR/FECHAR A DIV DE EXPOSIÇÃO DO OUTPUT
                                 icon = icon("plus"),
                                 class = "btn-transparent"),
                    div(id = "table_div_emp_code", class = "collapsible-content", #DIV
                        dataTableOutput("table_emp_code"),
                        br(),
                        downloadButton("download_xlsx_emp_code", "Download") #DOWNLOAD DE ARQUIVO EM XLSX
                        )
                    )
                )
              ),
      tabItem(tabName = "cruzamentos_empresas", #PÁGINA 4 - CRUZAMENTOS ESPECÍFICOS 
              fluidRow(
                box(width = 12, 
                    selectInput("grafico_cru", "Escolha o gráfico:", 
                                choices = c("Atuação e investimento nos elos da cadeia do alimento",
                                            "Ações por setor e ESG",
                                            "Ações por setor e por grupos em maior risco de insegurança alimentar e nutricional",  #ESCOLHA DO GRÁFICO PELO INPUT DO USUÁRIO
                                            "Média de certificações por empresa e por setor")
                    )
                ),
                box(width = 12,
                    uiOutput("dynamic_plot_cru") #OUTPUTS DOS GRÁFICOS DEFINIDOS PELO INPUT (GRÁFICOS DE BARRAS)
                )
              ))
      )
    )
)