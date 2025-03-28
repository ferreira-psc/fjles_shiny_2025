# É esperado que este script crie um dashboard criado usando o Shiny do R studio. 
# Você pode rodar o código clicando em "Run App". 

#### PACOTES ####
  if (!require('pacman')) install.packages('pacman'); library('pacman')
  p_load(shinydashboard, shiny)
  
#### SCRIPTS ####
  source("global.R") # O global é carrega objetos que precisam estar disponíveis para a UI e o servidor
  source("ui.R", local = TRUE) # A UI (interface do usuário) define o layout e IDs de outputs do dashboard.
  source("server.R", local = TRUE) # O server define a estrutura das visualizações que serão criadas

#### APP ####
  shinyApp(ui = ui, server = server)