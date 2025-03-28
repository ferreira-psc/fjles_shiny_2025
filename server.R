# SCRIPT DE DEFINIÇÃO DO SERVIDOR
server <- function(input, output, session) {

#### FUNÇÕES ####
  
 # Função para criar os gráficos de barras
  create_plotly <- function(data_obj, variable, title, colors) {
    
    # Criando ggplot
      plot_obj <- ggplot(data_obj, aes(x = .data[[variable]], y = acoes, fill = .data[[variable]])) +
        geom_bar(stat = "identity") +
        geom_text(aes(y = acoes + (max(acoes) * 0.05), label = paste0(acoes, "\n", round(relativo, 1), "%")), 
                  vjust = -1.2, size = 3) +
        scale_fill_manual(values = colors) +
        labs(title = title, x = NULL, y = NULL) +
        theme_minimal() +
        theme(axis.text.x = element_blank(),
              legend.text = element_text(size = 10),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        guides(fill = guide_legend(title = NULL))
    
    # Convertendo para plotly e definindo configurações de exibição
      plotly_obj <- ggplotly(plot_obj, tooltip = NULL) %>%
        style(hoverinfo = "none") %>%
        config(
          modeBarButtonsToRemove = c("zoom2d", "zoomIn2d", "zoomOut2d", "autoScale2d",
                                     "pan2d", "select2d", "lasso2d", "resetScale2d",
                                     "hoverClosestCartesian", "hoverCompareCartesian", 
                                     "toggleSpikelines"),
          displaylogo = FALSE 
        )
    
    # Executando plotly
      return(plotly_obj)
  }
  
  # Função para criar os labels usados nos mapas
    labels_group <- function(dados, nome_col, contagem_col) {
      lapply(
        sprintf("<strong>%s</strong><br/>%s ações", dados[[nome_col]], round(dados[[contagem_col]], 2)), 
        htmltools::HTML
      )
    }
  
  # Função para criar os mapas com as tabelas internas
    render_mapa <- function(id, dados_regiao, dados_estado, dados_unique, cores, colunas_selecionadas) {
      
      rv <- reactiveValues(region_selected = NULL, state_selected = NULL)  
      
      # Definindo a exibição inicial do mapa, agregado por regiões
        output[[id]] <- renderLeaflet({
          leaflet() %>%
            addProviderTiles("Esri.WorldImagery") %>%  
            addPolygons(
              data = dados_regiao,
              fillColor = cores,
              color = "white", weight = 0.7, smoothFactor = 0.5,
              opacity = 1, fillOpacity = 0.9
            ) %>%
            addCircleMarkers(
              data = dados_regiao,
              lng = ~lng, lat = ~lat,
              radius = ~sqrt(contagem) * 0.8,
              color = "#333333",
              fillColor = "#333333",
              fillOpacity = 0.9,
              label = labels_group(dados_regiao, "name_region", "contagem"),
              layerId = ~name_region  
            ) %>%
            addLabelOnlyMarkers(
              data = dados_regiao,
              lng = ~lng, lat = ~lat,
              label = ~round(contagem, 0),
              labelOptions = labelOptions(
                noHide = TRUE, direction = "center", textOnly = TRUE,
                className = "label-marker"
              )
            ) %>%
            addEasyButton(
              easyButton(
                icon = htmltools::tags$div(
                  style = "width: 19px; height: 29px;
              display: flex; align-items: center; justify-content: center;", 
                  htmltools::tags$i(class = "fa fa-refresh",
                                    style = "font-size: 22px; color: black;")
                ), 
                title = "Resetar mapa",
                onClick = JS(paste0("function(btn, map) {Shiny.setInputValue('reset_", id, "', Math.random());
            map.setView([-14.2350, -51.9253], 3);
          }"))
              )
            )
        })
      
      # Exibindo estados da região selecionada
        observeEvent(input[[paste0(id, "_marker_click")]], {
          click <- input[[paste0(id, "_marker_click")]]
  
          if (is.null(click$id)) return()
          
          if (click$id %in% dados_regiao$name_region) {
            rv$region_selected <- click$id
            rv$state_selected <- NULL  
            
            estados_filtrados <- dados_estado %>%
              filter(name_region == rv$region_selected)
            
            centro <- dados_regiao %>%
              filter(name_region == rv$region_selected)
            
            pal <- colorNumeric("YlGnBu", domain = estados_filtrados$contagem)
            
            leafletProxy(id) %>%
              clearShapes() %>%
              clearMarkers() %>%
              setView(lng = centro$lng, lat = centro$lat, zoom = 5.1) %>%
              addPolygons(
                data = estados_filtrados,
                fillColor = ~pal(contagem),
                color = "gray", weight = 1, smoothFactor = 0.5,
                opacity = 1, fillOpacity = 0.9
              ) %>%
              addCircleMarkers(
                data = estados_filtrados,
                lng = ~lng, lat = ~lat,
                radius = ~sqrt(contagem) * 2,
                color = "darkblue",
                fillColor = "darkblue",
                fillOpacity = 0.9,
                label = labels_group(estados_filtrados, "name_state", "contagem"),
                layerId = ~name_state  
              ) %>% 
              addLabelOnlyMarkers(
                data = estados_filtrados,
                lng = ~lng, lat = ~lat,
                label = ~round(contagem, 0),
                labelOptions = labelOptions(
                  noHide = TRUE, direction = "center", textOnly = TRUE,
                  className = "label-marker"
                )
              ) 
          }
          
        # Criando tabela quando um marcador de estado for clicado
          if (click$id %in% dados_unique$estado_atua) {
            rv$state_selected <- click$id
            
            output[[paste0("tabela_", id)]] <- renderDT({
              if (is.null(rv$state_selected) || !(rv$state_selected %in% dados_unique$estado_atua)) {
                return(NULL)  
              }
            
            # Filtrando e selecionando as colunas conforme passadas como parâmetro
              acoes_mod <- dados_unique %>%
                filter(estado_atua == rv$state_selected) %>%
                select(all_of(colunas_selecionadas)) %>%
                mutate(across(everything(), ~ ifelse(
                  nchar(.) > 40,
                  paste0(
                    "<div class='truncate-cell'>", ., "</div>",
                    "<span class='expand-button' onclick='toggleExpand(this)'>[+]</span>"
                  ),
                  .
                )))
              
              datatable(acoes_mod, 
                        rownames = FALSE,
                        escape = FALSE, 
                        class = "compact",
                        options = list(scrollY = "280px", scrollX = TRUE, paging = FALSE, dom = 't'))
            })
          
          # Exibindo tabela
            shinyjs::show(paste0("tabela_execucoes_", id))  
        }
      })
      
      # Fechando tabela quando botão vermelho for pressionado
        observeEvent(input[[paste0("fechar_tabela_", id)]], {
          rv$state_selected <- NULL  
          shinyjs::hide(paste0("tabela_execucoes_", id))  
        })
      
      # Resetando o mapa quando o botão for pressionado
        observeEvent(input[[paste0("reset_", id)]], {
          rv$region_selected <- NULL  
          rv$state_selected <- NULL  
        
        # Atualizando o mapa
          output[[id]] <- renderLeaflet({
            leaflet() %>%
              addProviderTiles("Esri.WorldImagery") %>%  
              addPolygons(
                data = dados_regiao,
                fillColor = cores,
                color = "white", weight = 0.7, smoothFactor = 0.5,
                opacity = 1, fillOpacity = 0.9
              ) %>%
              addCircleMarkers(
                data = dados_regiao,
                lng = ~lng, lat = ~lat,
                radius = ~sqrt(contagem) * 0.8,
                color = "#333333",
                fillColor = "#333333",
                fillOpacity = 0.9,
                label = labels_group(dados_regiao, "name_region", "contagem"),
                layerId = ~name_region  
              ) %>%
              addLabelOnlyMarkers(
                data = dados_regiao,
                lng = ~lng, lat = ~lat,
                label = ~round(contagem, 0),
                labelOptions = labelOptions(
                  noHide = TRUE, direction = "center", textOnly = TRUE,
                  className = "label-marker"
                )
              ) %>%
              addEasyButton(
                easyButton(
                  icon = htmltools::tags$div(
                    style = "width: 19px; height: 29px;
                display: flex; align-items: center; justify-content: center;", 
                    htmltools::tags$i(class = "fa fa-refresh",
                                      style = "font-size: 22px; color: black;")
                  ), 
                  title = "Resetar mapa",
                  onClick = JS(paste0("function(btn, map) {Shiny.setInputValue('reset_", id, "', Math.random());
              map.setView([-14.2350, -51.9253], 3);
            }"))
                )
              )
          })
        
        # Fechando tabela, caso esteja aberta, antes do reset do mapa
          shinyjs::hide(paste0("tabela_execucoes_", id))
      })
    }
    
    # Função para renderizar uma tabela DT com truncamento de células
      render_custom_table <- function(id, data) {
        output[[paste0("table_", id)]] <- renderDT({
          data_mod <- data %>%
            mutate(across(everything(), ~ ifelse(
              nchar(.) > 50,
              paste0(
                "<div class='truncate-cell'>", ., "</div>",
                "<span class='expand-button' onclick='toggleExpand(this)'>[+]</span>"
              ),
              .
            )))
          
          datatable(data_mod,
                    rownames = FALSE,
                    escape = FALSE,
                    options = list(
                      scrollY = "300px",
                      scrollX = TRUE,
                      paging = FALSE,
                      dom = 't'
                    ))
        })
      }
    
    # Função para download de CSV
      create_csv_download <- function(id, data, filename) {
        output[[paste0("download_csv_", id)]] <- downloadHandler(
          filename = function() { filename },
          content = function(file) {
            write.csv(data, file, row.names = FALSE)
          }
        )
      }
    
    # Função para download de XLSX
      create_xlsx_download <- function(id, data, filename) {
        output[[paste0("download_xlsx_", id)]] <- downloadHandler(
          filename = function() { filename },
          content = function(file) {
            writexl::write_xlsx(data, file)
          }
        )
      }
    
    # Função para alternar a visibilidade da tabela
      create_toggle_button <- function(id, session) {
        table_visible <- reactiveVal(FALSE)
        
        observeEvent(input[[paste0("toggle_", id)]], {
          shinyjs::toggle(id = paste0("table_div_", id), anim = TRUE, animType = "slide", time = 0.5)
          table_visible(!table_visible())
          updateActionButton(session, paste0("toggle_", id), icon = icon(ifelse(table_visible(), "minus", "plus")))
        })
      }

      # Função para criar gráficos de barras empilhadas
      grafico_stack <- function(data, x1_var, y_var, fill_var, percent_var, title, total_var, colors) {
        
        # Criando o gráfico com ggplot
        plot <- ggplot(data, aes_string(x = x1_var, y = y_var, fill = fill_var)) +
          geom_bar(stat = "identity", position = "stack") +
          geom_text(aes(label = scales::percent(!!sym(percent_var), accuracy = 1)), 
                    position = position_stack(vjust = 0.5), size = 3) +  # Removendo casas decimais
          geom_text(aes(y = !!sym(total_var) + (max(.data[[total_var]], na.rm = TRUE) * 0.08), 
                        label = !!sym(total_var)), 
                    vjust = -1.2, size = 3.5) +
          scale_fill_manual(values = colors) +
          labs(title = title) +  
          theme_minimal() +
          theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.text = element_text(size = 10)
          ) +
          guides(fill = guide_legend(title = NULL))
        
        # Convertendo o gráfico ggplot para plotly
        plotly <- ggplotly(plot, tooltip = NULL) %>%
          style(hoverinfo = "none") %>%
          config(
            modeBarButtonsToRemove = c("zoom2d", "zoomIn2d", "zoomOut2d", "autoScale2d",
                                       "pan2d", "select2d", "lasso2d", "resetScale2d",
                                       "hoverClosestCartesian", "hoverCompareCartesian", 
                                       "toggleSpikelines"),
            displaylogo = FALSE
          )
        
        return(plotly)
      }

      
#### RENDERIZAÇÃO ####
      
    # Criando mapa de Ações de Empresa
      render_mapa(
        id = "mapa_emp", 
        dados_regiao = emp_regiao, 
        dados_estado = emp_estado, 
        dados_unique = emp_mapa, 
        cores = cores_fjles,
        colunas_selecionadas = c("empresa", "tipo", "iniciativa", "ods2", "mecanismo", "objetivos") 
      )
      
    # Criando mapa de Ações de Fundações
      render_mapa(
        id = "mapa_fund", 
        dados_regiao = fund_regiao, 
        dados_estado = fund_estado, 
        dados_unique = fund_mapa, 
        cores = cores_fjles,
        colunas_selecionadas = c("fund_insti", "iniciativa", "tipo",  "mecanismo", "obj_ini01", "obj_ini02" ) 
      )

    # Criando banco de dados de Ações de Fundações
      render_custom_table("fund", fund_banco)
    
    # Criando botões de download do banco de dados Ações de Fundações
      create_csv_download("fund", fund_banco, "fund_acoes.csv") #CSV
      create_xlsx_download("fund", fund_banco, "fund_acoes.xlsx") #XLSX 
    
    # Criando botão de abertura do codebook de Ações de Fundações
      create_toggle_button("fund_code", session)
    
    # Criando codebook de Ações de Fundações
      render_custom_table("fund_code", fund_codebook)
      
    # Criando botão de download do codebook de Ações de Fundações
      create_xlsx_download("fund_code", fund_codebook, "fund_codebook.xlsx")
    
    # Criando banco de dados de Perfil de Empresas
    render_custom_table("perfil", perfil_banco)
    
    # Criando botões de download do banco de dados Perfil de Empresas  
    create_csv_download("perfil", perfil_banco, "perfil_banco.csv") #CSV
    create_xlsx_download("perfil", perfil_banco, "perfil_banco.xlsx") #XLSX
    
    # Criando botão de abertura do codebook de Perfil de Empresas 
    create_toggle_button("perfil_code", session)
    
    # Criando codebook de Perfil de Empresas
    render_custom_table("perfil_code", perfil_codebook)
    
    # Criando botões de download do codebook de Perfil de Empresas 
    create_xlsx_download("perfil_code", perfil_codebook, "perfil_codebook.xlsx")
      
    # Criando banco de dados de Ações de Empresas
    render_custom_table("emp", emp_banco)
    
    # Criando botões de download do banco de dados Ações de Empresas  
    create_csv_download("emp", emp_banco, "emp_acoes.csv") #CSV
    create_xlsx_download("emp", emp_banco, "emp_acoes.xlsx") #XLSX
    
    # Criando botão de abertura do codebook de Ações de Empresas 
    create_toggle_button("emp_code", session)
    
    # Criando codebook de Ações de Empresas
    render_custom_table("emp_code", emp_codebook)
    
    # Criando botões de download do codebook de Ações de Empresas 
    create_xlsx_download("emp_code", emp_codebook, "emp_codebook.xlsx")
    
    # Criando gráfico de barras - Ações de Fundações por Tipo
    output$fund_1 <- renderPlotly({
      
      data_fund_1 <- fund_binario %>%
        distinct(id_ini, tipo) %>% 
        count(tipo) %>%
        rename(acoes = n) %>% 
        mutate(relativo = (acoes / sum(acoes)) * 100) 
      data_fund_1$tipo <- factor(data_fund_1$tipo, 
                                 levels = data_fund_1$tipo[order(data_fund_1$acoes, decreasing = TRUE)])
      
      create_plotly(data_fund_1, "tipo", "Ações de Fundações por Tipo", cores_fjles)
      
    })
    
    # Criando gráfico de barras - Ações de Fundações por Mecanismo
    output$fund_2 <- renderPlotly({
      
      data_fund_2 <- fund_binario %>%
        distinct(id_ini, mecanismo) %>% 
        count(mecanismo) %>%
        rename(acoes = n) %>% 
        mutate(relativo = (acoes / sum(acoes)) * 100) 
      data_fund_2$mecanismo <- factor(data_fund_2$mecanismo, 
                                      levels = data_fund_2$mecanismo[order(data_fund_2$acoes, decreasing = TRUE)])
      
      create_plotly(data_fund_2, "mecanismo", "Ações de Fundações por Mecanismo", cores_fjles_expandida)
      
    })
    
    # Criando gráfico de barras - Ações de Fundações por Objetivo
    output$fund_3 <- renderPlotly({
      
      data_fund_3 <- fund_binario %>%
        select(id_ini, starts_with("obj_")) %>%  # Agora starts_with() está dentro de select()
        distinct() %>% 
        pivot_longer(cols = starts_with("obj_"), values_to = "objetivos") %>%
        select(-name) %>%   
        filter(!is.na(objetivos) & objetivos != "n/a") %>%  
        count(objetivos, name = "acoes") %>% 
        mutate(
          relativo = (acoes / sum(acoes)) * 100,
          objetivos = fct_reorder(objetivos, acoes, .desc = TRUE)
        )
      
      create_plotly(data_fund_3, "objetivos", "Ações de Fundações por Objetivo", cores_fjles_expandida)
      
    })
    
    # Criando gráfico de barras - Ações de Fundações por ODS
    output$fund_4 <- renderPlotly({
      
      labels <- c(
        "ods_2.1" = "ODS 2.1: acesso ao alimento",
        "ods_2.2" = "ODS 2.2: segurança nutricional",
        "ods_2.3" = "ODS 2.3: produtividade/renda pequenos produtores",
        "ods_2.4" = "ODS 2.4: produção sustentável", 
        "ods_2.5" = "ODS 2.5: diversidade genética",
        "ods_12.2" = "ODS 12.2: uso de recursos naturais",
        "ods_12.3" = "ODS 12.3: redução do desperdício",
        "ods_12.6" = "ODS 12.6: sustentabilidade empresarial"
      )
      
      data_fund_4 <- fund_binario %>%
        select(id_ini, starts_with("ods_")) %>% 
        distinct() %>%  
        summarise(across(-which(names(.) == "id_ini"), sum)) %>%  
        pivot_longer(cols = everything(), names_to = "ods", values_to = "acoes") %>%
        mutate(relativo = (acoes / sum(acoes)) * 100) %>%
        mutate(ods = recode(ods, !!!labels)) %>%
        mutate(ods = fct_reorder(ods, -acoes, .fun = sum)) 
      
      create_plotly(data_fund_4, "ods", "Ações de Fundações por ODS", cores_fjles_expandida)
      
    })
    
    # Criando gráfico de barras - Ações de Fundações por Elo da Cadeia do Alimento 
    output$fund_5 <- renderPlotly({
      
      labels <- c("elo_prod_ali" = "Produção de alimentos",
                  "elo_armaz" = "Armazenamento",
                  "elo_trans_log" = "Transporte",
                  "elo_process" = "Processamento",
                  "elo_var_atac"= "Varejo/Atacado",
                  "elo_consu" = "Consumo"
      )
      
      data_fund_5 <- fund_binario %>%
        select(id_ini, starts_with("elo_")) %>% 
        distinct() %>%  
        summarise(across(-which(names(.) == "id_ini"), sum)) %>%   
        pivot_longer(cols = everything(), names_to = "elo", values_to = "acoes") %>%
        mutate(relativo = (acoes / sum(acoes)) * 100) %>%
        mutate(elo = recode(elo, !!!labels)) %>%
        mutate(elo = fct_reorder(elo, -acoes, .fun = sum)) 
      
      create_plotly(data_fund_5, "elo", "Ações de Fundações por Elo da Cadeia do Alimento", cores_fjles_expandida)
      
    })
    
    # Criando gráfico de barras - Ações de Fundações por Ano de Financiamento
    output$fund_6 <- renderPlotly({
      
      labels <- c("ano20" = "2020",
                  "ano21" = "2021",
                  "ano22" = "2022")
      
      data_fund_6 <- fund_binario %>%
        select(id_ini, starts_with("ano")) %>%
        mutate(across(everything(), ~ suppressWarnings(as.numeric(as.character(.))))) %>% 
        distinct() %>%  
        summarise(across(-which(names(.) == "id_ini"), sum)) %>% 
        pivot_longer(cols = everything(), names_to = "ano", values_to = "acoes") %>%
        mutate(relativo = (acoes / sum(acoes, na.rm = TRUE)) * 100) %>%
        mutate(ano = recode(ano, !!!labels)) %>%
        mutate(ano = fct_reorder(ano, -acoes, .fun = sum)) 
      
      create_plotly(data_fund_6, "ano", "Ações de Fundações por Ano de Financiamento", cores_fjles)
      
    })
    
    # Criando gráfico de barras - Empresas por Setor
    output$perfil_1 <- renderPlotly({
      
      data_perfil_1 <- perfil_binario %>%
        count(set_ativ) %>%
        rename(acoes = n) %>% 
        mutate(relativo = (acoes / sum(acoes)) * 100) 
      data_perfil_1$set_ativ <- factor(data_perfil_1$set_ativ, 
                                 levels = data_perfil_1$set_ativ[order(data_perfil_1$acoes, decreasing = TRUE)])
      
      create_plotly(data_perfil_1, "set_ativ", "Empresas por Setor", cores_fjles)
      
    })
    
    # Criando gráfico de barras - Empresas, que possuem Fundações, por Setor
    output$perfil_2 <- renderPlotly({
      
      data_perfil_2 <- perfil_binario %>%
        filter(emp_fund_insti == 1)%>% 
        count(set_ativ) %>%
        rename(acoes = n) %>% 
        mutate(relativo = (acoes / sum(acoes)) * 100) 
      data_perfil_2$set_ativ <- factor(data_perfil_2$set_ativ, 
                                      levels = data_perfil_2$set_ativ[order(data_perfil_2$acoes, decreasing = TRUE)])
      
      create_plotly(data_perfil_2, "set_ativ", "Empresas, que possuem Fundações, por Setor", cores_fjles)
      
    })
    
    # Criando gráfico de barras - Empresas por Atuação nos Elos da Cadeia
    output$perfil_3 <- renderPlotly({
      
      labels <- c("emp_insum" = "Produção de insumos",
                  "emp_prod_ali" = "Produção de alimentos",
                  "emp_armaz" = "Armazenamento",
                  "emp_transp_log" = "Transporte",
                  "emp_process" = "Processamento",
                  "emp_var_atac"= "Varejo/Atacado",
                  "emp_consu" = "Consumo")
      
      data_perfil_3 <- perfil_binario %>%
        select(starts_with("emp_"), -emp_fund_insti) %>% 
        summarise(across(everything(), sum, na.rm = TRUE)) %>%  
        pivot_longer(cols = everything(), names_to = "elo", values_to = "acoes") %>%
        mutate(
          relativo = (acoes / sum(acoes, na.rm = TRUE)) * 100,
          elo = recode(elo, !!!labels),
          elo = fct_reorder(elo, -acoes)
        )
      
      create_plotly(data_perfil_3, "elo", "Empresas por Atuação nos Elos da Cadeia", cores_fjles_expandida)
      
    })
    
    # Criando gráfico de barras - Empresas por Priorização da Segurança Alimentar por Ano
    output$perfil_4 <- renderPlotly({
      
      labels <- c("pri_ssan_20" = "2020",
                  "pri_ssan_21" = "2021",
                  "pri_ssan_22" = "2022",
                  "pri_ssan_23" = "2023")
      
      data_perfil_4 <- perfil_binario %>%
        select(starts_with("pri")) %>% 
        mutate(across(everything(), ~suppressWarnings(as.numeric(.)))) %>% 
        summarise(across(everything(), sum, na.rm = TRUE)) %>%  
        pivot_longer(cols = everything(), names_to = "ano", values_to = "acoes") %>%
        mutate(
          relativo = (acoes / sum(acoes, na.rm = TRUE)) * 100,
          ano = recode(ano, !!!labels),
          ano = fct_reorder(ano, -acoes)
        )
      
      create_plotly(data_perfil_4, "ano", "Empresas por Priorização da Segurança Alimentar por Ano", cores_fjles_expandida)
      
    })

  # Criando gráfico de barras - Ações de Empresas por Tipo
    output$emp_1 <- renderPlotly({
        
      data_emp_1 <- emp_binario %>%
        distinct(id_iniciativa, tipo) %>% 
        count(tipo) %>%
        rename(acoes = n) %>% 
        mutate(relativo = (acoes / sum(acoes)) * 100) 
      data_emp_1$tipo <- factor( data_emp_1$tipo,
                                 levels =  data_emp_1$tipo[order(data_emp_1$acoes,
                                                                 decreasing = TRUE)])
        
        create_plotly(data_emp_1, "tipo", "Ações de Empresas por Tipo", cores_fjles)
        
      })
    
  # Criando gráfico de barras - Ações de Empresas por Mecanismo
    output$emp_2 <- renderPlotly({
        
      data_emp_2 <- emp_binario %>% 
          distinct(id_iniciativa, mecanismo) %>% 
          count(mecanismo) %>%
          rename(acoes = n) %>% 
          mutate(relativo = (acoes / sum(acoes)) * 100) 
        data_emp_2$mecanismo <- factor(data_emp_2$mecanismo, 
                                                      levels = data_emp_2$mecanismo[order(data_emp_2$acoes,
                                                                                          decreasing = TRUE)])
        
        create_plotly(data_emp_2, "mecanismo", "Ações de Empresas por Mecanismo", cores_fjles_expandida)
        
      })
    
  # Criando gráfico de barras - Ações de Empresas por Objetivo
    output$emp_3 <- renderPlotly({
      
      labels<- c(
        "obj_prod_ali" = "Produção de Alimentos",
        "obj_seg_nutri" = "Segurança Nutricional",
        "obj_ali_fome" = "Alívio da Fome",
        "obj_red_perda" = "Redução de Perdas",
        "obj_reap" = "Reaproveitamento",
        "obj_ace_agua" = "Acesso à água",
        "obj_res_emp" = "Melhores Práticas de Responsabilidade Empresariais",
        "obj_agri_fam" = "Fortalecimento da Agricultura Familiar",
        "obj_red_desp" = "Redução de Desperdícios"
      )
      
      data_emp_3 <- emp_binario %>%
        select(id_iniciativa, starts_with("obj_")) %>% 
        distinct() %>%  
        summarise(across(-which(names(.) == "id_iniciativa"), sum)) %>%  
        pivot_longer(cols = everything(), names_to = "objetivo", values_to = "acoes") %>%
        mutate(relativo = (acoes / sum(acoes)) * 100) %>%
        mutate(objetivo = recode(objetivo, !!!labels)) %>%
        mutate(objetivo = fct_reorder(objetivo, -acoes, .fun = sum)) 
      
      create_plotly(data_emp_3, "objetivo", "Ações de Empresas por Objetivo", cores_fjles_expandida) 
      
    })
    
    # Criando gráfico de barras - Ações de Empresas por ODS
      output$emp_4 <- renderPlotly({
        
        labels <- c(
          "ods_2.1" = "ODS 2.1: acesso ao alimento",
          "ods_2.2" = "ODS 2.2: segurança nutricional",
          "ods_2.3" = "ODS 2.3: produtividade/renda pequenos produtores",
          "ods_2.4" = "ODS 2.4: produção sustentável", 
          "ods_2.5" = "ODS 2.5: diversidade genética",
          "ods_12.2" = "ODS 12.2: uso de recursos naturais",
          "ods_12.3" = "ODS 12.3: redução do desperdício",
          "ods_12.4" = "ODS 12.4: manejo de químicos/resíduos", 
          "ods_12.5" = "ODS 12.5: redução de resíduos",
          "ods_12.6" = "ODS 12.6: sustentabilidade empresarial",
          "ods_12.8" = "ODS 12.8: conscientização individual")
        
        data_emp_4 <- emp_binario %>%
          select(id_iniciativa, starts_with("ods_")) %>% 
          distinct() %>%  
          summarise(across(-which(names(.) == "id_iniciativa"), sum)) %>%  
          pivot_longer(cols = everything(), names_to = "ods", values_to = "acoes") %>%
          mutate(relativo = (acoes / sum(acoes)) * 100) %>%
          mutate(ods = recode(ods, !!!labels)) %>%
          mutate(ods = fct_reorder(ods, -acoes, .fun = sum)) 
        
        create_plotly(data_emp_4, "ods", "Ações de Empresas por ODS", cores_fjles_expandida) 
        
      })
      
    # Criando gráfico de barras - Ações de Empresas por Elo da Cadeia do Alimento
      output$emp_5 <- renderPlotly({
      
        labels <- c("elo_prod_ali" = "Produção de alimentos",
                    "elo_arm" = "Armazenamento",
                    "elo_tra_log" = "Transporte",
                    "elo_pro" = "Processamento",
                    "elo_var_atac"= "Varejo/Atacado",
                    "elo_con" = "Consumo")
        
        data_emp_5 <- emp_binario %>%
          select(id_iniciativa, starts_with("elo_")) %>% 
          distinct() %>%  
          summarise(across(-which(names(.) == "id_iniciativa"), sum)) %>%  
          pivot_longer(cols = everything(), names_to = "elo", values_to = "acoes") %>%
          mutate(relativo = (acoes / sum(acoes)) * 100) %>%
          mutate(elo = recode(elo, !!!labels)) %>%
          mutate(elo = fct_reorder(elo, -acoes, .fun = sum)) 
        
        create_plotly(data_emp_5, "elo", "Ações de Empresas por Elo da Cadeia do Alimento", cores_fjles_expandida)
        
      })
      
    # Criando gráfico de barras - Ações de Empresas por ESG
      output$emp_6 <- renderPlotly({
        
        labels <- c("social" = "Benefício Social",
                    "ambiental" = "Benefício Ambiental",
                    "governanca" = "Benefício de Governança da Empresa")
        
        data_emp_6 <- emp_binario %>%
          select(id_iniciativa, social, ambiental, governanca) %>%  
          distinct() %>%  
          summarise(across(-which(names(.) == "id_iniciativa"), sum)) %>%  
          pivot_longer(cols = everything(), names_to = "esg", values_to = "acoes") %>%
          mutate(relativo = (acoes / sum(acoes)) * 100) %>%
          mutate(esg = recode(esg, !!!labels)) %>%
          mutate(esg = fct_reorder(esg, -acoes, .fun = sum)) 
        
        create_plotly(data_emp_6, "esg", "Ações de Empresas por ESG", cores_fjles)
        
      })
      
      # Criando gráfico de barras - Ações de Empresas por Partes Interessadas
      output$emp_7 <- renderPlotly({
        
        labels <- c("part_int_cli" = "Ação envolve clientes da empresa",
                    "part_int_for" = "Ação envolve fornecedores",
                    "part_int_com" = "Ação envolve comunidades locais",
                    "part_int_func" = "Ação envolve funcionários da empresa")
        
        data_emp_7 <- emp_binario %>%
          select(id_iniciativa, starts_with("part_")) %>% 
          distinct() %>%  
          summarise(across(-which(names(.) == "id_iniciativa"), sum)) %>%  
          pivot_longer(cols = everything(), names_to = "pti", values_to = "acoes") %>%
          mutate(relativo = (acoes / sum(acoes)) * 100) %>%
          mutate(pti = recode(pti, !!!labels)) %>%
          mutate(pti = fct_reorder(pti, -acoes, .fun = sum)) 
        
        create_plotly(data_emp_7, "pti", "Ações de Empresas por Partes Interessadas", cores_fjles)
      })

      # Criando gráfico de barras - Ações de Empresa por Ano de Financiamento 
        output$emp_8 <- renderPlotly({
          
          labels <- c("ano20" = "2020",
                      "ano21" = "2021",
                      "ano22" = "2022",
                      "ano23" = "2023")
          
          data_emp_8 <- emp_binario %>%
            select(id_iniciativa, starts_with("ano")) %>% 
            mutate(across(-id_iniciativa, ~suppressWarnings(as.numeric(.)))) %>%
            distinct() %>%  
            summarise(across(-id_iniciativa, sum, na.rm = TRUE)) %>%
            pivot_longer(cols = everything(), names_to = "ano", values_to = "acoes") %>%
            mutate(
              relativo = (acoes / sum(acoes, na.rm = TRUE)) * 100,
              ano = recode(ano, !!!labels),
              ano = fct_reorder(ano, -acoes)
            )
          
          create_plotly(data_emp_8, "ano", "Ações de Empresas por Ano de Financiamento", cores_fjles)
          
        })
        
      # Criando gráfico de barras - Ações de Empresa por Grupos Marginalizados
        output$emp_9 <- renderPlotly({
          
          labels <- c("gru_agri_fam" = "Ação envolveu agricultores familiares",
                      "gru_dem_esp" = "Ação envolveu grupos demográficos específicos",
                      "gru_vul_eco" = "Ação envolveu grupos vulneráveis",
                      "gru_cri_ado" = "Ação envolveu grupos crianças e adolescentes")
          
          data_emp_9 <- emp_binario %>%
            select(id_iniciativa, starts_with("gru_")) %>% 
            distinct() %>%  
            summarise(across(-which(names(.) == "id_iniciativa"), sum)) %>%
            pivot_longer(cols = everything(), names_to = "gru", values_to = "acoes") %>%
            mutate(relativo = (acoes / sum(acoes)) * 100) %>%
            mutate(gru = recode(gru, !!!labels)) %>%
            mutate(gru = fct_reorder(gru, -acoes, .fun = sum)) 
          
          create_plotly(data_emp_9, "gru", "Ações de Empresas por Grupos Marginalizados", cores_fjles)
          
        })
          
    # Renderização dinâmica dos gráficos de Ações de Fundações
    output$dynamic_plot_fund <- renderUI({
      if (input$grafico_fund == "Tipo") {
        plotlyOutput("fund_1")
      } else if (input$grafico_fund  == "Mecanismo") {
        plotlyOutput("fund_2")
      } else if (input$grafico_fund  == "Objetivo") {
        plotlyOutput("fund_3")
      } else if (input$grafico_fund  == "ODS") {
        plotlyOutput("fund_4")
      } else if (input$grafico_fund  == "Elo da Cadeia do Alimento") {
        plotlyOutput("fund_5")
      } else if (input$grafico_fund  == "Ano de Financiamento") {
        plotlyOutput("fund_6")
      } 
    })
    
    # Renderização dinâmica dos gráficos de Ações de Fundações
    output$dynamic_plot_perfil <- renderUI({
      if (input$grafico_perfil == "Por Setor") {
        plotlyOutput("perfil_1")
      } else if (input$grafico_perfil  == "Que possuem fundações, por Setor") {
        plotlyOutput("perfil_2")
      } else if (input$grafico_perfil  == "Por Atuação nos Elos da Cadeia") {
        plotlyOutput("perfil_3")
      } else if (input$grafico_perfil  == "Por Priorização da Segurança Alimentar por Ano") {
        plotlyOutput("perfil_4")
      }
    })
  
  # Renderização dinâmica dos gráficos de Ações de Empresa
    output$dynamic_plot_emp <- renderUI({
      if (input$grafico_emp == "Tipo") {
        plotlyOutput("emp_1")
      } else if (input$grafico_emp == "Mecanismo") {
        plotlyOutput("emp_2")
      } else if (input$grafico_emp == "Objetivo") {
        plotlyOutput("emp_3")
      } else if (input$grafico_emp == "ODS") {
        plotlyOutput("emp_4")
      } else if (input$grafico_emp == "Elo da Cadeia do Alimento") {
        plotlyOutput("emp_5")
      } else if (input$grafico_emp == "ESG") {
        plotlyOutput("emp_6")
      } else if (input$grafico_emp == "Partes Interessadas") {
        plotlyOutput("emp_7")
      } else if (input$grafico_emp == "Ano de Financiamento") {
        plotlyOutput("emp_8")
      } else if (input$grafico_emp == "Grupos Marginalizados") {
        plotlyOutput("emp_9")
      }
    })
    
    output$cru_2 <- renderPlotly({

      grafico_stack(cru_stack1, "set_ativ", "n", "name", "p",
                    "Ações de Empresas por Setor e ESG", "total_setor", cores_fjles)
        
    })
    
    output$cru_3 <- renderPlotly({
      
      grafico_stack(cru_stack2, "set_ativ", "n", "name", "p_setor",
                    "Ações de Empresas por Setor e Grupos Marginalizados","total_setor", cores_fjles)
      
    })
}
