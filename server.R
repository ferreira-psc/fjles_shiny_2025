# SCRIPT DE DEFINIÇÃO DO SERVIDOR
server <- function(input, output, session) {

#### FUNÇÕES ####
  
  create_plotly <- function(data_obj, x_var, y_var, title, colors, total) {
    
    # Criando ggplot
    plot_obj <- ggplot(data_obj, aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[x_var]])) +
      geom_bar(stat = "identity") +
      geom_text(aes(y = .data[[y_var]] + (max(.data[[y_var]], na.rm = TRUE) * 0.05), 
                    label = paste0(.data[[y_var]], "\n", round(.data[[y_var]] / total * 100, 2), "%")), 
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
    render_custom_table <- function(id, data, colunas_sem_filtro = NULL) {
      output[[paste0("table_", id)]] <- renderDT({
        # Formata longos textos com botão [+]
        data_mod <- data %>%
          mutate(across(everything(), ~ ifelse(
            nchar(.) > 50,
            paste0(
              "<div class='truncate-cell'>", ., "</div>",
              "<span class='expand-button' onclick='toggleExpand(this)'>[+]</span>"
            ),
            .
          )))
        
        # Identifica índices das colunas a remover filtro (JavaScript usa zero-based)
        col_indices_to_remove <- which(names(data_mod) %in% colunas_sem_filtro) - 1
        
        datatable(
          data_mod,
          rownames = FALSE,
          escape = FALSE,  # evita erro se alguma coluna não existir
          options = list(
            autoWidth = TRUE,
            columnDefs = list(list(width = 'auto', targets = "_all")),
            scrollX = TRUE,
            scrollY = "300px",
            paging = FALSE,
            searching = TRUE,
            initComplete = JS(sprintf(
              "
  function(settings, json) {
    var table = this.api();
    var colunasParaRemover = [%s];

    // Remove filtros das colunas especificadas
            colunasParaRemover.forEach(function(index) {
              $('thead tr:eq(1) th:eq(' + index + ') input').remove();
              $('thead tr:eq(1) th:eq(' + index + ')').html('');
            });

            // Substitui inputs restantes por selects
            table.columns().every(function() {
              var column = this;
              var idx = column.index();

              if (colunasParaRemover.includes(idx)) return;

      var select = $('<select><option value=\"\">' + column.header().textContent + '</option></select>')
        .on('change', function() {
          var val = $.fn.dataTable.util.escapeRegex($(this).val());
          column.search(val, true, false).draw();
        });

      column.data().unique().sort().each(function(d, j) {
        select.append('<option value=\"' + d + '\">' + d + '</option>');
      });

      var th = $(column.header());
      th.html('');
      th.append(select);
    });

    setTimeout(function() {
      table.columns.adjust().draw();
    }, 100);
  }
  ",
              paste(col_indices_to_remove, collapse = ", ")
            ))
            
          )
        )
      })
    }
    
      render_table_codebook <- function(id, data) {
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
                      dom = 't',
                      rowCallback = JS(
                        "function(row, data, index) {
      var api = this.api();
      var cell = $('td:eq(0)', row); // Encontra a primeira coluna

      if (index > 0) {
        var prevCell = null;

        // Encontra a primeira ocorrência válida da categoria
        for (var i = index - 1; i >= 0; i--) {
          var tempCell = $('td:eq(0)', api.row(i).node());
          if (tempCell.length > 0 && tempCell.text() === cell.text()) {
            prevCell = tempCell;
            break;
          }
        }

        // Se encontrou uma célula válida, aumenta o rowspan e remove a duplicata
        if (prevCell !== null) {
          var rowspan = prevCell.attr('rowspan') || 1;
          prevCell.attr('rowspan', parseInt(rowspan) + 1);
          cell.remove(); // Remove a célula duplicada
        }
      }
    }"
                      )
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
      grafico_stack <- function(data, x1_var, y_var, fill_var, percent_var, title, total_var, p_var, colors) {
        
        # Ordenando as colunas de forma decrescente com base na soma de y_var
        data[[x1_var]] <- factor(data[[x1_var]], levels = data %>%
                                   group_by_at(x1_var) %>%
                                   summarise(sum_y = sum(!!sym(y_var), na.rm = TRUE)) %>%
                                   arrange(-sum_y) %>%
                                   pull(!!sym(x1_var)))
        
        plot <- ggplot(data, aes_string(x = x1_var, y = y_var, fill = fill_var)) +
          geom_bar(stat = "identity", position = "stack") +
          geom_text(aes(label = scales::percent(!!sym(percent_var), accuracy = 0.01)), 
                    position = position_stack(vjust = 0.5), size = 3) +  
          geom_text(aes(y = !!sym(total_var) + (max(.data[[total_var]], na.rm = TRUE) * 0.08), 
                        label = paste0(
                          !!sym(total_var))),
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
      render_custom_table("fund", fund_banco, c("iniciativa", "descricao", "estado_atua"))
    
    # Criando botões de download do banco de dados Ações de Fundações
      create_csv_download("fund", fund_binario, "fund_acoes.csv") #CSV
      create_xlsx_download("fund", fund_binario, "fund_acoes.xlsx") #XLSX 
    
    # Criando botão de abertura do codebook de Ações de Fundações
      create_toggle_button("fund_code", session)
    
    # Criando codebook de Ações de Fundações
      render_table_codebook("fund_code", fund_codebook)
      
    # Criando botão de download do codebook de Ações de Fundações
      create_xlsx_download("fund_code", fund_codebook, "fund_codebook.xlsx")
    
    # Criando banco de dados de Perfil de Empresas
    render_custom_table("perfil", perfil_banco, c("cnae", "nome_fund_insti",
                                                  "assoc_pactos", "certific",
                                                  "ano_gee", "desc_certific", 
                                                  "empresa"))
    
    # Criando botões de download do banco de dados Perfil de Empresas  
    create_csv_download("perfil", perfil_binario, "perfil_banco.csv") #CSV
    create_xlsx_download("perfil", perfil_binario, "perfil_banco.xlsx") #XLSX
    
    # Criando botão de abertura do codebook de Perfil de Empresas 
    create_toggle_button("perfil_code", session)
    
    # Criando codebook de Perfil de Empresas
    render_table_codebook("perfil_code", perfil_codebook)
    
    # Criando botões de download do codebook de Perfil de Empresas 
    create_xlsx_download("perfil_code", perfil_codebook, "perfil_codebook.xlsx")
      
    # Criando banco de dados de Ações de Empresas
    render_custom_table("emp", emp_banco, c("iniciativa", "descricao",
                                            "estado_atua", "parceiro", "org_exec", "tipo_exec"))
    
    # Criando botões de download do banco de dados Ações de Empresas  
    create_csv_download("emp", emp_binario, "emp_acoes.csv") #CSV
    create_xlsx_download("emp", emp_binario, "emp_acoes.xlsx") #XLSX
    
    # Criando botão de abertura do codebook de Ações de Empresas 
    create_toggle_button("emp_code", session)
    
    # Criando codebook de Ações de Empresas
    render_table_codebook("emp_code", emp_codebook)
    
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
      
      create_plotly(data_fund_1, "tipo", "acoes",  "Ações por tipo (n = 331)", cores_fjles, 331)
      
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
      
      create_plotly(data_fund_2, "mecanismo", "acoes","Ações por Mecanismo (n = 331)", cores_fjles_expandida, 331)
      
    })
    
    # Criando gráfico de barras - Ações de Fundações por Objetivo
    output$fund_3 <- renderPlotly({
      
      data_fund_3 <- fund_binario %>%
        select(id_ini, starts_with("obj_")) %>%  
        distinct() %>% 
        pivot_longer(cols = starts_with("obj_"), values_to = "objetivos") %>%
        select(-name) %>%   
        filter(!is.na(objetivos) & objetivos != "n/a") %>%  
        count(objetivos, name = "acoes") %>% 
        mutate(
          relativo = (acoes / sum(acoes)) * 100,
          objetivos = fct_reorder(objetivos, acoes, .desc = TRUE)
        )
      
      create_plotly(data_fund_3, "objetivos", "acoes", "Ações por objetivo (n = 331)", cores_fjles_expandida, 331)
      
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
        group_by(id_ini) %>%
        summarise(across(everything(), ~as.numeric(any(. == 1, na.rm = TRUE)))) %>%
        ungroup() %>%
        summarise(across(-id_ini, sum, na.rm = TRUE)) %>% 
        pivot_longer(cols = everything(), names_to = "ods", values_to = "acoes") %>%
        mutate(relativo = (acoes / sum(acoes)) * 100) %>%
        mutate(ods = recode(ods, !!!labels)) %>%
        mutate(ods = fct_reorder(ods, -acoes, .fun = sum)) 
      
      create_plotly(data_fund_4, "ods","acoes", "Ações por ODS (n = 331)", cores_fjles_expandida, 331)
      
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
        group_by(id_ini) %>%
        summarise(across(everything(), ~as.numeric(any(. == 1, na.rm = TRUE)))) %>%
        ungroup() %>%
        summarise(across(-id_ini, sum, na.rm = TRUE)) %>%  
        pivot_longer(cols = everything(), names_to = "elo", values_to = "acoes") %>%
        mutate(relativo = (acoes / sum(acoes)) * 100) %>%
        mutate(elo = recode(elo, !!!labels)) %>%
        mutate(elo = fct_reorder(elo, -acoes, .fun = sum)) 
      
      create_plotly(data_fund_5, "elo","acoes", "Ações por elo da cadeia do alimento (n = 331)", cores_fjles_expandida, 331)
      
    })
    
    # Criando gráfico de barras - Ações de Fundações por Ano de Financiamento
    output$fund_6 <- renderPlotly({
      
      labels <- c("ano20" = "2020",
                  "ano21" = "2021",
                  "ano22" = "2022")
      
      data_fund_6 <- fund_binario %>%
        select(id_ini, starts_with("ano")) %>%
        mutate(across(everything(), ~ suppressWarnings(as.numeric(as.character(.))))) %>% 
        group_by(id_ini) %>%
        summarise(across(everything(), ~as.numeric(any(. == 1, na.rm = TRUE)))) %>%
        ungroup() %>%
        summarise(across(-id_ini, sum, na.rm = TRUE)) %>%  
        pivot_longer(cols = everything(), names_to = "ano", values_to = "acoes") %>%
        mutate(relativo = (acoes / sum(acoes, na.rm = TRUE)) * 100) %>%
        mutate(ano = recode(ano, !!!labels)) 
      
      create_plotly(data_fund_6, "ano","acoes", "Ações por Ano de financiamento (n = 331)", cores_fjles, 331)
      
    })
    
    # Criando gráfico de barras - Empresas por Setor
    output$perfil_1 <- renderPlotly({
      
      data_perfil_1 <- perfil_binario %>%
        count(set_ativ) %>%
        rename(acoes = n) %>% 
        mutate(relativo = (acoes / sum(acoes)) * 100) 
      data_perfil_1$set_ativ <- factor(data_perfil_1$set_ativ, 
                                 levels = data_perfil_1$set_ativ[order(data_perfil_1$acoes, decreasing = TRUE)])
      
      create_plotly(data_perfil_1, "set_ativ","acoes", "Empresas por setor (n = 98)", cores_fjles, 98)
      
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
      
      create_plotly(data_perfil_2, "set_ativ","acoes", "Empresas que possuem fundações por setor (n = 36)", cores_fjles, 36)
      
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
      
      create_plotly(data_perfil_3, "elo","acoes", "Empresas por atuação nos elos da cadeia (n = 98)", cores_fjles_expandida, 98)
      
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
      
      create_plotly(data_perfil_4, "ano","acoes", "Empresas por priorização da segurança alimentar por ano (n = 98)", cores_fjles_expandida, 98)
      
    })
    
    # Criando gráfico de barras - Empresas por Priorização da Segurança Alimentar por Ano
    output$perfil_5 <- renderPlotly({
      
      data_perfil_5 <- cru_bar %>%
        mutate(set_ativ = fct_reorder(set_ativ, -n_cert))
      
      create_plotly(data_perfil_5, "set_ativ","n_cert", "Certificações por setor (n = 558)", cores_fjles, 558)
      
    })

  # Criando gráfico de barras - Ações de Empresas por Tipo
    output$emp_1 <- renderPlotly({
        
      data_emp_1 <- emp_binario %>%
        distinct(id_ini, tipo) %>% 
        count(tipo) %>%
        rename(acoes = n) %>% 
        mutate(relativo = (acoes / sum(acoes)) * 100) 
      data_emp_1$tipo <- factor( data_emp_1$tipo,
                                 levels =  data_emp_1$tipo[order(data_emp_1$acoes,
                                                                 decreasing = TRUE)])
        
        create_plotly(data_emp_1, "tipo","acoes", "Ações por tipo (n = 681)", cores_fjles, 681)
        
      })
    
  # Criando gráfico de barras - Ações de Empresas por Mecanismo
    output$emp_2 <- renderPlotly({
        
      data_emp_2 <- emp_binario %>% 
          distinct(id_ini, mecanismo) %>% 
          count(mecanismo) %>%
          rename(acoes = n) %>% 
          mutate(relativo = (acoes / sum(acoes)) * 100) 
        data_emp_2$mecanismo <- factor(data_emp_2$mecanismo, 
                                                      levels = data_emp_2$mecanismo[order(data_emp_2$acoes,
                                                                                          decreasing = TRUE)])
        
        create_plotly(data_emp_2, "mecanismo","acoes", "Ações por mecanismo (n = 681)", cores_fjles_expandida, 681)
        
      })
    
  # Criando gráfico de barras - Ações de Empresas por Objetivo
    output$emp_3 <- renderPlotly({
      
      labels<- c(
        "obj_prod_ali" = "Produção de alimentos",
        "obj_seg_nutri" = "Segurança nutricional",
        "obj_ali_fome" = "Alívio da fome",
        "obj_red_perda" = "Redução de perdas",
        "obj_reap" = "Reaproveitamento",
        "obj_ace_agua" = "Acesso à água",
        "obj_res_emp" = "Melhores práticas de responsabilidade empresariais",
        "obj_agri_fam" = "Fortalecimento da agricultura familiar",
        "obj_red_desp" = "Redução de desperdícios"
      )
      
      data_emp_3 <- emp_binario %>%
        select(id_ini, starts_with("obj_")) %>%
        group_by(id_ini) %>%
        summarise(across(everything(), ~as.numeric(any(. == 1, na.rm = TRUE)))) %>%
        ungroup() %>%
        summarise(across(-id_ini, sum, na.rm = TRUE)) %>%  
        pivot_longer(cols = everything(), names_to = "objetivo", values_to = "acoes") %>%
        mutate(relativo = (acoes / sum(acoes)) * 100) %>%
        mutate(objetivo = recode(objetivo, !!!labels)) %>%
        mutate(objetivo = fct_reorder(objetivo, -acoes, .fun = sum)) 
      
      create_plotly(data_emp_3, "objetivo","acoes", "Ações por objetivo (n = 681)", cores_fjles_expandida, 681) 
      
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
          select(id_ini, starts_with("ods_")) %>% 
          group_by(id_ini) %>%
          summarise(across(everything(), ~as.numeric(any(. == 1, na.rm = TRUE)))) %>%
          ungroup() %>%
          summarise(across(-id_ini, sum, na.rm = TRUE)) %>% 
          pivot_longer(cols = everything(), names_to = "ods", values_to = "acoes") %>%
          mutate(relativo = (acoes / sum(acoes)) * 100) %>%
          mutate(ods = recode(ods, !!!labels)) %>%
          mutate(ods = fct_reorder(ods, -acoes, .fun = sum)) 
        
        create_plotly(data_emp_4, "ods", "acoes", "Ações por ODS (n = 681)", cores_fjles_expandida, 681) 
        
      })
      
    # Criando gráfico de barras - Ações de Empresas por Elo da Cadeia do Alimento
      output$emp_5 <- renderPlotly({
      
        labels <- c("elo_prod_ali" = "Produção de alimentos",
                    "elo_armaz" = "Armazenamento",
                    "elo_trans_log" = "Transporte",
                    "elo_process" = "Processamento",
                    "elo_var_atac"= "Varejo/Atacado",
                    "elo_consu" = "Consumo")
        
        data_emp_5 <- emp_binario %>%
          select(id_ini, starts_with("elo_")) %>% 
          group_by(id_ini) %>%
          summarise(across(everything(), ~as.numeric(any(. == 1, na.rm = TRUE)))) %>%
          ungroup() %>%
          summarise(across(-id_ini, sum, na.rm = TRUE)) %>% 
          pivot_longer(cols = everything(), names_to = "elo", values_to = "acoes") %>%
          mutate(relativo = (acoes / sum(acoes)) * 100) %>%
          mutate(elo = recode(elo, !!!labels)) %>%
          mutate(elo = fct_reorder(elo, -acoes, .fun = sum)) 
        
        create_plotly(data_emp_5, "elo","acoes", "Ações por elo da cadeia do alimento (n = 681)", cores_fjles_expandida, 681)
        
      })
      
    # Criando gráfico de barras - Ações de Empresas por ESG
      output$emp_6 <- renderPlotly({
        
        labels <- c("social" = "Benefício social",
                    "ambiental" = "Benefício ambiental",
                    "governanca" = "Benefício de governança da empresa")
        
        data_emp_6 <- emp_binario %>%
          select(id_ini, social, ambiental, governanca) %>%  
          group_by(id_ini) %>%
          summarise(across(everything(), ~as.numeric(any(. == 1, na.rm = TRUE)))) %>%
          ungroup() %>%
          summarise(across(-id_ini, sum, na.rm = TRUE)) %>% 
          pivot_longer(cols = everything(), names_to = "esg", values_to = "acoes") %>%
          mutate(relativo = (acoes / sum(acoes)) * 100) %>%
          mutate(esg = recode(esg, !!!labels)) %>%
          mutate(esg = fct_reorder(esg, -acoes, .fun = sum)) 
        
        create_plotly(data_emp_6, "esg","acoes", "Ações por ESG (n = 681)", cores_fjles, 681)
        
      })
      
      # Criando gráfico de barras - Ações de Empresas por Partes Interessadas
      output$emp_7 <- renderPlotly({
        
        labels <- c("part_int_cli" = "Clientes da empresa",
                    "part_int_for" = "Fornecedores",
                    "part_int_com" = "Comunidades locais",
                    "part_int_func" = "Funcionários da empresa")
        
        data_emp_7 <- emp_binario %>%
          select(id_ini, starts_with("part_")) %>% 
          group_by(id_ini) %>%
          summarise(across(everything(), ~as.numeric(any(. == 1, na.rm = TRUE)))) %>%
          ungroup() %>%
          summarise(across(-id_ini, sum, na.rm = TRUE)) %>%   
          pivot_longer(cols = everything(), names_to = "pti", values_to = "acoes") %>%
          mutate(relativo = (acoes / sum(acoes)) * 100) %>%
          mutate(pti = recode(pti, !!!labels)) %>%
          mutate(pti = fct_reorder(pti, -acoes, .fun = sum)) 
        
        create_plotly(data_emp_7, "pti","acoes", "Ações de empresas por partes interessadas (n = 681)", cores_fjles, 681)
      })

      # Criando gráfico de barras - Ações de Empresa por Ano de Financiamento 
        output$emp_8 <- renderPlotly({
          
          labels <- c("ano20" = "2020",
                      "ano21" = "2021",
                      "ano22" = "2022",
                      "ano23" = "2023")
          
          data_emp_8 <- emp_binario %>%
            select(id_ini, starts_with("ano")) %>%
            mutate(across(-id_ini, ~suppressWarnings(as.numeric(.)))) %>%
            group_by(id_ini) %>%
            summarise(across(everything(), ~as.numeric(any(. == 1, na.rm = TRUE)))) %>%
            ungroup() %>%
            summarise(across(-id_ini, sum, na.rm = TRUE))%>%
            pivot_longer(cols = everything(), names_to = "ano", values_to = "acoes") %>%
            mutate(
              relativo = (acoes / sum(acoes, na.rm = TRUE)) * 100,
              ano = recode(ano, !!!labels)
            )
          
          create_plotly(data_emp_8, "ano","acoes", "Ações por ano de financiamento (n = 681)", cores_fjles, 681)
          
        })
        
      # Criando gráfico de barras - Ações de Empresa por Grupos Marginalizados
        output$emp_9 <- renderPlotly({
          
          labels <- c("gru_agri_fam" = "Agricultores familiares",
                      "gru_dem_esp" = "Grupos demográficos específicos (ex: mulheres, quilombolas, indígenas, etc.)",
                      "gru_vul_eco" = "Grupos em situação de vulnerabilidade econômica",
                      "gru_cri_ado" = "Crianças e adolescentes")
          
          data_emp_9 <- emp_binario %>%
            select(id_ini, starts_with("gru_")) %>% 
            group_by(id_ini) %>%
            summarise(across(everything(), ~as.numeric(any(. == 1, na.rm = TRUE)))) %>%
            ungroup() %>%
            summarise(across(-id_ini, sum, na.rm = TRUE)) %>% 
            pivot_longer(cols = everything(), names_to = "gru", values_to = "acoes") %>%
            mutate(relativo = (acoes / sum(acoes)) * 100) %>%
            mutate(gru = recode(gru, !!!labels)) %>%
            mutate(gru = fct_reorder(gru, -acoes, .fun = sum)) 
          
          create_plotly(data_emp_9, "gru","acoes",
                        "Ações por grupos em maior risco de insegurança alimentar e nutricional (n = 681)",
                        cores_fjles, 681)
          
        })
        
        output$cru_1 <- renderPlotly({
          
          cru_stack3$elo <- factor(cru_stack3$elo, levels = c(
            "Produção de alimentos", "Armazenamento", "Transporte",
            "Processamento", "Varejo/Atacado", "Consumo"
          ))
          
          cru_stack3 <- cru_stack3 %>%
            mutate(set_ativ_lab = paste0(set_ativ, " (n = ", invest_setor, ")"))
          
          plot <- ggplot(cru_stack3, aes(x = elo, y = invest_elo, fill = elo)) +
            geom_bar(stat = "identity") +
            geom_text(aes(
              y = invest_elo + (max(invest_elo, na.rm = TRUE) * 0.08), 
              label = paste0(invest_elo, " (", scales::percent(p_elo_setor, accuracy = 0.01), ")")
            ),
            vjust = -1.2, size = 3) +
            scale_fill_manual(values = cores_fjles_expandida) +
            labs(title = "Investimentos por elo na cadeia do alimento por setor") +
            theme_minimal() +
            theme(
              plot.title = element_text(margin = margin(b = 100)),
              plot.margin = margin(t = 20),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.spacing = unit(1.5, "lines"),  
              panel.background = element_rect(fill = "#f2f2f2", color = NA),
              strip.background = element_rect(fill = "#f2f2f2", color = NA),
              strip.text = element_text(size = 11, face = "bold"),
              legend.position = "bottom"
            ) +
            guides(fill = guide_legend(title = NULL)) +
            facet_wrap(~set_ativ_lab, ncol = 1, scales = "free_y")
          
          plotly <- ggplotly(plot, tooltip = NULL, height = 750) %>%
            style(hoverinfo = "none") %>%
            config(
              modeBarButtonsToRemove = c("zoom2d", "zoomIn2d", "zoomOut2d", "autoScale2d",
                                         "pan2d", "select2d", "lasso2d", "resetScale2d",
                                         "hoverClosestCartesian", "hoverCompareCartesian", 
                                         "toggleSpikelines"),
              displaylogo = FALSE
            )
          
          return(plotly)
        })
        
        
        output$cru_2 <- renderPlotly({
          
          grafico_stack(cru_stack1, "set_ativ", "n", "name", "p",
                        "Ações por setor e ESG", "total_setor", "p_total", cores_fjles_claro)
          
        })
        
        output$cru_3 <- renderPlotly({
          
          grafico_stack(cru_stack2, "set_ativ", "n", "name", "p_setor",
                        "Ações por setor e por grupos em maior risco de insegurança alimentar e nutricional",
                        "total_setor", "p_total", cores_fjles_claro)
          
        })
        
        #Criando Box de informações - Total de ações de fundações
        output$total_fund <- renderValueBox({
          
          total_fund <- fund_binario %>%
            distinct(id_ini) %>%
            nrow()
          
          valueBox(
            value = total_fund,
            subtitle = "Ações de Fundações",
            icon = icon("list"),
            color = "aqua"
          )
        })
        
        #Criando Box de informações - Total de empresas
        output$total_perfil <- renderValueBox({
          
          total_perfil <- nrow(perfil_binario)
          
          valueBox(
            value = total_perfil,
            subtitle = "Empresas",
            icon = icon("list"),
            color = "aqua"
          )
        })

        #Criando Box de informações - Total ações de empresas
        output$total_emp <- renderValueBox({
          
          total_emp <- emp_binario %>%
            distinct(id_ini) %>%
            nrow()
          
          valueBox(
            value = total_emp,
            subtitle = "Ações de Empresas",
            icon = icon("list"),
            color = "aqua"
          )
        })
          
    # Renderização dinâmica dos gráficos de Ações de Fundações
    output$dynamic_plot_fund <- renderUI({
      if (input$grafico_fund == "Tipo") {
        tagList(plotlyOutput("fund_1"),
                em("*Os valores relativos foram calculados a partir do total 331 de ações analisadas."))
      } else if (input$grafico_fund  == "Mecanismo") {
        tagList(plotlyOutput("fund_2"),
                em("*Os valores relativos foram calculados a partir do total 331 de ações analisadas."))
      } else if (input$grafico_fund  == "Objetivo") {
        tagList(plotlyOutput("fund_3"),
                em("*Como cada ação poderia estar relacionada a mais de um objetivo,
                 podendo visar a produção de alimentos e o acesso à água,
                 por exemplo, a soma da quantidade de iniciativas por objetivo é superior ao total de 331 ações analisadas.
                   Os valores relativos foram calculados a partir do total de ações analisadas. "))
      } else if (input$grafico_fund  == "ODS") {
        tagList(plotlyOutput("fund_4"),
                em("*Como cada ação poderia estar relacionada a um ODS ou mais, a soma do número de ações considerando todos os indicadores ultrapassa o total de 331 ações analisadas.
                   Os valores relativos foram calculados a partir do total de ações analisadas. "))
      } else if (input$grafico_fund  == "Elo da cadeia do alimento") {
        tagList(plotlyOutput("fund_5"),
                em("*Cada ação poderia estar relacionada a nenhuma ou a mais de uma etapa da cadeia de alimentos.
                   Assim, a soma da quantidade de ações por elo não corresponde ao total de 331 ações analisadas.
                   Os valores relativos foram calculados a partir do total de ações analisadas. "))
      } else if (input$grafico_fund  == "Ano de financiamento") {
        tagList(plotlyOutput("fund_6"),
                em("*Cada ação pode ter recebido financiamento em um ou mais anos.
                   Por isso, a soma dos totais de financiamento não corresponde ao total de 331 ações analisadas. 
                   Os valores relativos foram calculados a partir do total de ações analisadas."))
      } 
    })
    
    # Renderização dinâmica dos gráficos de Ações de Perfil de Empresas
    output$dynamic_plot_perfil <- renderUI({
      if (input$grafico_perfil == "Por setor") {
        tagList(plotlyOutput("perfil_1"),
                em("*Os valores relativos foram calculados a partir do total de 98 empresas analisadas."))
      } else if (input$grafico_perfil  == "Que possuem fundações por setor") {
        tagList(plotlyOutput("perfil_2"),
                em("*Os valores do gráfico foram calculados em relação ao total de 36 empresas que possuem fundações ou institutos.
                   O número de fundações ou institutos é inferior ao total de empresas categorizadas como “possui instituto/fundação”, uma vez que as empresas Castolanda, Frísia e Capal possuem a mesma fundação (Fundação ABC).
                   Assim, o número total de fundações/institutos é de 34.
                   Os valores relativos foram calculados a partir do total de empresas que possuíam fundações ou institutos."))
      } else if (input$grafico_perfil  == "Por atuação nos elos da cadeia do alimento") {
        tagList(plotlyOutput("perfil_3"),
                em("*A soma do número de empresas atuantes em cada elo da cadeia do alimento ultrapassa o total de 98 empresas analisadas, uma vez que cada empresa poderia atuar em ao menos um elo e no máximo em todos.
                   Os valores relativos foram calculados a partir do total de empresas analisadas."))
      } else if (input$grafico_perfil  == "Por priorização da segurança alimentar por ano") {
        tagList(plotlyOutput("perfil_4"),
                em("*Cada empresa pode priorizar a segurança alimentar em um ou mais anos.
                   Por isso, a soma dos totais de priorizações não corresponde ao total de 98 empresas analisadas.
                   Os valores relativos foram calculados a partir do total de empresas analisadas."))
      }
      else if (input$grafico_perfil  == "Certificações por setor") {
        tagList(plotlyOutput("perfil_5"),
                em("*Cada empresa analisada poderia ter mais de um certificado ou nenhum.
                   O “n” foi calculado a partir do total de certificados por setor empresarial,
                   e os valores relativos foram calculados a partir desse total."))
      }
    })
    
  # Renderização dinâmica dos gráficos de Ações de Empresa
    output$dynamic_plot_emp <- renderUI({
      if (input$grafico_emp == "Tipo") {
        tagList(plotlyOutput("emp_1"),
                em("*Os valores relativos foram calculados a partir do total 681 de ações analisadas."))
      } else if (input$grafico_emp == "Mecanismo") {
        tagList(plotlyOutput("emp_2"),
                em("*Os valores relativos foram calculados a partir do total 681 de ações analisadas."))
      } else if (input$grafico_emp == "Objetivo") {
        tagList(plotlyOutput("emp_3"),
                em("*Como cada ação poderia estar relacionada a mais de um objetivo,
                 podendo visar a produção de alimentos e o acesso à água,
                 por exemplo, a soma da quantidade de iniciativas por objetivo é superior ao total de 681 ações analisadas. 
                Os valores relativos foram calculados a partir do total de ações analisadas."))
      } else if (input$grafico_emp == "ODS") {
        tagList(plotlyOutput("emp_4"),
                em("*Como cada ação poderia estar relacionada a um ODS ou mais, a soma do número de ações considerando todos os indicadores ultrapassa o total de 681 ações analisadas.
                   Os valores relativos foram calculados a partir do total de ações analisadas."))
      } else if (input$grafico_emp == "Elo da cadeia do alimento") {
        tagList(plotlyOutput("emp_5"),
                em("*Cada ação poderia estar relacionada a nenhuma ou a mais de uma etapa da cadeia de alimentos.
                   Assim, a soma da quantidade de ações por elo não corresponde ao total de 681 ações analisadas.
                   Os valores relativos foram calculados a partir do total de ações analisadas."))
      } else if (input$grafico_emp == "ESG") {
        tagList(plotlyOutput("emp_6"),
                em("*Cada iniciativa de SSAN analisada poderia gerar nenhum ou mais de um benefício ESG, ou seja, uma mesma ação poderia gerar um benefício social e ambiental ao mesmo tempo, por exemplo.
                   Por isso, a soma dos totais de cada elo ESG não corresponde ao total de 681 ações analisadas.
                   Os valores relativos foram calculados a partir do total de ações analisadas."))
      } else if (input$grafico_emp == "Partes interessadas") {
        tagList(plotlyOutput("emp_7"),
                em("*Cada ação poderia envolver, ter como público-alvo ou impactar mais de uma parte interessada ou nenhuma delas.
                   Portanto, a somatória do total de ações para cada variável difere do total de 681 ações analisadas.
                   Os valores relativos foram calculados a partir do total de ações analisadas."))
      } else if (input$grafico_emp == "Ano de financiamento") {
        tagList(plotlyOutput("emp_8"),
                em("*Cada ação pode ter recebido financiamento em um ou mais anos.
                   Por isso, a soma dos totais de financiamento não corresponde ao total de 681 ações analisadas.
                   Os valores relativos foram calculados a partir do total de ações analisadas."))
      } else if (input$grafico_emp == "Grupos em maior risco de insegurança alimentar e nutricional") {
        tagList(plotlyOutput("emp_9"),
                em("*Cada ação poderia envolver, ter como público-alvo ou impactar mais de um grupo ou nenhum deles. 
                   Os valores relativos foram calculados a partir do total de ações analisadas."))
      }
    })
    
    # Renderização dinâmica dos gráficos de Cruzamemtos
    output$dynamic_plot_cru <- renderUI({
      if (input$grafico_cru == "Investimentos por elo na cadeia do alimento por setor") {
        tagList(plotlyOutput("cru_1", height = "750px"),
                em("*Cada ação analisada poderia estar relacionada a um ou mais elos da cadeia do alimento, ou a nenhum deles.
                   O “n” total foi calculado a partir da soma de todas as ações relacionadas aos elos que foram financiadas ou apoiadas por empresas do respectivo setor.
                   As proporções consideraram o número de ações por elo da cadeia em relação ao número total de ações financiadas ou apoiadas pelo setor."))
      } else if (input$grafico_cru == "Ações por setor e ESG") {
        tagList(plotlyOutput("cru_2"),
                em("*Cada ação analisada poderia estar relacionada a um ou mais benefícios do ESG, ou até mesmo a nenhum.
                   O “n” total foi calculado a partir da soma de todas as ações que tinham benefícios ESG e foram financiadas ou apoiadas por empresas do respectivo setor.
                   Cada ação foi contabilizada o número de vezes equivalente à quantidade de benefícios que gerou.
                   Ex: se uma ação teve benefício ambiental e social, ela foi contabilizada duas vezes. Se ela gerou os três benefícios, equivaleu a três."))
      } else if (input$grafico_cru ==  "Ações por setor e por grupos em maior risco de insegurança alimentar e nutricional") {
        tagList(plotlyOutput("cru_3"),
                em("*Cada ação analisada poderia estar relacionada a um ou mais grupos, ou a nenhum deles.
                   O “n” total foi calculado a partir da soma de todas as ações relacionadas aos grupos que foram financiadas ou apoiadas por empresas do respectivo setor. Cada ação foi contabilizada o número de vezes equivalente à quantidade de grupos envolvidos ou impactados.
                   Ex: se uma ação envolveu agricultores familiares e crianças e adolescentes, ela foi contabilizada duas vezes.
                   Se ela envolveu agricultores familiares, crianças e adolescentes, e grupos demográficos específicos, equivaleu a três."))
      } 
    })
}
