# É esperado que este script sirva para todo o processo de preprossessamento dos dados utilizados
# na elaboração do dashboard do script "shinyApp.R". 
# Você pode rodar o código clicando em "Source" . 

#### PACOTES ####
if (!require('pacman')) install.packages('pacman'); library('pacman')
p_load( readxl, tidyverse, readr, arrow, here, data.table,
        rio, janitor, bslib, geobr, sf, stringr, openxlsx2,
        stringr, stringdist,  writexl)


#### EMPRESAS ####

# Importação dos dados de empresas (Ações e Perfil)
data <-import_list(here("data/dados_empresas.xlsx"))

for (obj in names(data)) {
  cleaned_name <- janitor::make_clean_names(obj, case = "snake")
  assign(cleaned_name, data[[obj]])
}

#Remoção da coluna de exemplo do codebook (desnecessária para a visualização)
emp_codebook <- codebook_1 %>% select(-4)
perfil_codebook <- codebook_2 %>% select(-4)

# Remoção de colunas desnecessárias
emp_banco <- acoes  %>%
  select(- c(5, 31, 32, 80, 81, 82,
             83, 84, 85, 86, 87, 88, 89, 90))

perfil_banco <- categorias_empresas %>% 
  select(- c(41, 42, 43, 44, 45, 46, 47))

# Transformação de variáveis binárias
emp_binario <- emp_banco %>% 
  mutate(across(everything(), ~ ifelse(. %in% c("Sim", "Não"), 
                                       ifelse(. == "Sim", 1, 0), 
                                       .)))

perfil_binario <- perfil_banco %>% 
  mutate(across(everything(), ~ ifelse(. %in% c("Sim", "Não"), 
                                       ifelse(. == "Sim", 1, 0), 
                                       .)))

# Dicionário de códigos regionais
dicionario_regioes <- data.frame(
  name_state = c("AC", "AP", "AM", "PA", "RO", "RR", "TO",
             "AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE",
             "DF", "GO", "MT", "MS",
             "ES", "MG", "RJ", "SP",
             "PR", "RS", "SC"),
  estado_atua = c(
    "Acre", "Amapá", "Amazônas", "Pará", "Rondônia", "Roraima", "Tocantins",
    "Alagoas", "Bahia", "Ceará", "Maranhão", "Paraíba", "Pernambuco", "Piauí", "Rio Grande Do Norte", "Sergipe",
    "Distrito Federal", "Goiás", "Mato Grosso", "Mato Grosso Do Sul",
    "Espírito Santo", "Minas Gerais", "Rio De Janeiro", "São Paulo",
    "Paraná", "Rio Grande Do Sul", "Santa Catarina"
  ),
  name_region = c(rep("Norte", 7), rep("Nordeste", 9), rep("Centro Oeste", 4),
             rep("Sudeste", 4), rep("Sul", 3))
)

# Transformação para formato longo 
emp_mapa <- emp_binario  %>% 
  separate_rows(estado_atua , sep = ";") %>%
  mutate(estado_atua = str_replace_all(estado_atua, "[^[:alnum:]]", "")) %>%
  rename( "name_state" = "estado_atua") %>% 
  filter(!name_state %in% c("si", "na", ""))%>% 
  left_join(dicionario_regioes, by = "name_state")

# Resumo das ODS_2 e dos objetivos em uma só coluna 
ods2 <- emp_mapa %>%
  pivot_longer(cols = starts_with("ods_2"), names_to = "ods2", values_to = "valor") %>%
  filter(valor == 1) %>%
  group_by(id_emp, id_iniciativa, estado_atua) %>%
  summarise(ods2 = str_c(ods2, collapse = ", "), .groups = "drop")

objetivos <- emp_mapa %>%  
  pivot_longer(cols = starts_with("obj_"), names_to = "objetivos", values_to = "valor") %>%
  filter(valor == 1) %>%
  mutate(objetivos = recode(objetivos,
                            "obj_prod_ali" = "Produção de Alimentos",
                            "obj_seg_nutri" = "Segurança Nutricional",
                            "obj_ali_fome" = "Alívio da Fome",
                            "obj_red_perda" = "Redução de Perdas",
                            "obj_reap" = "Reaproveitamento",
                            "obj_ace_agua" = "Acesso à água",
                            "obj_res_emp" = "Melhores Práticas de Responsabilidade Empresariais",
                            "obj_agri_fam" = "Fortalecimento da Agricultura Familiar",
                            "obj_red_desp" = "Redução de Desperdícios")) %>%
  group_by(id_emp, id_iniciativa, estado_atua) %>%
  summarise(objetivos = str_c(objetivos, collapse = ", "), .groups = "drop")

# União das duas bases de transformação e seleção das colunas usadas no Mapa
emp_mapa <- emp_mapa %>%
  left_join(ods2, by = c("id_emp", "id_iniciativa", "estado_atua")) %>%
  left_join(objetivos, by = c("id_emp", "id_iniciativa", "estado_atua")) %>%
  select(empresa, tipo, iniciativa, ods2, estado_atua, name_region, mecanismo, objetivos)

#### FUNDAÇÕES ####

# Importação dos dados de Fundações
data <-import_list(here("data/dados_fundacoes.xlsx"))

for (obj in names(data)) {
  cleaned_name <- janitor::make_clean_names(obj, case = "snake")
  assign(cleaned_name, data[[obj]])
}

#Remoção da coluna de exemplo do codebook (desnecessária para a visualização)
fund_codebook <- codebook %>% select(-4)

# Remoção de colunas desnecessárias
fund_banco <- banco_completo %>%
  select(- c(1, 11, 16, 19, 20, 21, 22, 23,
             24, 25, 50, 51, 52, 53, 54, 55))

# Transformação de variáveis binárias
fund_binario <- fund_banco %>% 
  mutate(across(everything(), ~ ifelse(. %in% c("Sim", "Não"), 
                                       ifelse(. == "Sim", 1, 0), 
                                       .)))

# Transformação para formato longo 
fund_mapa <- fund_binario%>%
  separate_rows(estado_atua, sep = ";") %>% 
  mutate(estado_atua = gsub("^\\s+", "", estado_atua)) %>% 
  filter(!estado_atua %in% c("s/i", "na", ""))%>%
  mutate(estado_atua = recode(estado_atua, #Correção de erros na base
                                  "Amazonas" = "Amazônas",
                                  "Rio Grade do Sul" = "Rio Grande do Sul"))


#### CRUZAMENTOS ####

#Importação dos dados de cruzamentos
data <-import_list(here("data/dados_cruzamentos.xlsx"))

for (obj in names(data)) {
  cleaned_name <- janitor::make_clean_names(obj, case = "snake")
  assign(cleaned_name, data[[obj]])
}

labels_gru <- c("gru_agri_fam" = "Ação envolveu agricultores familiares",
            "gru_dem_esp" = "Ação envolveu grupos demográficos específicos",
            "gru_vul_eco" = "Ação envolveu grupos vulneráveis",
            "gru_cri_ado" = "Ação envolveu grupos crianças e adolescentes")

setor_grupos <- setor_grupos %>%
  group_by(set_ativ) %>%
  mutate(total_setor = sum(n)) %>%
  ungroup()%>% 
  mutate(name = recode(name, !!!labels_gru)) 

labels_esg <- c("social" = "Benefício Social",
                "ambiental" = "Benefício Ambiental",
                "governanca" = "Benefício de Governança da Empresa")

setor_esg <- setor_esg %>%
  group_by(set_ativ) %>%
  mutate(total_setor = sum(n)) %>%
  ungroup()%>% 
  mutate(name = recode(name, !!!labels_esg)) 

#### DADOS + GEOMETRIA ####

# Dados espaciais regionais (GEOBR 2020) e cálculo de centróides
regioes_data <- read_region(year = 2020, showProgress = FALSE) %>% 
  mutate(lng = st_coordinates(st_centroid(geom))[,1],
         lat = st_coordinates(st_centroid(geom))[,2]) 

# Dados espaciais estaduais (GEOBR 2020) e cálculo de centróides
estados_data <- read_state(year = 2020, showProgress = FALSE)%>% 
  mutate(lng = st_coordinates(st_centroid(geom))[,1],
         lat = st_coordinates(st_centroid(geom))[,2])


estados_data <- estados_data %>%
  mutate(name_state_norm = stri_trans_general(name_state, "Latin-ASCII") %>%
           tolower())

fund_estado <- fund_mapa %>%
  group_by(estado_atua) %>%
  summarise(contagem = n(), .groups = "drop") %>%
  mutate(name_state_norm = stri_trans_general(estado_atua, "Latin-ASCII") %>%
           tolower()) %>%
  left_join(estados_data, by = "name_state_norm") %>% 
  select(-name_state_norm) %>%  
  st_as_sf() %>%
  st_transform(crs = 4326)

fund_regiao <- fund_mapa %>%
  group_by(estado_atua) %>%
  summarise(contagem = n()) %>% 
  mutate(name_state_norm = stri_trans_general(estado_atua, "Latin-ASCII") %>%
           tolower()) %>%
  left_join(estados_data, by = "name_state_norm") %>% 
  select(-name_state_norm) %>%  
  group_by(name_region) %>%
  summarise(contagem = sum(contagem)) %>% 
  left_join(regioes_data, by = "name_region") %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326)

emp_regiao <- emp_mapa %>% 
  group_by(name_region) %>%
  summarise(contagem = n()) %>%
  left_join(regioes_data, by = "name_region") %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) 

emp_estado <- emp_mapa %>%
  group_by(estado_atua) %>%
  summarise(contagem = n()) %>% 
  rename("name_state" = "estado_atua") %>% 
  left_join(estados_data, by =  "name_state") %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326)

#### EXPORTAÇÃO PARA A PASTA DATA ####

#Exportando dados tratados
write_xlsx(list(fund_binario = fund_binario,
                fund_mapa = fund_mapa,
                fund_codebook = fund_codebook,
                fund_banco = fund_banco,
                emp_binario = emp_binario,
                emp_mapa = emp_mapa,
                emp_codebook = emp_codebook,
                emp_banco = emp_banco,
                perfil_binario = perfil_binario,
                perfil_codebook = perfil_codebook,
                perfil_banco = perfil_banco, 
                cru_stack1 = setor_esg,
                cru_stack2 = setor_grupos,
                cru_stack3 = certificados_setor,
                cru_sankey = elo_atua_investe),
           "data/dados_tratados.xlsx")

# Converter geometria para WKT para cada dataframe
emp_estado_wkt <- emp_estado %>% mutate(geom = st_as_text(geom))
emp_regiao_wkt <- emp_regiao %>% mutate(geom = st_as_text(geom))
fund_estado_wkt <- fund_estado %>% mutate(geom = st_as_text(geom))
fund_regiao_wkt <- fund_regiao %>% mutate(geom = st_as_text(geom))

# Salvar os dataframes com a geometria convertida em WKT para Parquet
write_parquet(emp_estado_wkt, "data/emp_estado.parquet")
write_parquet(emp_regiao_wkt, "data/emp_regiao.parquet")
write_parquet(fund_estado_wkt, "data/fund_estado.parquet")
write_parquet(fund_regiao_wkt, "data/fund_regiao.parquet")

#### LIMPAR AMBIENTE ####
rm(list = ls())
gc()