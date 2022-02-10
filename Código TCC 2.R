#####Pacotes utilizados#######
require(tidyverse)
require(readr)
require(ggcorrplot)
require(geobr)
require(VGAM)
require(gtools)
library(extraDistr)
require(pracma)
require(quantreg)
library(rms)
require(lubridate)


####Carregando cache##########


load('cache/banco vacina.RDS')
load('cache/SEADE.RDS')

####Dados do IBGE: Importação e Tratamento##############

# dados demográficos
demo_sp_mun <- 
  readxl::read_xlsx('Dados/Dados Demográficos SP.xlsx') %>%
  select(Municipio=Município,
         Codigo=Código,
         IDHM,
         PIB_cap=`PIB per capita - R$ [2018]`,
         Pop=`População estimada - pessoas [2021]`,
         area=`Área Territorial - km² [2020]`) %>%
  mutate(Codigo=as.numeric(str_sub(Codigo,1,-2)),
         densidade2021=Pop/area,
         Municipio=toupper(Municipio))

##MUITO LERDO, NÃO RODAR SE JÁ TIVER CACHE#########


trat <-
  function(x, pos){
    x %>% group_by(vacina_dataAplicacao,
                   vacina_descricao_dose,
                   estabelecimento_municipio_codigo,
                   estabelecimento_uf) %>% count()}

trat2 <- function(x){
  x%>% group_by(vacina_dataAplicacao,
               vacina_descricao_dose,
               estabelecimento_municipio_codigo,
               estabelecimento_uf) %>% summarise(n = sum(n))
}

parte1 <- readr::read_csv2_chunked(
  'Dados/Vacina sp parte 1.csv',
  DataFrameCallback$new(trat),
  chunk_size = 7000) %>% trat2()


parte2 <- readr::read_csv2_chunked(
  'Dados/Vacina sp parte 2.csv',
  DataFrameCallback$new(trat),
  chunk_size = 7000)%>% trat2()


parte3 <- readr::read_csv2_chunked(
  'Dados/Vacina sp parte 3.csv',
  DataFrameCallback$new(trat),
  chunk_size = 7000)%>% trat2()


vacinacao_sp <- bind_rows(parte1,parte2,parte3)%>% trat2()




saveRDS(vacinacao_sp,'Cache/banco vacina.RDS')


rm(parte1,parte2,parte3)
#########TRATAMENTO MICRODADOS COVID SEADE################


SEADE <- read_csv2('Dados/20220121_Casos-e-obitos-ESP.csv',
                   locale = locale(encoding = 'UTF-8')) %>%
  mutate(Data=lubridate::dmy(`Data Inicio Sintomas`)) %>%
  unite('Risco',Asma:`Outros Fatores De Risco`,remove = TRUE) %>%
  mutate(Risco=str_detect(Risco,'SIM')*1) %>%
  group_by(Municipio,Data,Risco,Obito) %>% count() %>% left_join(demo_sp_mun)





saveRDS(SEADE,'Cache/SEADE.RDS')



## transformação logito
logit_fn <- function(y, y_min, y_max, epsilon) 
  log((y-(y_min-epsilon))/(y_max+epsilon-y))

antilogit_fn <- function(antiy, y_min, y_max, epsilon)
  (exp(antiy)*(y_max+epsilon)+y_min-epsilon)/(1+exp(antiy))