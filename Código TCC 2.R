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


vacinacao_sp <- readRDS('cache/banco vacina.RDS')
SEADE <- readRDS('cache/SEADE.RDS') %>% ungroup()
Banco_Idade <- readRDS('Cache/Idade.RDS')
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


# trat <-
#   function(x, pos){
#     x %>% group_by(vacina_dataAplicacao,
#                    vacina_descricao_dose,
#                    estabelecimento_municipio_codigo,
#                    estabelecimento_uf) %>% count()}
# 
# trat2 <- function(x){
#   x%>% group_by(vacina_dataAplicacao,
#                vacina_descricao_dose,
#                estabelecimento_municipio_codigo,
#                estabelecimento_uf) %>% summarise(n = sum(n))
# }
# 
# parte1 <- readr::read_csv2_chunked(
#   'Dados/Vacina sp parte 1.csv',
#   DataFrameCallback$new(trat),
#   chunk_size = 7000) %>% trat2()
# 
# 
# parte2 <- readr::read_csv2_chunked(
#   'Dados/Vacina sp parte 2.csv',
#   DataFrameCallback$new(trat),
#   chunk_size = 7000)%>% trat2()
# 
# 
# parte3 <- readr::read_csv2_chunked(
#   'Dados/Vacina sp parte 3.csv',
#   DataFrameCallback$new(trat),
#   chunk_size = 7000)%>% trat2()
# 
# 
# vacinacao_sp <- bind_rows(parte1,parte2,parte3)%>% trat2()
# 
# 
# 
# 
# saveRDS(vacinacao_sp,'Cache/banco vacina.RDS')
# 
# 
# rm(parte1,parte2,parte3)
# #########TRATAMENTO MICRODADOS COVID SEADE################
# 
# 
# SEADE <- read_csv2('Dados/20220121_Casos-e-obitos-ESP.csv',
#                    locale = locale(encoding = 'UTF-8')) %>%
#   mutate(Data=lubridate::dmy(`Data Inicio Sintomas`)) %>%
#   unite('Risco',Asma:`Outros Fatores De Risco`,remove = TRUE) %>%
#   mutate(Risco=str_detect(Risco,'SIM')*1) %>%
#   group_by(Municipio,Data,Risco,Obito) %>% count() %>% left_join(demo_sp_mun)
# 
# 
# 
# 
# 
# saveRDS(SEADE,'Cache/SEADE.RDS')
# 
# Preparo do banco de idade
# 
# Banco_Idade <- read_csv2('Dados/20220121_Casos-e-obitos-ESP.csv',
#                    locale = locale(encoding = 'UTF-8')) %>%
#   mutate(Data=lubridate::dmy(`Data Inicio Sintomas`)) %>%
#   select(Municipio,Data,Idade)
# 
# saveRDS(Banco_Idade,'Cache/Idade.RDS')
####montagem do banco lqr caso geral##########








a <- SEADE %>% group_by(Municipio,Risco) %>% summarise(n=sum(n)) %>%
  pivot_wider(names_from = Risco,values_from = n) %>% summarise(Risco=sum(`1`)/sum(`0`+`1`))


b <- SEADE %>%
  pivot_wider(names_from=Obito,values_from=n,values_fill = 0)%>%
  group_by(Municipio) %>%
  mutate(let=sum(`1`)/sum(`0`+`1`)) %>%
  select(-c(Data,Risco,`0`,`1`)) %>% unique()

c <- vacinacao_sp  %>%
  group_by(estabelecimento_municipio_codigo) %>% arrange(vacina_dataAplicacao) %>%
  summarise(Doses_diárias=mean(n)) %>% rename(Codigo=estabelecimento_municipio_codigo)


d <- Banco_Idade %>% group_by(Municipio) %>% summarise(Idade_mediana=median(Idade,na.rm = T))

banco_lqr <- left_join(b,a) %>% left_join(c) %>% left_join(d)








###################


###################
fit1  <- lm(let~IDHM+PIB_cap+densidade2021+Risco+Doses_diárias+Idade_mediana,
            x=T, y=T,data = banco_lqr)
summary(fit1)
vif_values <-vif(fit1)
barplot(vif_values, main = "VIF Values")


#######################
logit_fn <- function(y, y_min, y_max, epsilon){
  log((y-(y_min-epsilon))/(y_max+epsilon-y))}

antilogit_fn <- function(antiy, y_min, y_max, epsilon){
  (exp(antiy)*(y_max+epsilon)+y_min-epsilon)/(1+exp(antiy))}

## Quantis de interesse
qs <- seq(0.05,0.95,by=0.01)
CL <- matrix(NA,length(qs),7)
LS <- matrix(NA,length(qs),7)
LI <- matrix(NA,length(qs),7)


epsilon <- 0.0001
fit_lm <- Glm(let~IDHM+PIB_cap+densidade2021+Risco+Doses_diárias+Idade_mediana,
              x=T, y=T,data = banco_lqr)

# transformação
y_min <- 0
y_max <- 1

logit_linpred <- logit_fn(banco_lqr$let, 
                          y_min=y_min,
                          y_max=y_max,
                          epsilon=epsilon)


for(k in 1:length(qs)){
  
  ## quantil do loop
  tau=qs[k] 
  
  
  
  
  
  # criando uma regressão linear
  
  
  
  # montando a regressão quantílica
  fit_rq <- Rq(formula(fit_lm), x=T, y=T, tau=tau,data = banco_lqr)
  
  fit_rq_logit <- update(fit_rq, logit_linpred ~ .)
  boot_rq_logit <- bootcov(fit_rq_logit,B=500)
  # salvando os valores
  LI[k,] <- fit_rq_logit$coefficients-1.96*boot_rq_logit$summary[,2]
  CL[k,] <- fit_rq_logit$coefficients
  LS[k,] <- c(fit_rq_logit$coefficients)+1.96*boot_rq_logit$summary[,2]
  
}



##############################################################################