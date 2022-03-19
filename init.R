#####Pacotes

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
SEADE <- readRDS('cache/SEADE.RDS')
Banco_Idade <- readRDS('Cache/Idade.RDS')





##########funções criadas para ##################

#normalização para as variáveis 

normalizacao <- function(x){
  
  minimo <- min(x,na.rm = T)
  maximo <- max(x,na.rm = T)
  return((x-minimo)/(maximo-minimo))
}

corte_banco <- function(Data_inicio,Data_final){
  
  
  int <- interval(dmy(Data_inicio),dmy(Data_final))
  
  a <- SEADE %>% filter(Data %within% int) %>% group_by(Municipio,Risco) %>% summarise(n=sum(n)) %>%
    pivot_wider(names_from = Risco,values_from = n) %>% summarise(Risco=sum(`1`)/sum(`0`+`1`))
  
  
  b <- SEADE %>% filter(Data %within% int)  %>%
    pivot_wider(names_from=Obito,values_from=n,values_fill = 0)%>%
    group_by(Municipio) %>%
    mutate(let=sum(`1`,na.rm=T)/sum(`0`+`1`)) %>%
    select(-c(Data,Risco,`0`,`1`)) %>% unique()
  
  # Método antigo
  # c <- vacinacao_sp%>% filter(Data %within% int) %>% pivot_wider(names_from=vacina_descricao_dose,values_from = n, values_fill = 0) %>%
  #   group_by(estabelecimento_municipio_codigo,estabelecimento_uf)%>%
  #   summarise(esquema=sum(`2ª Dose`+`Dose Adicional`+`Reforço`+`Única`+`2ª Dose Revacinação`+`3ª Dose`+`1º Reforço`,na.rm=T)) %>%
  #   rename(Codigo=estabelecimento_municipio_codigo)
  # 
  
  c <- vacinacao_sp %>% pivot_wider(names_from=vacina_descricao_dose,values_from = n,values_fill = 0) %>%
    group_by(vacina_dataAplicacao,estabelecimento_municipio_codigo,estabelecimento_uf)%>%
    summarise(dose1=sum(`1ª Dose`+Dose+`1ª Dose Revacinação`,na.rm=T),
              dose2=sum(`2ª Dose`+`Dose Adicional`+`Única`+`2ª Dose Revacinação`,na.rm=T),
              reforco=sum(Reforço+`3ª Dose`+`1º Reforço`))  %>%
    arrange(vacina_dataAplicacao) %>%
    group_by(estabelecimento_municipio_codigo)%>%
    filter(vacina_dataAplicacao %within% int)%>%
    mutate(dose1=cumsum(dose1),
           dose2=cumsum(dose2),
           reforco=cumsum(reforco)) %>%
    summarise(across(dose1:reforco,max))%>%
    rename(Codigo=estabelecimento_municipio_codigo)
  
  
  
  d <- Banco_Idade %>% filter(Data %within% int) %>% group_by(Municipio) %>% summarise(Idade_mediana=median(Idade,na.rm = T))
  
  e <- vacinacao_sp %>% filter(vacina_dataAplicacao %within% int) %>%
    group_by(estabelecimento_municipio_codigo) %>% arrange(vacina_dataAplicacao) %>%
    summarise(Doses_diárias=mean(n)) %>% rename(Codigo=estabelecimento_municipio_codigo)
  
  banco_lqr_corte <- left_join(b,a) %>% left_join(c) %>% left_join(d) %>% left_join(e) %>% ungroup() %>%
    mutate(PIB_cap=PIB_cap/10000,densidade2021=densidade2021/100) %>%
    mutate(across(c(dose1,dose2,reforco),~normalizacao(./Pop)))%>% filter(!is.na(Codigo))
  
  return(banco_lqr_corte)
}


logit_fn <- function(y, y_min, y_max, epsilon){
  log((y-(y_min-epsilon))/(y_max+epsilon-y))}

antilogit_fn <- function(antiy, y_min, y_max, epsilon){
  (exp(antiy)*(y_max+epsilon)+y_min-epsilon)/(1+exp(antiy))}
