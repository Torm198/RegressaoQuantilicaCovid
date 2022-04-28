source('init.R',encoding='UTF-8')


periodo1 <- read_csv('Cortes/Periodo 1/Banco 04.02.2020 a 04.11.2020.csv')
periodo2 <- read_csv('Cortes/Periodo 2/Banco 01.03.2021 a 30.11.2021.csv')
periodo3 <- read_csv('Cortes/Periodo 3/Banco 01.12.2021 a 17.03.2022.csv')





#####################
cv <- function(x) sd(x,na.rm = T) / mean(x,na.rm=T) * 100


tabela_letalidade <- todo_periodo %>% group_by(periodo) %>%
  summarise(across(let,list(media=mean,mediana=median,DP=sd,CV=cv,ca=moments::skewness,cc=moments::kurtosis,min=min,max=max,n=length))) %>%
  mutate(across(-periodo,~round(.,digits = 3))) 

tabela_idade <- todo_periodo %>% group_by(periodo) %>%
  summarise(across(Idade_mediana,list(media=mean,mediana=median,DP=sd,CV=cv,ca=moments::skewness,cc=moments::kurtosis,min=min,max=max,n=length))) %>%
  mutate(across(-periodo,~round(.,digits = 3))) 

tabela_risco <- todo_periodo %>% group_by(periodo) %>%
  summarise(across(Risco,list(media=~mean(.x,na.rm=T),mediana=~median(.x,na.rm=T),DP=~sd(.x,na.rm=T),CV=~cv(.x),ca=~moments::skewness(.x,na.rm=T),
                              cc=~moments::kurtosis(.x,na.rm=T),min=~min(.,na.rm=T),max=~max(.x,na.rm=T),n=length))) %>%
  mutate(across(-periodo,~round(.,digits = 3))) 


todo_periodo %>% group_by(periodo) %>%
  summarise(across(dose1,list(media=~mean(.x,na.rm=T),mediana=~median(.x,na.rm=T),DP=~sd(.x,na.rm=T),CV=~cv(.x),ca=~moments::skewness(.x,na.rm=T),
                              cc=~moments::kurtosis(.x,na.rm=T),min=~min(.,na.rm=T),max=~max(.x,na.rm=T),n=length))) %>%
  mutate(across(-periodo,~round(.,digits = 3))) %>% xtable(.,digits = 3)


todo_periodo %>% group_by(periodo) %>%
  summarise(across(dose2,list(media=~mean(.x,na.rm=T),mediana=~median(.x,na.rm=T),DP=~sd(.x,na.rm=T),CV=~cv(.x),ca=~moments::skewness(.x,na.rm=T),
                              cc=~moments::kurtosis(.x,na.rm=T),min=~min(.,na.rm=T),max=~max(.x,na.rm=T),n=length))) %>%
  mutate(across(-periodo,~round(.,digits = 3))) %>% xtable(.,digits = 3)

todo_periodo %>% group_by(periodo) %>%
  summarise(across(reforco,list(media=~mean(.x,na.rm=T),mediana=~median(.x,na.rm=T),DP=~sd(.x,na.rm=T),CV=~cv(.x),ca=~moments::skewness(.x,na.rm=T),
                              cc=~moments::kurtosis(.x,na.rm=T),min=~min(.,na.rm=T),max=~max(.x,na.rm=T),n=length))) %>%
  mutate(across(-periodo,~round(.,digits = 3))) %>% xtable(.,digits = 3)

tabela_IDHM <- 
periodo1%>%
  summarise(across(IDHM,list(media=~mean(.x,na.rm=T),mediana=~median(.x,na.rm=T),DP=~sd(.x,na.rm=T),CV=~cv(.x),ca=~moments::skewness(.x,na.rm=T),
                                cc=~moments::kurtosis(.x,na.rm=T),min=~min(.,na.rm=T),max=~max(.x,na.rm=T),n=length))) %>%
  mutate(across(everything(),~round(.,digits = 3))) 
tabela_densidade <- 
periodo1%>%
  summarise(across(densidade2021,list(media=~mean(.x,na.rm=T),mediana=~median(.x,na.rm=T),DP=~sd(.x,na.rm=T),CV=~cv(.x),ca=~moments::skewness(.x,na.rm=T),
                                cc=~moments::kurtosis(.x,na.rm=T),min=~min(.,na.rm=T),max=~max(.x,na.rm=T),n=length))) %>%
  mutate(across(everything(),~round(.,digits = 3))) 
require(xtable)

xtable(tabela_letalidade,digits = 3)
xtable(tabela_idade,digits = 3)
xtable(tabela_risco,digits = 3)
xtable(tabela_IDHM,digits = 3)
xtable(tabela_densidade,digits = 3)
