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
SEADE <- readRDS('cache/SEADE.RDS')
Banco_Idade <- readRDS('Cache/Idade.RDS')

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

My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 18),
  axis.title.y = element_text(size = 20),
  axis.text.y = element_text(size = 18))
for( i in 1:7){
 beta0   <- data.frame(li=LI[,i],cl=CL[,i],ls=LS[,i])
 
 ggplot(beta0, aes(x = qs, y = cl)) +
   geom_ribbon(aes(ymin = li, ymax = ls), alpha = 0.2) +
   geom_line() +
   labs(x = expression(p), y = expression(beta)) + My_Theme
 Sys.sleep(2)
 
 }
