dir <- 'Periodo 3/'
banco_lqr_corte <- corte_banco('01/12/2021','17/03/2021')


###################
fit1  <- lm(let~IDHM+densidade2021+Risco+Idade_mediana,
            x=T, y=T,data = banco_lqr_corte)
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
CL <- matrix(NA,length(qs),8)
LS <- matrix(NA,length(qs),8)
LI <- matrix(NA,length(qs),8)


epsilon <- 0.0001
fit_lm <- Glm(let~IDHM+PIB_cap+densidade2021+Risco+Idade_mediana+dose1+dose2+reforco,
              x=T, y=T,data = banco_lqr_corte)

# transformação
y_min <- min(banco_lqr_corte$let, na.rm=T)
y_max <- max(banco_lqr_corte$let, na.rm=T)

logit_linpred <- logit_fn(banco_lqr_corte$let, 
                          y_min=y_min,
                          y_max=y_max,
                          epsilon=epsilon)


for(k in 1:length(qs)){
  
  ## quantil do loop
  tau=qs[k] 
  
  
  
  
  
  # criando uma regressão linear
  
  
  
  # montando a regressão quantílica
  fit_rq <- Rq(formula(fit_lm), x=T, y=T, tau=tau,data = banco_lqr_corte)
  
  fit_rq_logit <- update(fit_rq, logit_linpred ~ .)
  boot_rq_logit <- bootcov(fit_rq_logit,B=500)
  # salvando os valores
  LI[k,] <- fit_rq_logit$coefficients-1.96*boot_rq_logit$summary[,2]
  CL[k,] <- fit_rq_logit$coefficients
  LS[k,] <- c(fit_rq_logit$coefficients)+1.96*boot_rq_logit$summary[,2]
  
}


beta0   <- data.frame(li=LI[,1],cl=CL[,1],ls=LS[,1])
beta1   <- data.frame(li=LI[,2],cl=CL[,2],ls=LS[,2])
beta2   <- data.frame(li=LI[,3],cl=CL[,3],ls=LS[,3])
beta3   <- data.frame(li=LI[,4],cl=CL[,4],ls=LS[,4])
beta4   <- data.frame(li=LI[,5],cl=CL[,5],ls=LS[,5])
beta5   <- data.frame(li=LI[,6],cl=CL[,6],ls=LS[,6]) 
beta6   <- data.frame(li=LI[,7],cl=CL[,7],ls=LS[,7])
beta7   <- data.frame(li=LI[,8],cl=CL[,8],ls=LS[,8])
##############################################################################

### plots


My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 18),
  axis.title.y = element_text(size = 20),
  axis.text.y = element_text(size = 18))


ggplot(beta0, aes(x = qs, y = cl)) +
  geom_ribbon(aes(ymin = li, ymax = ls), alpha = 0.2) +
  geom_line() +
  labs(x = expression(q), y = expression(beta[0])) + My_Theme
ggsave(paste0(dir,'beta0.png'))

ggplot(beta1, aes(x = qs, y = cl)) +
  geom_ribbon(aes(ymin = li, ymax = ls), alpha = 0.2) +
  geom_line() +
  labs(x = expression(q), y = expression(beta[1])) + My_Theme
ggsave(paste0(dir,'beta1.png'))


ggplot(beta2, aes(x = qs, y = cl)) +
  geom_ribbon(aes(ymin = li, ymax = ls), alpha = 0.2) +
  geom_line() +
  labs(x = expression(q), y = expression(beta[2])) + My_Theme
ggsave(paste0(dir,'beta2.png'))

ggplot(beta3, aes(x = qs, y = cl)) +
  geom_ribbon(aes(ymin = li, ymax = ls), alpha = 0.2) +
  geom_line() +
  labs(x = expression(q), y = expression(beta[3])) + My_Theme
ggsave(paste0(dir,'beta3.png'))


ggplot(beta4, aes(x = qs, y = cl)) +
  geom_ribbon(aes(ymin = li, ymax = ls), alpha = 0.2) +
  geom_line() +
  labs(x = expression(q), y = expression(beta[3])) + My_Theme
ggsave(paste0(dir,'beta4.png'))

ggplot(beta5, aes(x = qs, y = cl)) +
  geom_ribbon(aes(ymin = li, ymax = ls), alpha = 0.2) +
  geom_line() +
  labs(x = expression(q), y = expression(beta[4])) + My_Theme

ggsave(paste0(dir,'beta5.png'))

ggplot(beta6, aes(x = qs, y = cl)) +
  geom_ribbon(aes(ymin = li, ymax = ls), alpha = 0.2) +
  geom_line() +
  labs(x = expression(q), y = expression(beta[6])) + My_Theme
ggsave(paste0(dir,'beta6.png'))

ggplot(beta7, aes(x = qs, y = cl)) +
  geom_ribbon(aes(ymin = li, ymax = ls), alpha = 0.2) +
  geom_line() +
  labs(x = expression(q), y = expression(beta[6])) + My_Theme
ggsave(paste0(dir,'beta7.png'))


###################
## Previsoes
###################

set.seed(2020)
inds <- sample(1:nrow(banco_lqr_corte), 600, replace=FALSE)
banco_lqr_corte_treino <- banco_lqr_corte[inds,]
banco_lqr_corte_valid  <- banco_lqr_corte[-inds,]


## Quantis de interesse
qs <- c(0.025,0.975)
CL <- matrix(NA,length(qs),8)
LS <- matrix(NA,length(qs),8)
LI <- matrix(NA,length(qs),8)

forecasts_95  <- matrix(NA,nrow(banco_lqr_corte_valid),length(qs)) 


epsilon <- 0.0001
fit_lm <- Glm(let~IDHM+PIB_cap+densidade2021+Risco+Doses_diárias+Idade_mediana+esquema,
              x=T, y=T,data = banco_lqr_corte_treino)

# transformação
y_min <- min(banco_lqr_corte_treino$let, na.rm=T)
y_max <- max(banco_lqr_corte_treino$let, na.rm=T)

logit_linpred <- logit_fn(banco_lqr_corte_treino$let, 
                          y_min=y_min,
                          y_max=y_max,
                          epsilon=epsilon)


## matriz X de validacao

X_valid <- cbind(1,
                 banco_lqr_corte_valid$IDHM,
                 banco_lqr_corte_valid$PIB_cap,
                 banco_lqr_corte_valid$densidade2021,
                 banco_lqr_corte_valid$Risco,
                 banco_lqr_corte_valid$Doses_diárias,
                 banco_lqr_corte_valid$Idade_mediana,
                 banco_lqr_corte_valid$esquema)

for(k in 1:length(qs)){
  
  ## quantil do loop
  tau=qs[k] 
  
  # criando uma regressão linear
  
  # montando a regressão quantílica
  fit_rq <- Rq(formula(fit_lm), x=T, y=T, tau=tau,data = banco_lqr_corte_treino)
  
  fit_rq_logit <- update(fit_rq, logit_linpred ~ .)
  boot_rq_logit <- bootcov(fit_rq_logit,B=500)
  # salvando os valores
  LI[k,] <- fit_rq_logit$coefficients-1.96*boot_rq_logit$summary[,2]
  CL[k,] <- fit_rq_logit$coefficients
  LS[k,] <- c(fit_rq_logit$coefficients)+1.96*boot_rq_logit$summary[,2]
  
  
  # predict 
  p_treino <- X_valid %*% fit_rq_logit$coefficients
  
  
  # Change back to org. scale
  transformed_p  <- antilogit_fn(p_treino,
                                 y_min=y_min,
                                 y_max=y_max,
                                 epsilon=epsilon)
  
  
  forecasts_95[,k]  <-  as.vector(transformed_p)
  
  
}

#gráfico no ggplot está dando problemas com o gráfico padrão
data.frame(upper=forecasts_95[,1],lower=forecasts_95[,2],indice=1:dim(forecasts_95)[1],dados=banco_lqr_corte_valid$let) %>%
  ggplot(aes(x=indice,y=upper)) + geom_line()+
  geom_line(aes(y=lower)) + geom_point(aes(y=dados)) + xlab('Letalidade') +ylab('Índice')+
  labs(title = 'Intervalo de predição 95%')#+ My_Theme
ggsave(paste0(dir,'Performance.png'))

#mantido até resolver o tema do ggplot

plot(banco_lqr_corte_valid$let,xlab="índice",ylab="Letalidade",ylim=c(0,0.10),pch=16, main="Intervalo de predição 95%")
lines(forecasts_95[,1],lwd=1.3,lty=3)
lines(forecasts_95[,2],lwd=1.3,lty=3)

round(mean(forecasts_95[,1] <= banco_lqr_corte_valid$let & banco_lqr_corte_valid$let <= forecasts_95[,2], na.rm = TRUE)*100,2)