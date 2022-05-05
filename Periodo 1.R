source('init.R',encoding='UTF-8')

dir <- 'Cortes/Periodo 1/'

# banco_lqr_corte <- corte_banco('04/02/2020','04/10/2020')
# write_csv(banco_lqr_corte,'Cortes/Periodo 1/Banco 04.02.2020 a 04.10.2020.csv')


banco_lqr_corte <- read_csv('Cortes/Periodo 1/Banco 04.02.2020 a 04.10.2020.csv')

###################
fit1  <- lm(let~IDHM+densidade2021+Risco+Idade_mediana,
            x=T, y=T,data = banco_lqr_corte)
summary(fit1)
vif_values <-vif(fit1)
barplot(vif_values, main = "VIF para o Período 1")


#######################




fit_lm <- Glm(let~IDHM+densidade2021+Risco+Idade_mediana,
              x=T, y=T,data = banco_lqr_corte)


qs <- seq(0.01,0.99,by=0.01)


CL <- matrix(NA,length(qs),length(fit_lm$coefficients))
LS <- matrix(NA,length(qs),length(fit_lm$coefficients))
LI <- matrix(NA,length(qs),length(fit_lm$coefficients))
pv <- matrix(NA,length(qs),length(fit_lm$coefficients))

epsilon <- 0.0001


# transformação


logit_linpred <- logit_fn(banco_lqr_corte$let, 
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
  pv[k,] <- fit_rq$summary[,4]
  
}


beta0   <- data.frame(li=LI[,1],cl=CL[,1],ls=LS[,1],pv=pv[,1])
beta1   <- data.frame(li=LI[,2],cl=CL[,2],ls=LS[,2],pv=pv[,2])
beta2   <- data.frame(li=LI[,3],cl=CL[,3],ls=LS[,3],pv=pv[,3])
beta3   <- data.frame(li=LI[,4],cl=CL[,4],ls=LS[,4],pv=pv[,4])
beta4   <- data.frame(li=LI[,5],cl=CL[,5],ls=LS[,5],pv=pv[,5])

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
ggsave(paste0(dir,'beta0.pdf'),device="pdf")

ggplot(beta1, aes(x = qs, y = cl)) +
  geom_ribbon(aes(ymin = li, ymax = ls), alpha = 0.2) +
  geom_line() +
  labs(x = expression(q), y = expression(beta[1])) + My_Theme
ggsave(paste0(dir,'beta1.pdf'),device="pdf")


ggplot(beta2, aes(x = qs, y = cl)) +
  geom_ribbon(aes(ymin = li, ymax = ls), alpha = 0.2) +
  geom_line() +
  labs(x = expression(q), y = expression(beta[2])) + My_Theme
ggsave(paste0(dir,'beta2.pdf'),device="pdf")

ggplot(beta3, aes(x = qs, y = cl)) +
  geom_ribbon(aes(ymin = li, ymax = ls), alpha = 0.2) +
  geom_line() +
  labs(x = expression(q), y = expression(beta[3])) + My_Theme
ggsave(paste0(dir,'beta3.pdf'),device="pdf")


ggplot(beta4, aes(x = qs, y = cl)) +
  geom_ribbon(aes(ymin = li, ymax = ls), alpha = 0.2) +
  geom_line() +
  labs(x = expression(q), y = expression(beta[4])) + My_Theme
ggsave(paste0(dir,'beta4.pdf'),device="pdf")


###

ggplot(beta0, aes(x = qs, y = pv)) +
  geom_line() +
  labs(x = expression(q), y = expression('p-valor'~beta[0]))
ggsave(paste0(dir,'beta0_pvalor.pdf'),device="pdf")

ggplot(beta1, aes(x = qs, y = pv)) +
  geom_line() +
  labs(x = expression(q), y =  expression('p-valor'~beta[1]))
ggsave(paste0(dir,'beta1_pvalor.pdf'),device="pdf")

ggplot(beta2, aes(x = qs, y = pv)) +
  geom_line() +
  labs(x = expression(q), y =  expression('p-valor'~beta[2]))
ggsave(paste0(dir,'beta2_pvalor.pdf'),device="pdf")

ggplot(beta3, aes(x = qs, y = pv)) +
  geom_line() +
  labs(x = expression(q), y =  expression('p-valor'~beta[3]))
ggsave(paste0(dir,'beta3_pvalor.pdf'),device="pdf")

ggplot(beta4, aes(x = qs, y = pv)) +
  geom_line() +
  labs(x = expression(q), y = expression('p-valor'~beta[4]))
ggsave(paste0(dir,'beta4_pvalor.pdf'),device="pdf")
#################
nome <- names(fit_rq$coefficients)

# função abaixo recebe uma matriz de resultado para tratar numa forma que facilite a criação de gráficos
resultado <- left_join(tratamento(LI,'inferior'),tratamento(CL,'pontual')) %>%
  left_join(tratamento(LS,'superior')) %>%
  left_join(tratamento(pv,'pvalor'))

ggplot(resultado,aes(x=quantil,y=pontual))+
  geom_line()+
  geom_ribbon(aes(ymin=inferior,ymax=superior),alpha=0.2)+
  facet_wrap(~beta,scales = 'free_y')
ggsave(paste0(dir,'estimativas_sem_escala.pdf'),device='pdf')
ggplot(resultado,aes(x=quantil,y=pontual))+
  geom_line()+
  geom_ribbon(aes(ymin=inferior,ymax=superior),alpha=0.2)+
  facet_wrap(~beta)
ggsave(paste0(dir,'estimativas_com_escala.pdf'),device='pdf')

ggplot(resultado,aes(x=quantil,y=pvalor))+
  geom_line()+
  facet_wrap(~beta)
ggsave(paste0(dir,'pvalor.pdf'),device='pdf')

###################
## Previsoes
###################

set.seed(2020)
inds <- sample(1:nrow(banco_lqr_corte), 200, replace=FALSE)
banco_lqr_corte_treino <- banco_lqr_corte[-inds,]
banco_lqr_corte_valid  <- banco_lqr_corte[inds,]


## Quantis de interesse
qs <- c(0.025,0.975)
CL <- matrix(NA,length(qs),length(fit_lm$coefficients))
LS <- matrix(NA,length(qs),length(fit_lm$coefficients))
LI <- matrix(NA,length(qs),length(fit_lm$coefficients))
pv <- matrix(NA,length(qs),length(fit_lm$coefficients))
forecasts_95  <- matrix(NA,nrow(banco_lqr_corte_valid),length(qs)) 


epsilon <- 0.0001


# transformação

logit_linpred <- logit_fn(banco_lqr_corte_treino$let, 
                          epsilon=epsilon)


## matriz X de validacao

X_valid <- cbind(1,
                 banco_lqr_corte_valid$IDHM,
                 banco_lqr_corte_valid$densidade2021,
                 banco_lqr_corte_valid$Risco,
                 banco_lqr_corte_valid$Idade_mediana)

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
                                 epsilon=epsilon)
  
  
  forecasts_95[,k]  <-  as.vector(transformed_p)
  
  
}

#gráfico no ggplot está dando problemas com o gráfico padrão
data.frame(upper=forecasts_95[,1],lower=forecasts_95[,2],indice=1:dim(forecasts_95)[1],dados=banco_lqr_corte_valid$let) %>%
  ggplot(aes(x=indice,y=upper)) + geom_line()+
  geom_line(aes(y=lower)) + geom_point(aes(y=dados)) + xlab('Letalidade') +ylab('Índice')+
  labs(title = 'Intervalo de predição 95%')#+ My_Theme
ggsave(paste0(dir,'Performance.pdf'),device="pdf")


round(mean(forecasts_95[,1] <= banco_lqr_corte_valid$let & banco_lqr_corte_valid$let <= forecasts_95[,2], na.rm = TRUE)*100,2)
