source('init.R',encoding='UTF-8')

metadata <- download_metadata()

mun_sp <- read_municipality(code_muni = 35,year = 2020) %>% mutate(cod_ibge=as.numeric(substr(code_muni,1,6)))
periodo1 <- read_csv('Cortes/Periodo 1/Banco 04.02.2020 a 04.11.2020.csv')
periodo2 <- read_csv('Cortes/Periodo 2/Banco 01.03.2021 a 30.11.2021.csv')
periodo3 <- read_csv('Cortes/Periodo 3/Banco 01.12.2021 a 17.03.2022.csv')


todo_periodo <- 
  bind_rows(
    periodo1 %>% mutate(periodo='Período 1'),
    periodo2 %>% mutate(periodo='Período 2'),
    periodo3 %>% mutate(periodo='Período 3'))

graf1 <- left_join(mun_sp,periodo1,by=c('cod_ibge'='Codigo'))
graf2 <- left_join(mun_sp,periodo2,by=c('cod_ibge'='Codigo'))
graf3 <- left_join(mun_sp,periodo3,by=c('cod_ibge'='Codigo'))
graf_all <- left_join(mun_sp,todo_periodo,by=c('cod_ibge'='Codigo'))
#labs(title="Taxa de Letalidade por município em São Paulo até 04/10/2021", size=8)+

tema_mapa <- theme(line = element_blank())

##############casos gerais##################
SEADE %>% group_by(Data) %>% summarise(n=sum(n)) %>%
  ggplot(.,aes(x=Data,y=n))+geom_col()+geom_vline(xintercept=as.numeric(c(dmy("04/10/2020"),dmy("30/11/2021"))))+
  ylab('N° de novos casos de COVID-19')


##############mapa letalidade###########

grafico_letalidade <-
 ggplot()+geom_sf(data = graf_all,aes(fill=let),size=0)+
  scale_fill_gradient(name = "Taxa de Letalidade",low="lightgrey", high="red",na.value = "white",limits=c(0,.25))  +facet_wrap(~periodo,nrow = 2)+tema_mapa
ggsave(grafico_letalidade,file='Gráficos Exploratórios/Mapa de letalidade.pdf',
       device = 'pdf',dpi=320)


#################mapa risco#############
grafico_risco <-
  ggplot()+geom_sf(data = graf_all,aes(fill=Risco),size=0)+
  scale_fill_gradient(name = "Proporção de pacientes de Risco",low="lightgrey", high="orange",na.value = "white")  +facet_wrap(~periodo,nrow = 2)+tema_mapa

ggsave(grafico_risco,file='Gráficos Exploratórios/Mapa de risco.pdf',
       device = 'pdf',dpi=320)
###############mapa IDHM#################
grafico_idhm <- 
ggplot()+geom_sf(data = graf1,aes(fill=IDHM),size=.15)+
  scale_fill_gradient(name = "IDHM",low="lightgrey", high="darkgreen",na.value = "white")  + 
  theme_minimal(base_size = 10) + tema_mapa
ggsave(grafico_idhm,file='Gráficos Exploratórios/Mapa IDHM.pdf',
       device = 'pdf',dpi=320)
###############mapa densidade#################
grafico_densidade <- 
ggplot()+geom_sf(data = graf1,aes(fill=densidade2021),size=.15)+
  scale_fill_gradient(name = "pop./m²",low="lightgrey", high="brown",na.value = "white")  + 
  theme_minimal(base_size = 10) + tema_mapa
ggsave(grafico_densidade,file='Gráficos Exploratórios/Mapa densidade.pdf',
       device = 'pdf',dpi=320)


######### mapa idade mediana##############
ggplot()+geom_sf(data = graf1,aes(fill=Idade_mediana),size=.15)+
  scale_fill_gradient(name = "Idade",low="lightgrey", high="red",na.value = "white")  + 
  theme_minimal(base_size = 10)

ggplot()+geom_sf(data = graf2,aes(fill=Idade_mediana),size=.15)+
  scale_fill_gradient(name = "Idade",low="lightgrey", high="red",na.value = "white")  + 
  theme_minimal(base_size = 10)

ggplot()+geom_sf(data = graf3,aes(fill=Idade_mediana),size=.15)+
  scale_fill_gradient(name = "Idade",low="lightgrey", high="red",na.value = "white")  + 
  theme_minimal(base_size = 10)



map_median <- ggplot()+geom_sf(data = graf_all,aes(fill=Idade_mediana),size=0)+
  scale_fill_gradient(name = "Idade mediana",low="lightgrey", high="blue",na.value = "white")+
  tema_mapa+facet_wrap(~periodo,nrow=2)

ggsave(map_median,file='Gráficos Exploratórios/Mapa de mediana.pdf',
       device = 'pdf',dpi=320)

############mapa vacina############



ggplot()+geom_sf(data = graf_all ,aes(fill=dose1),size=.15)+
  scale_fill_gradient(name = "Porcentagem de doses",low="lightgrey", high="red",na.value = "white")  + 
  theme_minimal(base_size = 10) +facet_wrap(~periodo)
ggplot()+geom_sf(data = graf_all ,aes(fill=dose2),size=.15)+
  scale_fill_gradient(name = "Porcentagem de doses",low="lightgrey", high="red",na.value = "white")  + 
  theme_minimal(base_size = 10) +facet_wrap(~periodo)
ggplot()+geom_sf(data = graf_all ,aes(fill=reforco),size=.15)+
  scale_fill_gradient(name = "Porcentagem de doses",low="lightgrey", high="red",na.value = "white")  + 
  theme_minimal(base_size = 10) +facet_wrap(~periodo)

graf_vacina <- todo_periodo %>% pivot_longer(c(dose1,dose2,reforco)) %>%
  left_join(mun_sp,.,by=c('cod_ibge'='Codigo')) %>% filter(periodo!='Período 1')

mapa_vacina <- 
ggplot()+geom_sf(data = graf_vacina ,aes(fill=value),size=0)+
  scale_fill_gradient(name = 'Proporção de doses\naplicadas na população',low="lightgrey", high="magenta",na.value = "white") +
  facet_wrap(vars(periodo,name),nrow=2,dir = "h")

ggsave(mapa_vacina,file='Gráficos Exploratórios/Mapa vacina.pdf',
       device = 'pdf',dpi=320)


##########scatter plots############

ggplot(todo_periodo,aes(x=IDHM,y=let)) + geom_point() +facet_wrap(~periodo)


scat_median <- ggplot(todo_periodo,aes(x=Idade_mediana,y=let)) + geom_point() +facet_wrap(~periodo)+ ylab('Taxa de Letalidade')
ggsave(scat_median,file='Gráficos Exploratórios/scat median.pdf',
       device = 'pdf',dpi=320)


ggplot(todo_periodo,aes(x=densidade2021,y=let)) + geom_point() +facet_wrap(~periodo)+ ylab('Taxa de Letalidade')
scat_risco <- ggplot(todo_periodo,aes(x=Risco,y=let)) + geom_point() +facet_wrap(~periodo)+ ylab('Taxa de Letalidade')
ggsave(scat_risco,file='Gráficos Exploratórios/scat risco.pdf',
       device = 'pdf',dpi=320)

scat_vacina <- ggplot(graf_vacina,aes(x=value,y=let))+geom_point()+
  facet_wrap(vars(periodo,name))+ 
  xlab('Proporção de doses aplicadas na população') + ylab('Taxa de Letalidade')
ggsave(scat_vacina,file='Gráficos Exploratórios/scat vacina.pdf',
       device = 'pdf',dpi=320)



scat_IDHM <- ggplot(todo_periodo,aes(x=IDHM,y=let)) + geom_point() +facet_wrap(~periodo)+ ylab('Taxa de Letalidade')
ggsave(scat_IDHM,file='Gráficos Exploratórios/scat IDHM.pdf',
       device = 'pdf',dpi=320)


scat_dense <- ggplot(todo_periodo,aes(x=densidade2021,y=let)) + geom_point()+facet_wrap(~periodo) + ylab('Taxa de Letalidade')
ggsave(scat_dense,file='Gráficos Exploratórios/scat densidade.pdf',
       device = 'pdf',dpi=320)

##########histogramas######


hist_let <- ggplot(todo_periodo,aes(x=let)) + geom_histogram() +facet_wrap(~periodo) + xlab('Taxa de Letalidade') + ylab('Frequência')
ggsave(hist_let,file='Gráficos Exploratórios/Hist let.pdf',
       device = 'pdf',dpi=320)

hist_risco <- ggplot(todo_periodo,aes(x=Risco)) + geom_histogram() +facet_wrap(~periodo)
ggsave(hist_risco,file='Gráficos Exploratórios/Hist ris.pdf',
       device = 'pdf',dpi=320)
hist_median <- ggplot(todo_periodo,aes(x=Idade_mediana)) + geom_histogram() +facet_wrap(~periodo)
ggsave(hist_median,file='Gráficos Exploratórios/Hist med.pdf',device='pdf',dpi=320)


hist_idhm <- ggplot(periodo1,aes(x=IDHM)) + geom_histogram()
ggsave(hist_idhm,file='Gráficos Exploratórios/Hist idhm.pdf',device='pdf',dpi=320)
hist_densidade <- ggplot(periodo1,aes(x=densidade2021)) + geom_histogram()
ggsave(hist_densidade,file='Gráficos Exploratórios/Hist densidade.pdf',device='pdf',dpi=320)

hist_vacina <- ggplot(graf_vacina,aes(x=value))+geom_histogram()+
  facet_wrap(vars(periodo,name))+ 
  xlab('Proporção de doses aplicadas na população') + ylab('Frequência')

ggsave(hist_vacina,file='Gráficos Exploratórios/Hist vacina.pdf',
       device = 'pdf',dpi=320)
