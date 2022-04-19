source('init.R',encoding='UTF-8')

metadata <- download_metadata()

mun_sp <- read_municipality(code_muni = 35,year = 2020) %>% mutate(cod_ibge=as.numeric(substr(code_muni,1,6)))
periodo1 <- read_csv('Cortes/Periodo 1/Banco 04.02.2020 a 04.11.2020.csv')
periodo2 <- read_csv('Cortes/Periodo 2/Banco 01.03.2021 a 30.11.2021.csv')
periodo3 <- read_csv('Cortes/Periodo 3/Banco 01.12.2021 a 17.03.2022.csv')



graf1 <- left_join(mun_sp,periodo1,by=c('cod_ibge'='Codigo'))
graf2 <- left_join(mun_sp,periodo2,by=c('cod_ibge'='Codigo'))
graf3 <- left_join(mun_sp,periodo3,by=c('cod_ibge'='Codigo'))
graf_all <- left_join(mun_sp,todo_periodo,by=c('cod_ibge'='Codigo'))
#labs(title="Taxa de Letalidade por município em São Paulo até 04/10/2021", size=8)+


# letalidade
ggplot()+geom_sf(data = graf1,aes(fill=let),size=.15)+
  scale_fill_gradient(name = "Taxa de letalidade",low="lightgrey", high="red",na.value = "white")  + 
  theme_minimal(base_size = 10)
ggplot()+geom_sf(data = graf2,aes(fill=let),size=.15)+
  scale_fill_gradient(name = "Taxa de letalidade",low="lightgrey", high="red",na.value = "white",limits=c(0,1))  + 
  theme_minimal(base_size = 10)
ggplot()+geom_sf(data = graf3,aes(fill=let),size=.15)+
  scale_fill_gradient(name = "Taxa de letalidade",low="lightgrey", high="red",na.value = "white",limits=c(0,.25))  + 
  theme_minimal(base_size = 10)
ggplot()+geom_sf(data = graf_all,aes(fill=let),size=.15)+
  scale_fill_gradient(name = "Taxa de letalidade",low="lightgrey", high="red",na.value = "white",limits=c(0,.25))  + 
  theme_minimal(base_size = 10)+facet_wrap(~periodo)



# IDHM

ggplot()+geom_sf(data = graf1,aes(fill=IDHM),size=.15)+
  scale_fill_gradient(name = "IDHM",low="lightgrey", high="red",na.value = "white")  + 
  theme_minimal(base_size = 10)

# idade mediana
ggplot()+geom_sf(data = graf1,aes(fill=Idade_mediana),size=.15)+
  scale_fill_gradient(name = "Idade",low="lightgrey", high="red",na.value = "white")  + 
  theme_minimal(base_size = 10)

ggplot()+geom_sf(data = graf2,aes(fill=Idade_mediana),size=.15)+
  scale_fill_gradient(name = "Idade",low="lightgrey", high="red",na.value = "white")  + 
  theme_minimal(base_size = 10)

ggplot()+geom_sf(data = graf3,aes(fill=Idade_mediana),size=.15)+
  scale_fill_gradient(name = "Idade",low="lightgrey", high="red",na.value = "white")  + 
  theme_minimal(base_size = 10)



ggplot()+geom_sf(data = ,aes(fill=Idade_mediana),size=.15)+
  scale_fill_gradient(name = "Idade",low="lightgrey", high="red",na.value = "white")  + 
  theme_minimal(base_size = 10) +facet_wrap(~periodo)


ggplot()+geom_sf(data = graf_all ,aes(fill=dose1),size=.15)+
  scale_fill_gradient(name = "Porcentagem de doses",low="lightgrey", high="red",na.value = "white")  + 
  theme_minimal(base_size = 10) +facet_wrap(~periodo)
ggplot()+geom_sf(data = graf_all ,aes(fill=dose2),size=.15)+
  scale_fill_gradient(name = "Porcentagem de doses",low="lightgrey", high="red",na.value = "white")  + 
  theme_minimal(base_size = 10) +facet_wrap(~periodo)
ggplot()+geom_sf(data = graf_all ,aes(fill=reforco),size=.15)+
  scale_fill_gradient(name = "Porcentagem de doses",low="lightgrey", high="red",na.value = "white")  + 
  theme_minimal(base_size = 10) +facet_wrap(~periodo)




#### let x y
todo_periodo <- 
  bind_rows(
  periodo1 %>% mutate(periodo=1),
  periodo2 %>% mutate(periodo=2),
  periodo3 %>% mutate(periodo=3))

ggplot(todo_periodo,aes(x=IDHM,y=let)) + geom_point() +facet_wrap(~periodo)


ggplot(todo_periodo,aes(x=Idade_mediana,y=let)) + geom_point() +facet_wrap(~periodo)
ggplot(todo_periodo,aes(x=densidade2021,y=let)) + geom_point() +facet_wrap(~periodo)
ggplot(todo_periodo,aes(x=Risco,y=let)) + geom_point() +facet_wrap(~periodo)
ggplot(todo_periodo,aes(x=dose1,y=let)) + geom_point() +facet_wrap(~periodo)
ggplot(todo_periodo,aes(x=dose2,y=let)) + geom_point() +facet_wrap(~periodo)
ggplot(todo_periodo,aes(x=reforco,y=let)) + geom_point() +facet_wrap(~periodo)

################


ggplot(todo_periodo,aes(x=let)) + geom_histogram() +facet_wrap(~periodo)
ggplot(todo_periodo,aes(x=Risco)) + geom_histogram() +facet_wrap(~periodo)
ggplot(todo_periodo,aes(x=Idade_mediana)) + geom_histogram() +facet_wrap(~periodo)
ggplot(todo_periodo,aes(x=let)) + geom_histogram() +facet_wrap(~periodo)
ggplot(periodo1,aes(x=IDHM)) + geom_histogram()
ggplot(periodo1,aes(x=densidade2021)) + geom_histogram()
