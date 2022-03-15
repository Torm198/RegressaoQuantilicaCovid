corte_banco <- function(Data_inicio,Data_final){
  
  
  int <- interval(dmy(Data_inicio),dmy(Data_final))
  
  a <- SEADE %>% filter(Data %within% int) %>% group_by(Municipio,Risco) %>% summarise(n=sum(n)) %>%
    pivot_wider(names_from = Risco,values_from = n) %>% summarise(Risco=sum(`1`)/sum(`0`+`1`))
  
  
  b <- SEADE %>% filter(Data %within% int)  %>%
    pivot_wider(names_from=Obito,values_from=n,values_fill = 0)%>%
    group_by(Municipio) %>%
    mutate(let=sum(`1`)/sum(`0`+`1`)) %>%
    select(-c(Data,Risco,`0`,`1`)) %>% unique()
  
  c <- vacinacao_sp %>% filter(vacina_dataAplicacao %within% int)  %>%
    group_by(estabelecimento_municipio_codigo) %>% arrange(vacina_dataAplicacao) %>%
    summarise(Doses_diárias=mean(n)) %>% rename(Codigo=estabelecimento_municipio_codigo)
  
  
  d <- Banco_Idade %>% filter(Data %within% int) %>% group_by(Municipio) %>% summarise(Idade_mediana=median(Idade,na.rm = T))
  
  e <- vacinacao_sp %>% filter(Data %within% int) %>%
    group_by(estabelecimento_municipio_codigo) %>% arrange(vacina_dataAplicacao) %>%
    summarise(Doses_diárias=mean(n)) %>% rename(Codigo=estabelecimento_municipio_codigo)
  
  banco_lqr <- left_join(b,a) %>% left_join(c) %>% left_join(d) %>% left_join(e) %>%
    mutate(esquema=esquema/Pop) %>% filter(!is.na(Codigo))
  
  return(banco_lqr)
}