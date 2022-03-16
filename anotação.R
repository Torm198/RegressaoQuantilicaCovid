analise <- vacinacao_sp %>% pivot_wider(names_from=vacina_descricao_dose,values_from = n,values_fill = 0) %>%
  group_by(vacina_dataAplicacao,estabelecimento_municipio_codigo,estabelecimento_uf)%>%
  summarise(esquema=sum(`2ª Dose`+`Dose Adicional`+`Reforço`+`Única`+`2ª Dose Revacinação`+`3ª Dose`+`1º Reforço`,na.rm=T))  %>%
  group_by(estabelecimento_municipio_codigo) %>% mutate(esquema=cumsum(esquema))


vacinacao_sp %>% pivot_wider(names_from=vacina_descricao_dose,values_from = n,values_fill = 0) %>%
  group_by(vacina_dataAplicacao,estabelecimento_municipio_codigo,estabelecimento_uf)%>%
  is.na() %>% apply(., 2, any)
