analise <- vacinacao_sp %>% pivot_wider(names_from=vacina_descricao_dose,values_from = n,values_fill = 0) %>%
  group_by(vacina_dataAplicacao,estabelecimento_municipio_codigo,estabelecimento_uf)%>%
  summarise(dose1=sum(`1ª Dose`+Dose+`1ª Dose Revacinação`,na.rm=T),
            dose2=sum(`2ª Dose`+`Dose Adicional`+`Única`+`2ª Dose Revacinação`,na.rm=T),
            reforco=sum(Reforço+`3ª Dose`+`1º Reforço`))  %>%
  arrange(vacina_dataAplicacao) %>%
  group_by(estabelecimento_municipio_codigo) %>% mutate(dose1=cumsum(dose1),
                                                        dose2=cumsum(dose2),
                                                        reforco=cumsum(reforco)) %>%
  summarise(across(dose1:reforco,max))
