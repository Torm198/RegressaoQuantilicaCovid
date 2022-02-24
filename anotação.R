vacinacao_sp %>% pivot_wider(names_from=vacina_descricao_dose,values_from = n) %>%mutate(esquema=`2ª Dose`+`Dose Adicional`+`Reforço`+`Única`+`2ª Dose Revacinação`+`3ª Dose`+`1º Reforço`)
