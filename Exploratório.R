source('init.R',encoding='UTF-8')


periodo1 <- read_csv('Cortes/Periodo 1/Banco 04.02.2020 a 04.11.2020.csv')
periodo2 <- read_csv('Cortes/Periodo 2/Banco 01.03.2021 a 30.11.2021.csv')
periodo3 <- read_csv('Cortes/Periodo 3/Banco 01.12.2021 a 17.03.2022.csv')



##############
Banco_Idade %>% filter(Data %within% interval(dmy('04/02/2020'),dmy('04/11/2020')))
Banco_Idade %>% filter(Data %within% interval(dmy('01/03/2021'),dmy('30/11/2021')))
Banco_Idade %>% filter(Data %within% interval(dmy('01/12/2021'),dmy('17/03/2022')))




#####################



