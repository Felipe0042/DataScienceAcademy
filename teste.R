# Aprendendo a usar a linguagem r


# abrindo um dataset
dataset_bank <- read.table("bank.csv", header=TRUE,sep=";")
View(dataset_bank)

#Primeiro passo - Criando uma nova coluna de dados 

#acessando valores de uma tabela
table(dataset_bank$job)
#outra forma
table(dataset_bank[[2]])

library(dplyr)
library(ggplot2)

# - %>% -> pipe (função infixa)
# tem como função facilitar a visualização da função
# tem como finalidade tambem ajudar a programar pipelines

#criando grafico de barras dos empregos no banco 
dataset_bank %>%
  group_by(job) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=job, y=n)) +
  geom_bar(stat ='identity') +
  theme(axis.text.x = element_text(angle=90, hjust = 1))


#criando uma coluna no dataset definindo o nivel dos profissionais em termos
#de uso de tecnologia


#mutate - adicionar mais uma coluna ao seu banco de dados
help(mutate)
dataset_bank <- dataset_bank %>%
  mutate(technology_use =
           case_when(
             job == 'admin'         ~ 'medio',
             job == 'blue-collar'   ~ 'baixo',
             job == 'entrepreneur'  ~ 'alto',
             job == 'housemaid'     ~ 'baixo',
             job == 'management'    ~ 'medio',
             job == 'retired'       ~ 'baixo',
             job == 'self-employed' ~ 'medio',
             job == 'services'      ~ 'baixo',
             job == 'student'       ~ 'alto',
             job == 'technician'    ~ 'alto',
             job == 'unemployed'    ~ 'baixo',
             job == 'unknow'        ~ 'baixo',
           ))

View(dataset_bank)

#visualizar conteudo da tabela
qtd <-table(dataset_bank$technology_use)

#em porcentual
round(prop.table(qtd),2)

#criando uma variavel dummy 

dataset_bank <- dataset_bank %>% 
  mutate(defaulted = ifelse(default == 'yes',1,0))


#técnica One Hot Encoding
#converter valor de uma variável categórica para um correspondente numérico

library(caret)
#criando variaveis dummy para todas do conjunto de dados indicado pelo "~."
dmy <- dummyVars("~.", data=dataset_bank)

#pego a função dmy e aplico ao meu conjuto de dados e por fim crio um novo dataframe 
bank.dummies <- data.frame(predict(dmy, newdata = dataset_bank))



#combinação de variaveis 
#AGRUPANDO RELAÇÃO DE CARGOS COM ESTADO CIVIL
dataset_bank %>%
  group_by(job, marital) %>%
  summarise(n = n())


dataset_bank %>%
  group_by(job, marital) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=job, y=n, fill = marital)) +
  geom_bar(stat ='identity', position = "dodge") +
  theme(axis.text.x = element_text(angle=90, hjust = 1))


#Criar variaveis dummy para representar essa uniao
dmy <- dummyVars( ~ job:marital, data=dataset_bank)
bank.cross <- predict(dmy, newdata = dataset_bank)
View(bank.cross)









