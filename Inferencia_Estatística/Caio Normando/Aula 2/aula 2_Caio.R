#Aula 2 - 16/05/2020

#O que é um modelo?
#Você recebe um conjunto de dados brutos e tenta encontrar uma expressão matemática que reproduza o padrão destes
#dados. Sempre há espaço para algum erro nas previsões produzidas pelos nossos modelos. Quanto menor o erro,
#teoricamente melhor será o nosso modelo.

library(ggplot2)
library(dplyr)

library(tidyr)
install.packages('modelr')
install.packages('gapminder')
library(modelr) #manipulação de modelos
library(gapminder) #base de dados utilizada

View(gapminder)
unique(gapminder$year)
?gapminder
write.csv2(gapminder, "gapminder.csv", row.names=FALSE)

install.packages("data.table")
library(data.table)

#Carregando os dataframes:
fread("populacao2.csv", sep=";", dec=",", header=TRUE) -> populacao2
fread("expectativa_de_vida2.csv", sep=";", dec=",", header=TRUE) -> expectativavida2
fread("pibpercapita2.csv", sep=";", dec=",", header=TRUE) -> pibpercabita2

#Criando o tibble , convertendo para Integer as colunas de ano e unificando-as na coluna "year"
tibblePopulacao2 <- tibble(populacao2) %>% 
  mutate_at(vars("1952":"2007"), ~as.integer(gsub("\\.","",.))) %>% 
  gather(key='year', value='populacao','1952':'2007')

#Criando o tibble , convertendo para Double as colunas de ano e unificando-as na coluna "year"
tibbleExpectativaVida2 <- tibble(expectativavida2) %>%
  mutate_at(vars("1952":"2007"), ~as.numeric(gsub("\\.","",.))) %>%
  gather(key='year', value='lifeExp','1952':'2007')

#Criando o tibble , convertendo para Double as colunas de ano e unificando-as na coluna "year"
tibblePibPerCapita2 <- tibble(pibpercabita2) %>%
  mutate_at(vars("1952":"2007"), ~as.numeric(gsub("\\$","",.))) %>%
  gather(key='year', value='GDP','1952':'2007')

#Unindo os data frames e convertendo a coluna year para Integer
tibbleJoin <- inner_join(tibblePopulacao2, tibbleExpectativaVida2,
                 by=c('continent'='continent','country'='country','year'='year')) %>% 
  inner_join(tibblePibPerCapita2,
                          by=c('continent'='continent','country'='country','year'='year')) %>%
  mutate(year = as.integer(year))

#Reordenando as colunas e ordenando por países
tibbleJoin <- select(tibbleJoin, country, continent, year, lifeExp, populacao, GDP) %>%
  arrange(country)

View(tibbleJoin)

gapminder
