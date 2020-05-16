install.packages("tidyverse")
install.packages("ggplot2")
install.packages("modelr")
install.packages("gapminder")
install.packages("data.table")
install.packages("redxls")
install.packages("redxl")

library(tidyverse)
library(ggplot2)
library(modelr)
library(gapminder)
library(dplyr)
library(data.table)
library(readxl)
library(readxls)

help(spread)

view(gapminder)
write.csv2(gapminder, "gapminder.csv", row.names = FALSE)
teste = tibble(test)

test = gapminder %>% select(-pop, -gdpPercap)
test2 = gapminder %>% select(-pop, -lifeExp)
test3 = gapminder %>% select(-gdpPercap, -lifeExp)

lifeExpct = test %>% spread(year ,lifeExp, c("1952","1957","1962","1967","1972","1977","1982","1987","1992","1997","2002","2007"))
pop = test3 %>% spread(year, population, c("1952","1957","1962","1967","1972","1977","1982","1987","1992","1997","2002","2007"))
gdp = test2 %>% spread(year, gdpPercap, c("1952","1957","1962","1967","1972","1977","1982","1987","1992","1997","2002","2007"))

help(fread)

fread("populacao2.csv", sep=";", dec=",") -> populacao
fread("populacao2.csv", sep=";", dec=",", header = TRUE) -> populacao2
fread("expectativa_de_vida2.csv", sep=";", dec=",", header = TRUE) -> expectativa2
fread("pibpercapita2.csv", sep=";", dec=",", header = TRUE) -> pibpercapita2
fread("populacao.csv", sep=";", dec=",", header = TRUE) -> populacao
fread("populacao.csv", sep=";", dec=",", header = TRUE, skip = 1) -> populacao

#GATHER -> KEY = "Nome da nova Coluna", VALUE = "Valor que será Agrupado", VETOR = "Valores Unitários"
#SPREAD -> KEY = "Nome da nova Coluna", VALUE = "Valor que será Agrupado", VETOR = "Valores Unitários"

gather(populacao2, key = "year", value = "populacao", c("1952":"2007")) -> populacao2_gather
gather(expectativa2, key = "year", value = "lifeExp", c("1952":"2007")) -> expectativa2_gather
gather(pibpercapita2, key = "year", value = "gdpCap", c("1952":"2007")) -> pibpc2_gather

pibpc2_gather = pibpc2_gather %>% tibble() 
pibpc2_gather
help(gsub)
pibpc2_gather$gdpCap = gsub("\\$","", pibpc2_gather$gdpCap)
pibpc2_gather$gdpCap = gsub('\\.', "",pibpc2_gather$gdpCap)  %>% as.numeric()
pibpc2_gather

tibble(populacao) -> populacao

populacao
cols = c(3:14)
df = populacao

df[,cols] = apply(df[,cols], 2, function(x) gsub("\\.","",x))
df[,cols] = apply(df[,cols], 2, function(x) as.numeric(x))

df

help(mutate_at)
populacao = df
populacao

gather(populacao, key = "year", value = "populacao", c("1952":"2007")) -> populacao_gather


x = expectativa_gather
y = pibpc2_gather

help(inner_join)
df = right_join(x,y, by = c("country" = "country" , "year" = "year", "continent" = "continent"))

x = df
y = populacao_gather
df = right_join(x,y, by = c("country" = "country" , "year" = "year", "continent" = "continent"))

tibble(df)

View(gapmider)

filter(gapminder, country == "Brazil") -> br_dados

lm (data = br_dados, formula = lifeExp ~year) -> br_modelo
br_modelo

predict(br_modelo, br_dados)

add_predictions(data = br_dados, model = br_modelo) %>% add_residuals(model = br_modelo) %>% View()

gapminder %>% group_by(country)
gapminder %>% group_by(country) %>% nest() #Dataframe Aninhado
gapminder %>% group_nest(country) -> gapminder_nested

#Apartir do dataframe Aninhado aplicar a função de modelos lineares utilizando o apply.

apply(gapminder_nested, 1, function(df) {
  
  lm(data = df$data, formula = lifeExp ~ year)
  
})


apply(gapminder_nested, 1, function(df) {
  lm(data = df$data, formula = lifeExp ~ year)
}) -> gapminder_nested$modelo

#Aplicar a cada linha da coluna data do dataframe gapminder_nested o modelo correspondente a linha
apply(gapminder_nested, 1, function(df){
  add_predictions(data = df$data, model = df$modelo)
})

apply(gapminder_nested, 1, function(df){
  add_predictions(data = df$data, model = df$modelo)
}) -> gapminder_nested$data

gapminder_nested$data

apply(gapminder_nested, 1, function(df){
  add_residuals(data = df$data, model = df$modelo)
}) -> gapminder_nested2$data


gapminder_nested %>% tidyr::unnest(data) %>% ungroup() -> gapminder_2

View(gapminder_2)

