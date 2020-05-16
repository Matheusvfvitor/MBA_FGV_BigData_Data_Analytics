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

getwd()

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

install.packages("readxl")
library(readxl)

setwd("/cloud/project/Inferencia_Estatística/Matheus Vitor/Aula2")
mba <- read_excel("mba.xlsx", sheet ="dados")
View(mba)

mean(mba$salario)
median(mba$salario)
hist(mba$salario)

filter (mba, salario >= 0) -> mba2

mean(mba2$salario)
median(mba2$salario)
hist(mba2$salario)

filter (mba, salario > 0) -> mba3
mean(mba3$salario)
median(mba3$salario)
hist(mba3$salario)

mba3$sexo %>% unique()

#Gráfico ideal para observar a influência de uma variável categórica em uma variável contínua : Boxplot

boxplot(mba3$salario ~mba3$sexo)

#Criacao de um modelo linear baseado em uma variavel categórica sexo
# O modelo será do tipo: Salário Previsto =  b0 + b1*sexo , Onde sexo Feminino = 0 e sexo Masculino = 1

lm(data = mba3 , formula = salario ~ sexo) -> modelo1

summary(modelo1)
# Sal^ 98524 + 6447*(sexo)

#Nesta análise a qual segue o padrão clássico da estatística vamos trabalhar como critério para escolha das variáveis o p-value

#Vamos remover o outlier feminino que pode estar distorcendo o valor

View(mba3)

mba3 %>% filter(salario < max(mba3$salario)) -> mba4
lm(data = mba4 , formula = salario ~ sexo) -> modelo1

boxplot(mba4$salario ~mba4$sexo)
summary(modelo1)

#Criando explicitamente a variável categórica
mutate(mba4, codSexo = (sexo == "Masculino")*1) -> mba5
View(mba5)

#Criando um modelo para a variável categórica transformada ou variável dumificada, ou variável dummy
lm(data = mba5, formula = salario ~ codSexo) -> modelo3
summary(modelo3)

#Influência da variável desempenho
boxplot(mba5$salario ~mba4$desempenho)

#Criando o modelo diretamente no R
lm(data = mba5, formula = salario ~ desempenho) -> modelo4
summary(modelo4)
# sal^ = 103612 + 2717*Excelente - 8017*Fraco - 5293*Regular
#Como pode ser visto o esquema de valores escolhidos para a variável categórica desempenho não é o ideal.
#O ideal seria o seguinte :
#   Regular          Bom           Excelente
#         0            0                   0 -> Fraco
#         1            0                   0 -> Regular
#         0            1                   0 -> Bom
#         0            0                   1 -> Excelente

#Criação de um sistema de dummies com esta sequencia, mas os valores númericos ainda não se apresentam de forma correta
#pois aparece que a relação entre as variáveis é igual.

match(mba5$desempenho, c("Fraco", "Regular", "Bom", "Excelente")) -> mba5$desempenho_num
lm(data = mba5, formula = salario ~ desempenho_num) -> modelo5
summary(modelo5)
#Sal^ 91570 + 3744*desempenho_num


#O Formato ideal de transformação de uma variável categórica de varios níveis envolve a criação de r-1
#de variáveis dummy para cada categoria de variável de entrada, sendo 0 atribuida a todas as dummys para formar o caso base.

mutate(mba5, 
       codExcelente = (desempenho == "Excelente")*1,
       codBom =(desempenho == "Bom")*1,
       codRegular =(desempenho == "Regular")*1,
       ) -> mba6

View(mba6)
lm(data = mba6, formula = salario ~ codRegular + codBom + codExcelente) -> modelo6
summary(modelo6)

#Analisar o efeito de Experiência
lm(data = mba6, formula = salario ~ experiencia) -> modelo7
summary(modelo7)

#Analisar o efeito da idade
lm(data = mba6, formula = salario ~ idade) -> modelo8
summary(modelo8)

#Analisar o efeito combinado de experiência mais idade
lm(data = mba6, formula = salario ~experiencia + idade ) -> modelo9
summary(modelo9)

#Multicolinearidade (escolher uma variável das duas)
cor(mba6$experiencia, mba6$idade)

#Como exercício a montagem de modelos com as demais variáveis numéricas (desempenho).
#Nenhuma será significativa.

lm(data = mba6, formula = salario ~ sexo + experiencia + codExcelente) -> modelo10
summary(modelo10)
#sal^ = 89037 + 10027*sexoMasculino + 994*Experiência + 6468*CodExcelente



