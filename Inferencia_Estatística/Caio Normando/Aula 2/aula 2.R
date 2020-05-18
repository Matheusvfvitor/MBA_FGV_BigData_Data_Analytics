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

#1° exercício - modelo de previsão de lifeExp a partir de year para os dados do Brasil (tabela gapminder)
filter(gapminder, country=='Brazil') -> br_dados
#lm = Linear Model
lm(data = br_dados, formula= lifeExp ~ year) -> br_modelo
#Nosso modelo de previsão será: lifeExp^ = -709.9427 + 0.3901*year
br_modelo

#Prever expectativa de vida a partir do ano
predict(br_modelo, br_dados) -> br_prediction

#Adiciona no br_dados as predições do modelo
add_predictions(data = br_dados, model= br_modelo) -> br_dados

#Adiciona no br_dados os resíduos do modelo
add_residuals(data = br_dados, model = br_modelo) -> br_dados

#2° exercício - modelo de previsão de lifeExp a partir de year para os dados de cada país

#Cria uma espécie de dicionário Json para cada país (dataframe aninhado - uma lista de tibbles)
gapminder %>% group_by(country) %>% nest()
#ou
gapminder %>% group_nest(country) -> gapminder_nested

#A partir do data frame aninhado, vamos aplicar a função de modelos lineares a cada um dos conjuntos de dados
#de cada país, gerando 142 modelos, os quais serão salvos em uma nova coluna de gapminder_nested, a qual
#chamaremos de "modelo"

apply(gapminder_nested, 1, function(df){
  lm(data = df$data, formula = lifeExp ~ year)
  #predict(gapminderModel, df$data) -> gapminderPrediction
  #add_predictions(data = df$data, model = gapminderModel)
  #add_residuals(data = df$data, model = gapminderModel) 
}) -> gapminder_nested$modelo



apply(gapminder_nested, 1, function(df){
  add_predictions(data = df$data, model = df$modelo)
}) -> gapminder_nested$data

gapminder_nested2 = gapminder_nested

apply(gapminder_nested, 1, function(df){
  add_residuals(data = df$data, model = df$modelo)
}) -> gapminder_nested2$data

gapminder_nested = gapminder_nested2

#Desaninhando a coluna data e desagrupando
gapminder_nested %>% unnest(data) %>% ungroup() -> gapminder2

View(gapminder2)

ggplot(data = gapminder2) + geom_smooth(aes(x=year, y=lifeExp), color='blue') + 
  geom_smooth(aes(x=year, y=pred), color='red') + geom_smooth(aes(x=year, y=resid), color='green') +
  facet_wrap(~country)


#Gráfico de previsões para todos os países
ggplot(gapminder2, aes(x=year, y=pred, group=country)) + geom_line(color='blue', alpha=0.33, aes(group=country))


ggplot(data = br_dados) + geom_smooth(aes(x=year, y=lifeExp), color = 'blue') +
  geom_smooth(aes(x=year, y=pred), color='red')
View(br_dados)



#Criar um modelo de previsão para o salário esperado por um formando de mba a partir dos valores presentes
#na aba dados do arquivo mba.xlsx o qual encontra-se em anexo. Deveremos avaliar se fatores presentes
#nas demais colunas do modelo influenciam ou não o salário que pode ser esperado por um formando dependendo
#de suas características

#1° Ler os dados da planilha sem a necessidade de gerar arquivos csv. Para isso, vamos utilizar o pacote
#readxl e função read_excel

install.packages("readxl")
library(readxl)
mba <- read_excel("mba.xlsx")
View(mba)

#Limpeza de dados. Eliminação de valores indevidos na coluna salário. Determinação visual damaior
#"normalidade" dos dados da coluna

mean(mba$salario)
median(mba$salario)
hist(mba$salario)

filter(mba, salario >= 0) -> mba2
mean(mba2$salario)
median(mba2$salario)
hist(mba2$salario)

filter(mba2, salario > 0) -> mba3
mean(mba3$salario)
median(mba3$salario)
hist(mba3$salario)


#Variáveis possíveis de serem utilizadas em um modelo preditivo do salário inicial de um recém formado
#em um programa de MBA

#Influência da variável sexo (categórica)

#Gráfico ideal para observarmos a influência de uma variável categória em uma variável contínua: boxplot
boxplot(mba3$salario ~ mba3$sexo)
#barra preta - média
#barras superior e inferior - 1,5 x a média pra cima e pra baixo - valores fora dele - outliers

#Criar um modelo linear baseado na variável categórica sexo. O modelo será do tipo: Sal^ = b0+b1*sexo,
#onde sexo(feminino)= 0  e sexo(masculino)=1

lm(data=mba3, formula=salario~sexo) -> modelo1
#Sal^ = 98524 + 6447*sexoMasculino
#sexoMasculino = 1. sexoFeminino = 0. Logo, homens ganham mais que mulheres

summary(modelo1)
#Probabilidade do modelo estar ocorrendo por acaso - p-value

#Nesta análise(a qual segue o padrão clássico da estatística), vamos trabalhar como critério para escolha
# das variáveis do nosso modelo o p-value. O p-value nos dá a probabilidade do nosso modelo estar "errado",
#isto é, a influência detectada pelo modelo nos dados seria fruto do acaso. O melhor então é que o p-value
#seja tão baixo quanto possível. No caso deste exemplo, o p-value deu 9,32% (ideal nesse caso é até 5% <100000 amostras)
#. Portanto, não podemos, com base
#nestes dados, a princípio, inferir que a influência da variável sexo no salário seja significativa. Porém,
#o boxplot não mente e sabemos que o grupo masculino teve um valor em bloco maior que o bloco de valores
#femininos (as caixas do boxplot). O que pode estar ocorrendo?


#Aqui, retiramos o outlier do salário feminino.
filter(mba3,salario < max(salario)) -> mba4
lm(data=mba4, formula=salario~sexo) -> modelo2
boxplot(mba4$salario ~ mba4$sexo)
summary(modelo2)
#Sal^ = 94475+10496*sexoMasculino
#pvalue=0.2% => influencia na saída

#Criando explicitamente a variável categória (feminino = 0, masculino = 1)
mutate(mba4, codSexo=(sexo=='Masculino')*1) -> mba5
lm(data=mba5, formula=salario~codSexo) -> modelo3
summary(modelo3)


#Verificando o salário de alunos em função da influência da coluna desempenho
lm(data=mba5, formula=salario~desempenho) -> modelo4
summary(modelo4)
boxplot(mba5$salario~mba5$desempenho)

#Se repararmos, o p-value é baixo, mas ele não é confiável. Isso porque, por exemplo, o p-value de desempenho
#bom é <2e-16, e o de desempenhoFraco é 5%. Esses até são aceitáveis, pois são menores que 5%. Porém, os outros
#são muito discrepantes (43% e 16%). Isso tira a credibilidade do p-value geral. Esse tipo de coisa costuma
#acontecer com variáveis ordinais. Assim, essa coluna também
#não é um bom parâmetro para influenciar no salário. 

#Criando as dummies para o desempenho
match(mba5$desempenho, c("Fraco","Regular","Bom","Excelente")) -> mba5$desempenho_num
lm(data=mba5, formula=salario~desempenho_num) -> modelo5
boxplot(mba5$salario~mba5$desempenho_num)
summary(modelo5)
#Sal^ = 91570 + 3744*desempenho_num

#O formato ideal de transformação de uma variável ordinal de vários níveis envolve a criação de r-1 
#variáveis dummy (0 ou 1)

mutate(mba5, codExcelente = (desempenho=="Excelente")*1,
       codBom = (desempenho=="Bom")*1,
       codRegular = (desempenho=="Regular")*1) -> mba6
lm(data=mba6, formula=salario~codRegular+codBom+codExcelente) -> modelo6
summary(modelo6)

#O p-value geral continua sendo o mesmo, mas os específicos não. Os valores agora são mais confiáveis.
#O desempenho regular agora tem uma probabilidade de erro muito grande.

#Analisar o efeito de anos de experiencia

lm(data = mba6, formula=salario ~experiencia) -> modelo7
summary(modelo7) #bom fator, p-value bem baixo. Logo, influencia na saída

#Analisar o efeito da idade
lm(data=mba6, formula=salario~idade)->modelo8
summary(modelo8) #Excelente fator, p-value bem baixo. Logo, influencia na saída


#Teoricamente, quanto mais idade uma pessoa tem, mais anos de experiência ela tem. Por isso, não faz sentido
#basear o modelo nas duas.

#Analisar o efeito combinado de experiência + idade
lm(data = mba6, formula = salario~experiencia + idade) -> modelo9
summary(modelo9)
#os p-values específicos são muito altos porque as variáveis idade e experiência tem alta correlação.
cor(mba6$experiencia, mba6$idade) #mais de 50% de correlação, as variáveis começam a perder o significa porque
#a estatística divide a informação entre as duas, e isso causa problemas. Isso se chama multicolinearidade.


#Fica como exercício a montagem de modelos com as demais variáveis numéricas (desempenho 1sem etc.)
#(nenhuma será significativa)
#o modelo final será:

#Modelo de pessoas com desempenho excelente considerando sexo e experiência
lm(data=mba6, formula = salario ~ sexo + experiencia + codExcelente) -> modelo10
summary(modelo10) #p-value baixíssimo, alta correlação e pouquíssima chance de erro por acaso.
#sal^=89037+10027*sexoMasculino + 994.9*experiencia + 6468*codExcelente
