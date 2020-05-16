# 2o Dia, 3a Aula

# Para a próxima aula:
# Introdução aos modelos estatísticos
# R for Data Science (online)
# https://r4ds.had.co.nz
# Caps.23, 24, 25 (modelr) (group_nest)

# O que é um modelo?
# Você recebe um conjunto de dados brutos e tente encontrar uma
#  expressão matemática que reproduza o padrão deste dados.
#  Sempre há espaço para algum erro nas previsões produzidas pelos
#  nossos modelos. Quanto menor o erro, teoricamente melhor será o
#  nosso modelo. Vamos para um exemplo.

# Bibliotecas já instaladas
library(ggplot2) #biblioteca de gráficos
library(dplyr)   #biblioteca de manipulação de dados
library(tidyr)   #biblioteca de transformação de tabelas

# Bibliotecas por instalar
#install.packages("data.table")
library(data.table) # biblioteca de leitura de dados (rápida)

#install.packages("modelr")
library(modelr)  #biblioteca de manipulação de modelos

#install.packages("gapminder")
library(gapminder) #base de dados a ser utilizada nesta aula
gapminder

# Vamos primeiro "desmontar" a gapminder no Excel.
# Para isso fazemos o download correspondente
write.csv2(gapminder, "gapminder.csv", row.names=FALSE)

# Após desmontar a base gapminder no Excel, gerar os .csvs
# correspondentes e fazer o upload dos mesmos, fazemos o
# carregamento na memória do R e o ajuste de colunas dos
# .csvs com nomes de coluna na primeira linha


# Dificil de ler o formato resultante de fread
fread("populacao.csv", sep=";", dec=",") -> populacao
populacao

# Ler e transformar em um tibble. Facilita a visualizacao
fread("populacao.csv", sep=";", dec=",") %>% as_tibble() -> populacao
populacao

# Desprezar a primeira linha por conta da frase explicativa vinda
#  do Excel. 
fread("populacao.csv", sep=";", dec=",", skip=1) %>% 
  as_tibble() -> populacao
populacao

# Vemos que a primeira linha por conter números foi ignorada como 
#  nome de coluna. Precisamos forçar a sua leitura com a opção 
# header=TRUE
fread("populacao.csv", sep=";", dec=",", skip=1, 
      header=TRUE) %>% as_tibble() -> populacao
populacao

#  Observamos que o data frame populacao possui as colunas de 
#  '1952' até '2007' como character e queremos que as mesmas sejam 
#  convertidas para números. Vamos aprender primeiro como eliminar
#  os caracteres de formatação n

# 
mutate(populacao, '1952' = ~gsub("\\.","",.))

mutate(populacao, vars('1952') = ~gsub("\\.","",.))

mutate_at(populacao, vars('1952'), ~gsub("\\.","", .))

mutate_at(populacao, vars('1952'), ~gsub("\\.","", .)) %>%
  mutate_at(vars('1952'), ~as.numeric(.))

# Para executar o gather de várias colunas em sequência 
#  utilizamos a opção de ":" entre as colunas inicio e fim
#  a operação de gather.
gather(populacao, key = 'year', value = 'populacao', 
       '1952':'2007') %>% as_tibble() -> populacao_gather
populacao_gather

# Podemos fazer a mesma operação para expectativa_de_vida
#  em duas conforme abaixo:
fread("expectativa_de_vida.csv", sep=";", dec=",", skip=1,
      header = TRUE) -> expectativa
gather(expectativa, key = 'year', value = 'lifeExp',
       '1952':'2007') %>% as_tibble() -> expectativa_gather

# Podemos gerar esta mesma operação com pipes em uma linha
#  de comando apenas, conforme abaixo:
fread("pibpercapita.csv", sep=";", dec=",", skip=1,
      header = TRUE) %>% 
      gather(key='year', value='gdpCap', '1952':'2007') %>%
  as_tibble() -> pib_gather
pib_gather


# 1o com apply
nlin = length(populacao$country)
ncol = length(populacao)
apply(populacao[,3:ncol], 2, function(x) { 
  numeros = gsub("\\.","", x);
  resultado = as.numeric(numeros);
  return(resultado)}) -> populacao[, 3:ncol]
populacao

# 2o com mutate_at
# mutate_at(dataframe, números de coluna, ~operação(.))
#"." -> indica que o valor de cada coluna 
#       será usado como input
#"~" -> indica que o resultado da operação deverá 
#       ser salvo na própria coluna
mutate_at(populacao, c(3:14), ~as.numeric(gsub("\\.","",.)))

# Ajuste de formato na coluna gdpCap

# gsub função de substituição de caracteres em uma string
gsub("a","b","Gustavo")

# substituição de caracteres especiais, tipo $ e . 
#  não devemos utilizar $ e . diretamente como caracteres
#  na substituição pois o comportamento resultante será 
#  imprevisível
gsub("$","","$1.456") # Aparentemente sem efeito

# Devemos utilizar o caracter de escape. Porém $ em si não
#  é um controle, portanto escapar o caracter $ também produzirá
#  um erro
gsub("\$","","$1.456") # Erro pois $ não é um controle em sí

#  Devemos portanto escapar a \ com duas barras \\ para que o
#   $ deixe de ser considerado um caracter especial e passe a 
#   ser considerado um caracter comum, passível de substituição
#   pela gsub
gsub("\\$","","$1.456") # Agora funcionará

#  O comportamento do "." é similar. Apenas o "." produz um resultado
#   não esperado na substituição
gsub(".","","1.456")

#  Escapar o "." como um controle produz um erro
gsub("\.","","1.456")

# Devemos portanto escapar a \ com duas \\ para que o "." seja
#  considerado um caracter comum e seja substituido pela gsub
gsub("\\.", "", "1.456")

# Então as operações de limpeza na coluna do data frame deverão ser
#  executadas conforme a seguir
pib2_gather$gdpCap2 <- gsub("\\$","", pib2_gather$gdpCap)
pib2_gather

pib2_gather$gdpCap2 <- gsub("\\.","", pib2_gather$gdpCap2)
pib2_gather

pib2_gather$gdpCap2 <- as.numeric(pib2_gather$gdpCap2)
pib2_gather
