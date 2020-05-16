# 1a Aula de Inferência Estatística

# Um comentário sobre obtenção de help no R
?sum

# Operações Aritméticas Básicas
2+3 # Adição
2-3 # Subtração
2*3 # Multiplicação
2/3 # Divisão
2^3 # Potência
10 %% 3 # Mod ou Resto Inteiro da Divisão
10 %/% 3 # Int ou Parte Inteira de uma Divisão

# Variáveis
a # a ainda não existe como variável
a = 2 # Variáveis numéricas
b = 3
a+b

nome = "Gustavo" # Variável tipo string
nome

# Conjuntos de valores em uma variável (Vetor 1D)
c(10, 9, 9.5, 7, 4.5, 8, 7.5, 8.5, 9, 5.5)
notas = c(10, 9, 9.5, 7, 4.5, 8, 7.5, 8.5, 9, 5.5)
notas <- c(10, 9, 9.5, 7, 4.5, 8, 7.5, 8.5, 9, 5.5)
c(10, 9, 9.5, 7, 4.5, 8, 7.5, 8.5, 9, 5.5) -> notas

notas

# Operação com um único resultado
mean(notas)
sd(notas)
median(notas)
sum(notas)
length(notas)

# Operações executadas em cada um dos elementos de notas
notas
notas + 1
notas^2
sqrt(notas)

# Acesso aos valores individuais de notas
notas
notas[2]
notas[10]

2:9
notas[c(2,3,4,5,6,7,8,9)]
notas[2:9]

notas
notas[5] = 6
notas

notas[-3]

# Variáveis com linha e coluna: data frames
nomes = c("Andre", "Bernardo", "Camila", "Eduardo", "Fernanda",
          "Guilherme", "Helio", "Irma", "Juliana", "Karin")
sexo = c("M","M","F","M","F","M","M","F","F","F")
notas = c(10, 9, 9.5, 7, 4.5, 8, 7.5, 8.5, 9, 5.5)

turma1 = data.frame(aluno = nomes,
                    genero = sexo,
                    prparcial = notas)
View(turma1)

# Biblioteca de Manipulação de Data Frames (tibbles)
install.packages("dplyr")
library(dplyr)
turma1 = tibble(aluno = nomes,
                genero = sexo,
                prparcial = notas)
turma1

# Passar dados do R para o ambiente
write.csv2(turma1, "turma1.csv", row.names = FALSE)

# Ler dados do ambiente para o R
read.csv2("turma1.csv")
read.csv2("turma1.csv") -> turma1b

# Visualização dos dados
install.packages("ggplot2")
library(ggplot2)
mpg
?mpg

# Tomando por base os valores presentes no data frame mpg
#  podemos dizer que carros com motor maior consomem mais 
#  combustível?
View(mpg)

# Nomes das variáveis
names(mpg)

# Motor maior -> tamanho do motor -> cilindrada -> displ
summary(mpg$displ) # variável contínua

# Consumo maior -> menor no.de milhas (km) por galão (l)
# Consumo maior -> menor desempenho 
# Consumo maior -> menor hwy (mpg na estrada)
summary(mpg$hwy) # variável continua

# Gráfico de pontos (dispersão) no R básico
plot(mpg$displ, mpg$hwy)

# Gráfico de pontos (dispersão) através do ggplot2
library(ggplot2)
ggplot(mpg, aes(x=displ, y=hwy)) + geom_point()

# Apesar de no geral a tendência ter sido confirmada
#  existem pontos que nitidamente estão fora dela?
#  Em outras palavras "outliers"
ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point(color='blue')

ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point(aes(color=class))

# Visualizar um modelo de previsão do
#  hwy a partir do displ
ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point(aes(color=class)) + 
  geom_smooth()

ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point(aes(color=class)) +
  geom_smooth(method = "lm",
              formula = 'y~x')

unique(mpg$drv)

ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point(aes(color=class)) +
  geom_smooth(method = "lm",
              formula = 'y~x',
              aes(lty = drv))

# Será que a partir de uma certa cilindrada
#  o aumento de tamanho do motor passa a 
#  ser feito pelo aumento do número de 
#  cilindros
unique(mpg$cyl)

ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point(aes(color = class,
                 size = cyl)) +
  geom_smooth(method = "lm",
              formula = 'y~x',
              aes(lty=drv))

# Vamos dividir o gráfico com todos os
#  seus detalhes, um para cada fabricante
ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point(aes(color = class,
                 size = cyl)) +
  geom_smooth(method = "lm",
              formula = 'y~x',
              aes(lty = drv)) +
  facet_wrap(~manufacturer)

# Exercícios para treino:
# R for Data Science (online)
# https://r4ds.had.co.nz
# Cap.3 - Data Visualization (ggplot2)

# Tipos de variáveis
# Numéricas
#   Contínuas (com vírgula)
#   Discretas (sem vírgula)
#
# Categóricas
#   Nominais - não admitem ordenação
#   Ordinais - admitem ordenação

# 2a Aula de Inferência Estatística
# Filtragem de dados em uma tabela (dplyr) 
# Mesclagem de tabelas (dplyr)
# Ajuste de formato em tabelas (tidyr)

# dplyr em uma tabela
install.packages("dplyr")
library(dplyr)

dados <- tibble(cor = c("azul","preto","preto","azul","azul"),
                valor = c(1,2,3,4,5))
dados

# filter, select, arrange, mutate, group_by, summarise

# FILTRAGEM DE LINHAS
filter(dados, cor == "azul")
filter(dados, valor > 2)
filter(dados, cor != "azul") # not, não, !
filter(dados, valor > 2 & cor == "preto") # and, e, &
filter(dados, cor == "azul" | valor < 3) # or, ou, |
filter(dados, cor %in% c("azul","vermelho","verde")) # "in"

# SELEÇÃO DE COLUNAS
select(dados, cor)
select(dados, -cor)
select(dados, valor, cor)

teste = "cor"
select(dados, !!teste)
select(dados, cor:valor)

# ORDENAÇÃO DE LINHAS
arrange(dados, cor)
arrange(dados, desc(valor))
arrange(dados, -valor)
arrange(dados, cor, desc(valor))

# CRIAÇÃO DE COLUNAS
mutate(dados, dobro = 2*valor)
mutate(dados, nivel = ifelse(valor > 3, "alto", "baixo"))
mutate(dados, resultado = (cor=="azul"))
mutate(dados, atraso1 = lag(cor,1,"nenhum"))
mutate(dados, atraso2 = lag(cor,2))
mutate(dados, avanco1 = lead(valor,1))
mutate(dados, avanco2 = lead(valor,2))

# AGRUPAMENTO E RESUMO DE DADOS
dados
summarise(dados, total = sum(valor))

group_by(dados, cor) -> dados2
dados2
summarise(dados2, total = sum(valor))

# Exercícios para treino:
# R for Data Science (online)
# https://r4ds.had.co.nz
# Cap.5 Data Transformation (dplyr 1 tabela)

# mesclagem de tabelas de dados - dplyr em 2 tabelas
library(dplyr)
x <- tibble(nome = c("John","Paul","Ringo","Harrison","Peter"),
            instrumento = c("guitarra","baixo","bateria","guitarra","teclado"))
x

y <- tibble(nome = c("John","Paul","Ringo","Harrison","Stuart","Davies"),
            banda = c(TRUE,TRUE,TRUE,TRUE,FALSE,FALSE))
y

# inner_join junta as colunas dos dois e 
#  as linhas das colunas de comparação cujos
#  valores aparecem em ambos os data frames
inner_join(x, y, by=c("nome"="nome"))

# preserva no da esquerda apenas os valores
#  que aparecem na coluna de comparação no 
#  2o data frame. Junta as colunas de ambos
left_join(x, y, by=c("nome"="nome"))

# preserva no da direita apenas os valores
#  que aparecem na coluna de comparação no 
#  1o data frame. Junta as colunas de ambos
right_join(x, y, by=c("nome"="nome"))

# preserva no da esquerda apenas os valores
#  que aparecem na coluna de comparação no
#  da direita. Não junta colunas
semi_join(x, y, by=c("nome"="nome"))
semi_join(y, x, by=c("nome"="nome"))

# preserva no da esquerda apenas os valores
#  que NÃO aparecem na coluna de comparação no
#  da direita. Não junta colunas
anti_join(x, y, by=c("nome"="nome"))
anti_join(y, x, by=c("nome"="nome"))

# junta todas as linhas das colunas de comparação.
# insere NA quando o valor não existir. Junta
# todas as colunas.
full_join(x, y, by=c("nome"="nome"))

# Exercícios para treino:
# R for Data Science (online)
# https://r4ds.had.co.nz
# Cap.13 Relational Data (dplyr 2 tabelas)

# Formas de Carregamento de dados

# 1a forma: leitura de .csv
# read.csv2 <- leitor de dados nativo do R
# fread <- pacote 'data.table' MUUUITO + rápido

# 2a forma: leitura por RODBC
# library(RODBC)
# conn <- odbcConnectExcel2007("arquivo.xlsx")
# dados <- sqlFetch(conn, "planilha")
# odbcClose(conn) <- Prático, porém MUUUITO lento

# Modificação de Tabelas (tidyr)
install.packages("tidyr")
library(tidyr)

#O que é o formato "tidy"?
# Cada coluna representa apenas uma variável
# Cada linha representa um registro independente
# Cada célula (linha x coluna) contém um único valor
table1

table2
spread(table2, key='type', value='count')

table4a
gather(table4a, key='year', value='cases', '1999','2000') -> table4a2
table4a2

table4b
gather(table4b, key='year', value='population', '1999':'2000') -> table4b2
table4b2

inner_join(table4a2, table4b2, by=c("country"="country", "year"="year"))

# Modificação de colunas em uma tabela (tidyr)
table3
separate(table3, rate, into=c("cases","population"), sep="/") -> table3b
mutate(table3b, cases = as.numeric(cases), 
                population = as.numeric(population))

# Operações no formato função(dados)
table5
unite(table5, ano, century, year, sep="") -> table5b; table5b
mutate(table5b, ano = as.integer(ano)) -> table5c; table5c
separate(table5c, rate, into=c("cases","population"), sep="/") -> table5d; table5d

# Operações no formato dados %>% função()
table5 %>% unite(ano, century, year, sep="") %>%
  mutate(ano = as.integer(ano)) %>%
  separate(rate, into=c("cases","population"), sep="/") -> table5d
table5d

# Exercícios para treino:
# R for Data Science (online)
# https://r4ds.had.co.nz
# Cap.12 Tidy Data (tidyr)

# Estruturas avançadas de dados. 
# listas e apply

# apply
dados <- tibble(col1 = c(1,2,3,4,5),
                col2 = c(10,20,30,40,50),
                col3 = c(100,200,300,400,500))
dados

# Como calcular a média de todas as colunas do data frame dados?
apply(dados, 2, mean)

# Como calcular a soma dos valores de cada linha do data frame dados?
apply(dados, 1, sum)

# Vamos detalhar melhor a função que está sendo aplicada às colunas do 
#  data frame dados
apply(dados, 2, function(x){ mean(x) })

# Fazendo o mesmo para a função que está sendo aplicada às linhas do 
#  data frame dados
apply(dados, 1, function(x){ sum(x) })

# Vamos criar a nossa função em separado e chama-la no apply
minhamedia <- function(x) {
  media <- mean(x)
  return(media)
}

apply(dados, 2, minhamedia)

# listas
a <- 1 #variável
b <- c("Jose","Faria") #vetor
d <- dados #dataframe

minha_lista <- list(a, b, d)
minha_lista

minha_lista[1] # como lista de um elemento
minha_lista[[1]] # como o elemento da lista

minha_lista[2]
minha_lista[[2]]

minha_lista[3]
minha_lista[[3]]

# Exercícios para treino:
# R for Data Science (online)
# https://r4ds.had.co.nz
# (listas, cap.20-vectors) 
# (apply, cap.21-iteração)

# Para a próxima aula:
# Introdução aos modelos estatísticos
# R for Data Science (online)
# https://r4ds.had.co.nz
# Caps.23, 24, 25 (modelr) (group_nest)
