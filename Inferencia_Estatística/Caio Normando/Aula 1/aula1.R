#1a Aula de inferência estatística

2+3 #Ctrl + Enter executa no console essa operação dentro do script.
# Se estiver na linha, não precisa selecioná-la

a = 3
c = 'Caio'
notas = 3
notas <- 3

caio <- 3
caio = 3

notas = c(10,9,8,6.5,10,3)
mean(notas)
sd(notas) #desvio padrão
median(notas)
sum(notas)
length(notas)

notas+1 #Atribui + 1 em cada um dos elementos do vetor
notas ^2 #Eleva ao quadrado cada elemento do vetor
sqrt(notas) #Calcula a raiz quadrada de cada elemento do vetor

notas [3:5]

notas[5] = 6
notas

notas[-1] #remove o elemento da posição 1

notas = c(8, 7, 3.5, 8, 9, 9.5, 6, 10, 8, 7)

nomes = c("André", "Bernardo", "Camila", "Eduardo", "Fernanda",
          "Guilherme", "Helio", "Irma", "Juliana", "Karen")
sexo = c("M", "M", "F", "M", "F", "M", "M", "F", "F", "F")

#Cria a tabela com as colunas de acordo com os vetores passados no mapping
turma1 = data.frame(aluno = nomes, genero = sexo, provaparcial = notas)

View(turma1) #Gera uma tabela na aba ao lado

#Biblioteca de manipulação de data frames (tibbles)
install.packages("dplyr") #instalação
library(dplyr) #Habilitar a biblioteca

#Tibble também é um tipo de dataframe
turma1 = tibble(aluno = nomes, genero = sexo, provaparcial = notas)
turma1

#Criação de Csv
write.csv2(turma1, "turma1.csv", row.names =  FALSE)

#leitura de csv
read.csv2("turma1.csv") -> turma1b

View(turma1b)

#Visualização dos dados
install.packages("ggplot2")
library(ggplot2)
View(mpg)

?mpg

#################################################################

#Tomando por base os valores presentes no dataset mpg, 
#podemos dizer que carros com motor maior consomem mais combustível?

#Motor maior -> tamanho do motor -> cilindrada -> displ


#Resumo estatístico da variável displ do dataset mpg (min, 1st etc.)
summary(mpg$displ) #variável contínua

#Nome das variáveis do dataset
names(mpg)

#Consumo maior -> menor n° de milhas por galão
#Consumo maior -> menor desempenho
#Consumo maior -> menor hwy (miles/gallon na estrada)

summary(mpg$hwy) #variável contínua

#Gráfico de pontos (dispersão) no R básico
plot(mpg$displ , mpg$hwy)

#Gráfico de pontos (dispersão) através do ggplot2
library(ggplot2)
ggplot(mpg, aes(x = displ, y = hwy)) + geom_point()

#Apesar de no geral a tendência ter sido confirmada, existem pontos
# que nitidamente estão fora dela (outliers)?

#Atribuindo cor azul a todos os pontos
ggplot(mpg, aes(x = displ, y = hwy)) + geom_point(color='blue')

#Atribuindo cor de acordo com o mapeamento dos pontos
#em classes diferentes
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color=class))

#Visualizar um modelo de previsão do hwy a partir do displ
ggplot(mpg, aes(x=displ, y=hwy)) + geom_point(aes(color=class)) +
  geom_smooth()

#Tendência de Y em função de X
ggplot(mpg,(aes(x=displ,y=hwy))) + geom_point(aes(color=class)) +
  geom_smooth(method='lm', formula = 'y~x') #linearmodel

#Inserir linhas de tendência de acordo com o tipo de drv (tração)

unique(mpg$drv) #Pegar os valores únicos de drv

#lty = line type
ggplot(mpg, aes(x=displ, y=hwy)) + geom_point(aes(color=class)) +
  geom_smooth(method='lm', formula='y~x', aes(lty = drv))


#Será que a partir de uma certa cilindrada, o aumento do tamanho do motor
#passa a ser feito pelo aumento do número de cilindros?
unique(mpg$cyl)

#size = tamanho da bolinha no gráfico
ggplot(mpg, aes(x=displ, y=hwy)) + geom_point(aes(color=class, size=cyl)) +
 geom_smooth(method='lm', formula='y~x', aes(lty=drv))

# dividir o gráfico com todos os seus detalhes: um para cada fabricante

ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point(aes(color=class, size = cyl)) + #Gráfico de dispersão separando por cores e tamanho
  geom_smooth(method='lm', formula='x~y', aes(lty=drv))+ #Tendência
  facet_wrap(~manufacturer) #Quadrados separando os fabricantes


#Exercícios para treino
#Capítulo 3 - https://r4ds.had.co.nz/data-visualisation.html

#Tipos de variáveis
  #Numéricas
    #Contínuas (com vírgula)
    #Discretas (sem vírgula)
  
  #Categóricas
    #Nominais (não admitem ordenação) (não existe uma melhor que a outra)
    #Ordinais (admitem ordenação) (desempenho, por exemplo: bom, ruim, ótimo etc.)



#####################################################################
#2° Aula de Inferência Estatística
#Filtragem de dados em uma tabela (dplyr)
#Mesclagem de tabelas (dplyr)
#Ajuste de formato em tabelas (tidyr)

#dplyr em uma tabela
install.packages("dplyr")
library(dplyr)

dados <- tibble(cor = c('azul', 'preto', 'preto', 'azul', 'azul'),
                valor = c(1,2,3,4,5))
dados

#filter, select, arrange, mutate, group_by, summarise

#Filtragem de linhas

#obter apenas as linhas em que a cor é azul
filter(dados, cor == 'azul')

#obter apenas as linhas com valor > 2
filter(dados, valor > 2)

#obter apenas as linhas em que cor não for azul
filter(dados, cor != 'azul')

#obter apenas as linhas em que valor > 2 e cor = preto
filter(dados, cor == 'preto' & valor > 2)

#obter apenas as linhas em que cor for azul ou valor < 3
filter(dados, cor == 'azul' | valor < 3)

#obter apenas as linhas em que cor for azul, vermelho ou verde
filter(dados, cor %in% c('azul', 'vermelho', 'verde'))

10 %% 2 #Resto da divisão
10 %/% 2 #Parte inteira de uma divisão 

#Seleção de colunas
select(dados, cor) #Seleciona só a coluna cor
select(dados, -cor) #Seleciona todas as colunas menos cor
select(dados, valor, cor)

var_cor = 'cor'

#Seleciona a coluna que tem o nome igual ao valor armazenado
# na variável var_cor
select(dados,!!var_cor)

#Selecionar as colunas de cor até valor
select(dados, cor:valor)


#Ordenação de linhas

#Ordenação crescente por cor
arrange(dados, cor)

#Ordenação decrescente por cor
arrange(dados, desc(cor))

#Ordenação decrescente por variáveis numéricas
arrange(dados, -valor)
arrange(dados, desc(valor))

#Ordenação crescente de cor e, dentro de cada cor, decrescente de valor
arrange(dados, cor, -valor)


#Criação de Colunas

#Criar coluna com dobro do valor
mutate(dados, dobro = 2*valor)

#Criar coluna nivel em que, caso valor < 3, nivel = baixo; valor > 3, nivel = alto
mutate(dados, nivel= ifelse(valor > 3, 'Alto', 'Baixo'))

mutate(dados, nivel=ifelse(cor == 'azul', TRUE, FALSE))

#lag = pega o valor anterior da coluna 'cor' e atribui 'nenhum' se for null
mutate(dados, atraso1 = lag(cor,1,'nenhum'))

#Pega dois valores anteriores da coluna cor
mutate(dados, atraso2 = lag(cor,2))

#Pega um valor pra frente da coluna valor
mutate(dados, avanco1 = lead(valor,1))

#Pega dois valores pra frente da coluna valor
mutate(dados, avanco1 = lead(valor,2))

#Agrupamento e resumo de dados

#Soma todos os valores de valor e joga na coluna total
summarise(dados, total = sum(valor))

group_by(dados, cor) -> dados2
dados2
summarise(dados2, total = sum(valor))

#Exercícios para treino
#Capítulo 5 - https://r4ds.had.co.nz/transform.html


# Mesclagem de tabelas de daods - dplyr em 2 tabelas

library(dplyr)

x <- tibble(nome=c('John', 'Paul', 'Ringo', 'Harrison', 'Peter'),
            instrumento = c('guitarra', 'baixo', 'bateria', 'guitarra', 'teclado'))
y <- tibble(nome=c('John', 'Paul', 'Ringo', 'Harrison', 'Stewart', 'Davies'),
            banda =c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))

#Juntando dois data frames com um join na coluna name

#As linhas das colunas de comparação cujos valores aparecem em ambos os data frames
inner_join(x,y,by=c('nome'='nome'))

#left join - Preserva no da esquerda apenas os valores que aparecem na coluna
#de comparação no 2° data frame
left_join(x,y,by=c('nome'='nome'))

#right join  - Preserva no da direta apenas os valores que aparecem na coluna
#de comparação no 1° data frame
right_join(x,y,by=c('nome'='nome'))

#semi_join - Fica o x apenas com os valores correspondentes do y (Não mostra os elementos de y)
semi_join(x, y, by=c('nome'='nome'))

#semi_join - Fica o y apenas com os valores correspondentes do x (Não mostra os elementos de x)
semi_join(y, x, by=c('nome'='nome'))

#anti_join - oposto do inner join - Só mostra do x o que não tem no y
anti_join(x,y, by=c('nome'='nome'))

#anti_join - oposto do inner join - Só mostra do y o que não tem no x
anti_join(y,x, by=c('nome'='nome'))

#full join
full_join(x,y,by=c('nome'='nome'))

#Formas de carregamento de dados

#1a forma: leitura de .csv
#read.csv2 <- leitor de dados nativo do R
#fread -< pacote 'data.table' muito mais rápido

#2° forma: leitura por ODBC
#library(RODBC)
#conn <- odbcConnectExcel2007('arquivo.xlsx')
#dados <- sqlFetch(conn, 'planilha')
#odbcClose(conn) <- prático, porém muito lento

#Exercícios para treino
#Capítulo 13 - https://r4ds.had.co.nz/relational-data.html


#Modificação de tabelas(tidyr)
install.packages('tidyr')

library(tidyr)
#O que é o formato tidy?
#Cada coluna representa apenas uma variável
#Cada linha representa um registro independente
#Cada célula contém um único valor

table1 # Tabela arrumada
table2 # Tabela desarrumada

#Para arrumar a tabela, é melhor aumentar a largura da tabela,
#em vez da altura (espalhamento)
spread(table2, key=type, value=count)
#key = qual coluna que vai ser espalhada (virar mais de uma coluna)
#value = qual coluna possui os valores que popularão as colunas que surgirão com o espalhamento


table4a
#juntamos os valores dos anos em uma coluna ano
gather(table4a,key='year',value='cases','1999','2000') -> table4a2
gather(table4a,key='year',value='cases','1999':'2000')
#key = qual coluna vai ser criada para as colunas que já existem
#value = qual coluna vai ser criada com os valores das colunas dos anos

table4b
gather(table4b,key='year',value='population', '1999':'2000') -> table4b2

#Juntando table4a2 com table4b2
inner_join(table4a2, table4b2, by=c('country'='country','year'='year'))

#Modificação de colunas em uma tabela

table3
#Tabela tem o rate juntando cases e população numa única chave separado por /
#Separa a coluna com o separador em duas colunas chamadas "cases" e "population"
separate(table3,rate,into=c('cases','population'),'/') -> table3b

#Transformando as colunas cases e population no tipo double
mutate(table3b,cases = as.numeric(cases), population = as.numeric(population))


table5
#Tabela tem século e ano separados

#Cria uma tabela year juntando as colunas century e year sem separador
unite(table5,year,century, year, sep='') -> table5b
#Converte o ano para int
mutate(table5b,year=as.integer(year)) -> table5c

#Separando em colunas cases e population, tirando a /
separate(table5c, rate, into=c('cases','population'),'/')


#Operações no formato dados %>% função() - Mesmo resultado
table5 %>% unite(ano, century, year, sep='') %>%
  mutate(ano = as.integer(ano)) %>%
  separate(rate,into=c('cases','population'), sep='/') -> table5d
table5d


#apply
dados <- tibble(col1=c(1,2,3,4,5), col2=c(10,20,30,40,50),
                col3=c(100,200,300,400,500))
dados

#Como calcular a média de todas as colunas do dataframe?

#apply  - aplica operações em loop

#2 - coluna
apply(dados, 2, mean)

#1 - linha
#Como calcular a soma dos valores de cada linha do data frame dados?
apply(dados,1,sum)

#Vamos detalhar melhor a função que está sendo aplicada às colunas do
# data frame dados

#Criando a função - x é cada item da coluna
apply(dados, 2, function(x) { mean(x)})

#Fazendo o mesmo para a função que está sendo aplicada às linhas do
#data frame dados
apply(dados,1,function(x){sum(x)})

#Criando a função separadamente e chamá-la no apply

minhamedia <- function(x){
  media <- mean(x)
  return(media)
}

apply(dados,2,minhamedia)

#Listas
a <- 1
b <-c('Jose', 'Faria')
d <-dados
minha_lista <- list(a,b,d)
minha_lista[[1]]
minha_lista[1]