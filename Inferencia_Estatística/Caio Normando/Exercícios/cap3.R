
#Visualização dos dados
install.packages("ggplot2")
library(ggplot2)
View(mpg)

#Gráfico tamanho do motor x eficiência na estrada
ggplot(data=mpg) + geom_point(aes(x=displ, y=hwy))

#ggplot = cria apenas o sistema de coordenadas
#o ggplot só com o argumento data não exibe nada

#ggplot com argumento data e aes exibe somente os eixos nomeados
#com os valores das variáveis

#podemos usar o aes no ggplot ou no geom_point
#aes = mapemamento estético - onde dizemos o mapeamento
#das variáveis visuais do gráfico em relação aos dados do dataframe

#A geom_point() é a função responsável por adicionar a camada de pontos
#no gráfico

#TODA FUNÇÃO NO GGPLOT2 TEM UM ARGUMENTO MAPPING

################################################################
#Função genérica do ggplot para plottar gráficos:
#ggplot(data= <DATA>) + <GEOM_FUNCTION>(mapping=aes(<MAPPING>))
################################################################

################################################################
#3.2.4 Exercises


#2
?mpg



#4
ggplot(data=mpg) + geom_point(mapping = aes(x=hwy, y=cyl))

#5
ggplot(data=mpg) + geom_point(aes(x=class, y=drv))
################################################################

################################################################
#3.3.1 Exercises

#1
#color is inside aes

################################################################
