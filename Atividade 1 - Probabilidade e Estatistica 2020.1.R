#---------------------------------------------------------------------------

#ALUNO: Wesley Honorato Pontes
#MATRÍCULA: 16212577
#CURSO: Engenharia de Computação - UFAL

#---------------------------------------------------------------------------

#1ª QUESTÃO
Dados <- c(48, 58, 56, 63, 52, 50, 59, 51, 59, 38, 57, 56, 73, 61, 41,
           55, 49, 61, 49, 49, 52, 55, 60, 52, 54, 57, 47, 66, 60, 53,
           59, 50, 45, 57, 64, 56, 57, 60, 47, 58, 53, 58, 66, 47, 40)
#a) Gerar a tabela de distribuição de frequência:
Freq = table(c(Dados))

FreqAc <- cumsum(Freq) 
FreqRel <- prop.table(Freq) 
FreqRelAc <- cumsum(FreqRel)

TabResul = cbind(Freq, FreqAc, FreqRel = round(FreqRel*100, digits = 2), 
                FreqRelAc = round(FreqRelAc*100, digits = 2))
#Gráfico:       
barplot(table(Dados),
        main = "Distribuição dos Dados",
        xlab = "Dados",
        ylab = "Frequência")

#b) Média, Desvio Padrão, Moda de Czuber, Mediana, Q3, P8, P50, P80, AP2 e K:
#Média:
mean(Dados)

#Desvio Padrão
sd(Dados, na.rm = TRUE)

#Moda Czuber
moda <- c(Freq)
names(Freq)[which.max(Freq)]

#Mediana
median(Dados)

#Q3 - Terceiro quartil (75%)
quantile(c(Dados), probs = 0.75, na.rm = TRUE)

#P8, P50, P80 - Percentis 8%, 50%, 80%
quantile(c(Dados), 0.08)
quantile(c(Dados), 0.5)
quantile(c(Dados), 0.8)


#AP2 - Coeficiente de assimetria de Pearson.
AP2 = 3 * (mean(Dados) - median(Dados))/sd(Dados); AP2

#K
NK <- round(1 + 3.322 * log10(length(Dados))); NK

#c) Gráfico BoxPlot
boxplot(c(Dados), 
        col = c("red", "yellow"),
        main = "BoxPlot: Dados",
        legend = TRUE)

#---------------------------------------------------------------------------

#2ª QUESTÃO
#a) Gráfico de barra e boxplot de óbitos:
barplot(Base_Covid_Alagoas$Seq,
        ylim = c(0,3000),
        xlab = "Tempo",
        ylab = "Quantidade de Óbitos",
        main = "Base de Óbitos COVID-19 AL",
        beside = TRUE,
        legend = TRUE)

boxplot(Seq~Ano, data = Base_Covid_Alagoas, col = c("red", "yellow"))

#b) Gráfico de barras mês a mês:
hist(Base_Covid_Alagoas$Mes,
     main = "Óbitos em Alagoas",
     xlab = "Mês",
     ylab = "Óbitos",
     col = 2,
     border = 3);

#c) Gráficos de óbitos por sexo:
barplot(table(Base_Covid_Alagoas$Sexo),
        legend = TRUE,
        col = 2:1,
        ylim = c(0,3000),
        xlab = "Sexo",
        ylab = "Quantidade",
        main = "Quantidade de óbitos por Sexo")

#d) Gráfico de obtidos por idade:
barplot(table(Base_Covid_Alagoas$Idade),
        legend = FALSE,
        col = 1:8,
        ylim = c(0,100),
        xlab = "Idade",
        ylab = "Quantidade",
        main = "Quantidade de óbitos por Idade")

#---------------------------------------------------------------------------

#3ª QUESTÃO
barplot(table(Base_Covid_Alagoas$Municipio),
        legend = TRUE,
        col = 1:8,
        ylim = c(0,250),
        xlab = "Municipios",
        ylab = "Óbitos",
        main = "Quantidade de óbitos por Municipio")
