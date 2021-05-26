
# Aluno: Wesley Honorato Pontes
# Matrícula: 16212577

#Valores:

value <- c(26, 24, 23, 22, 28, 25, 27, 26, 28, 
           24, 27, 25, 20, 24, 24, 27, 25, 31)

#--------------------------------------------------------------------------
# 1º) xx = 31.

# Letra a) Analizar o nível de Significancia:


# >> Utilizando a função t.test <<

t.test(value, mu = 26)

# Value = Valores da Questão.
# mu = Média Dada na Questão ( 26mg ).

# Letra b) Analizar o Nível de Confiança

t.test(value, conf = 0.92, mu = 26)

# Value = Valores da Questão.
# Conf = Nível Percentual do Intervalo de Confiança.
# mu = Média Dada na Questão ( 26mg ).


#--------------------------------------------------------------------------
# 2º) Xn = 5 ; Yn = ~ 6,58 ; Zn = ~ 6,42.

#Letra a)

# Adicionando os Valores da Tabela
X <- c(10.8, 8.6, 11.0, 9.8, 11.0, 14.0, 6.0, 4.0, 12.0, 7.4, 5)
Y <- c(8.04, 6.58, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26, 10.84, 4.82, 6.58)
Z <- c(6.58, 5.76, 7.71, 8.84, 8.47, 7.04, 5.25, 10.50, 5.25, 7.91, 6.42) 

# Criando uma Tabela
df <- data.frame(X, Y, Z)
cor(df)

# Calculando os Coeficientes de Correlações
cor(X,Y)
cor(X,Z)
cor(Y,Z)

# Letra b) 2 Variáveis com Maior Correlação e calcular a Equação da Reta
# e o Coeficiente de Determinação de Pearson (R2)

cor.test(df$X, df$Y, mothod = "pearson")

summary(lm(df$Y ~ df$X))

# Na parte "Coefficients:"
#  Equação: Y = (Estimate (df$X))x + (Estimate (Intercept))
#  R² = (R-squared)

# Letra c) Os Resíduos são aderentes à distribuição normal?
# E com qual nível de significância ?

#No final da Tabela dada acima tera os valores:
#"Residual Standar Error: (value) on 9 degress of freedom"
#   Significa que Resíduos aderem à distribuição normal com nível de
#   significancia de 9% para erro residual padrão de (valor dado).

shapiro.test(lm(df$Y ~ df$X)$residuals) #Analizar Residuos em Teste Normal


#--------------------------------------------------------------------------
# 3º) Xx = 51 ; Yy = 55 ; Zz = 62.

# Aplicando todos os Dados sem importação de tabela:
Fabrica <- c("Atlanta", "Atlanta", "Atlanta", "Atlanta", "Atlanta", 
             "Atlanta", "Atlanta", "Dallas", "Dallas", "Dallas", 
             "Dallas", "Dallas", "Dallas", "Dallas", "Seatle", "Seatle", 
             "Seatle", "Seatle", "Seatle", "Seatle", "Seatle")

Dados <- c(85, 75, 82, 76, 71, 85, 51, 
           71, 75, 73, 74, 69, 82, 55, 
           59, 64, 62, 69, 76, 67, 62) 

result <- aov(Dados ~ Fabrica)

anova(result)


# Importando uma tabela:

# Nome da tabela importada: "fabricas"
result <- aov(fabricas$Dados ~ fabricas$Fabricas) 

summary(result) # Mostrar a tabela



#--------------------------------------------------------------------------
# 4º) Xta = 355 ; Xtn = 359.

# Letra a) Calcular o Intervalo de Confiança com 2% de Significância
# para os valores de 'TecNova'

TecAtual <- c(300, 280, 344, 385, 372, 360, 288, 321, 376, 290, 301, 355)

TecNova <- c(274, 220, 308, 336, 198, 300, 315, 258, 318, 310, 273, 359)

# Intervalo de Confiança de 98%, Utilizando a Função
t.test(TecNova, conf = 0.98)

# Intervalo Sem Uso da Função
alfa = 0.02
n = length(valores)
desvio = sd(valores)
media = mean(valores)

tc = qt(p = 1 - alfa/2, df = n - 1)
tc = round(tc, 3)
tc

erro = tc * desvio / sqrt(n)
erro = round(erro, 3)
erro

cat("(", media - erro, ",", media + erro, ")")

# Letra b) Considerando o Nível de Significância de 4%

t.test(TecAtual, TecNova, alternative = 'greater', conf = 0.96)

