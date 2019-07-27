library(lavaan) 
library(tidyverse)
library(dplyr)
library(ggplot2)

df <- read.csv("C:\\Users\\amori\\OneDrive\\Documentos\\UnB\\LAMFO\\ANATEL\\BD_PRE.csv")
#Separar base de dados por operadora
df_afe_oi <- df %>% filter(OPERADORA == "OI")
df_afe_tim <- df %>% filter(OPERADORA == "TIM")
df_afe_claro <- df %>% filter(OPERADORA == "CLARO")
df_afe_algar <- df %>% filter(OPERADORA == "ALGAR")
df_afe_vivo <- df %>% filter(OPERADORA == "VIVO")
df_afe_sercomtel <- df %>% filter(OPERADORA == "SERCOMTEL")
df_afe_nextel <- df %>% filter(OPERADORA == "NEXTEL")

#separar bases em tipos: likert e sim x não 

df_afe_vivo_likert <- df_afe_vivo %>% select(B1_1, B1_2, C1_1, C1_2, D2_1, D2_2, D2_3, E1_1, E1_2, E1_3, A2_1, A2_2, A2_3, A3, A4, A5, F2, F4, F6)
df_afe_vivo_likert[df_afe_vivo_likert == "99"] <- NA

#percentual de NA 
# total de linhas
n = nrow(df_afe_vivo_likert)

# porcentagem de NA por coluna
round(colSums(is.na(df_afe_vivo_likert))*100/n, 2)
#Colunas com mais de 60% de NA foram desconsideradas 
df_afe_vivo_likert <- df_afe_vivo_likert %>% select(-A4, -A5, -F2, -F4, -F6)
#calcular matriz de correlação, índice KMO 
df_afe_vivo_likert_na <- na.omit(df_afe_vivo_likert)
cor_likert <- cor(df_afe_vivo_likert_na)
kmo <- function(x)
{
  x <- subset(x, complete.cases(x)) # Omit missing values
  r <- cor(x) # Correlation matrix
  r2 <- r^2 # Squared correlation coefficients
  i <- solve(r) # Inverse matrix of correlation matrix
  d <- diag(i) # Diagonal elements of inverse matrix
  p2 <- (-i/sqrt(outer(d, d)))^2 # Squared partial correlation coefficients
  diag(r2) <- diag(p2) <- 0 # Delete diagonal elements
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
} 

kmo(df_afe_vivo_likert_na)
# [1] 0.9335901 - Fatorável 

#análise fatorial 

acpcor <- prcomp(df_afe_vivo_likert_na, scale = TRUE)
summary(acpcor)
screeplot(acpcor,npcs = 15, type = "lines")
plot(1:ncol(df_afe_vivo_likert_na), acpcor$sdev^2, type = "b", xlab = "Componente",
          ylab = "Variância", pch = 20, cex.axis = 1.3, cex.lab = 1.3)


k <- 2 #2 fatores selecionados
carfat = acpcor$rotation[, 1:k] %*% diag(acpcor$sdev[1:k])
carfatr = varimax(carfat)
#sem rotação
plot(carfat, pch = 20, col = "red", xlab = "Fator 1", ylab = "Fator 2")
text(carfat, rownames(carfat), adj = 1)
#com rotação
plot(carfatr$loadings, pch = 20, col = "red", xlab = "Fator 1", ylab = "Fator 2")
text(carfatr$loadings, rownames(carfat), adj = 1)
