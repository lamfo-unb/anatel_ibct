library(lavaan) 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(psych)
library(parallel)
library(mvtnorm)
library(polycor)

####################################### Análise Fatorial
df <- read.csv("C:\\Users\\amori\\OneDrive\\Documentos\\UnB\\LAMFO\\ANATEL\\BD_PRE.csv")
#separar bases em tipos: likert e sim x não 

df_likert <- df %>% select(B1_1, B1_2, C1_1, C1_2, D2_1, D2_2, D2_3, E1_1, E1_2, E1_3, A2_1, A2_2, A2_3, A3, A4, A5, F2, F4, F6)
df_likert[df_likert == "99"] <- NA
df_sn <- df %>% select(Q1, Q3, Q4, Q5, D1, F1, F3, F5 , I1, I2, A1_1, A1_2, A1_3, A1_4)
df_sn[df_sn == "99"] <- NA
df_sn <- df_sn %>% 
                    mutate(A1_1 = ifelse(is.na(A1_1), 0, 1),
                            A1_2 = ifelse(is.na(A1_2), 0, 1),
                            A1_3 = ifelse(is.na(A1_3), 0, 1),
                            A1_4 = ifelse(is.na(A1_4), 0, 1)) 
df_sn[df_sn == "2"] <- 0 

######################### Análise Fatorial Exporatória - Escala Likert####################################
                           
#percentual de NA 
# total de linhas
n = nrow(df_likert)

# porcentagem de NA por coluna
round(colSums(is.na(df_likert))*100/n, 2)
#Colunas com mais de 60% de NA foram desconsideradas 
df_likert <- df_likert %>% select(-A4, -A5, -F2, -F4, -F6)
#calcular matriz de correlação policórica e índice KMO 
df_likert_na <- na.omit(df_likert)
corpoly <- hetcor(df_likert_na, use="complete.obs", ML = TRUE)

corpoly2 <- corpoly$correlations

round(corpoly2,3)

kmo <- function(x)
{
  x <- subset(x, complete.cases(x)) 
  corpoly <- hetcor(x, use="complete.obs", ML = TRUE)
  r <- corpoly$correlations # Correlação policórica
  r2 <- r^2 
  i <- solve(r)
  d <- diag(i) 
  p2 <- (-i/sqrt(outer(d, d)))^2 
  diag(r2) <- diag(p2) <- 0 
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
} 

kmo(df_likert_na)
# [1] 0.9364679 - Fatorável 

#análise fatorial 1 - pca + análise fatorial

acpcor <- prcomp(df_likert_na, scale = TRUE)
summary(acpcor)
screeplot(acpcor,npcs = 15, type = "lines")
plot(1:ncol(df_likert_na), acpcor$sdev^2, type = "b", xlab = "Componente",
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

#análise fatorial - psych 

parallel <- fa.parallel(df_likert_na, fm = "minres", fa = "fa")

#Parallel analysis suggests that the number of factors =  5  and the number of components =  NA 

for(i in c(2:5)){
  fact_i <- fa(df_likert_na,nfactors = i,rotate = "varimax",fm="minres")
  print(fact_i$loadings,cutoff = 0.3)}

#No PCA, foi sugerido dois fatores quanto na Análise Paralela foram sugeridos 5,

#analise fatorial - factanal()
for(i in c(2:5)){
fator_i <- factanal(covmat = corpoly2, factors = i, rotation = "varimax")
print(fator_i$loadings, cutoff = 0.3)}


#########################################Análise Fatorial Exploratória - Variáveis Binárias#####################################################

#percentual de NA 
# total de linhas
n = nrow(df_sn)

# porcentagem de NA por coluna
round(colSums(is.na(df_sn))*100/n, 2)
#desconsiderando as colunas com mais de 30% de NA 
df_sn <- df_sn %>% select(-F5, -I2)

#calcular matriz de correlação policórica  
df_sn_na <- na.omit(df_sn)
#df_afe_vivo_sn_na <- sapply(df_afe_vivo_sn_na, as.factor)
#corpoly_sn <- hetcor(df_afe_vivo_sn_na, ML = TRUE)
cor_sn <- tetrachoric(df_sn_na)

#analise fatorial - factanal() - Fator = 5 

fit_5 <- factanal(covmat = cor_sn$rho, factors = 5, rotation = "varimax")
print(fit_5$loadings,cutoff = 0.3)

#analise fatorial - psysh - fa()

fa_2 <- fa(r = cor_sn$rho, nfactors = 2, rotate = "varimax")
print(fa_2$loadings,cutoff = 0.3)
