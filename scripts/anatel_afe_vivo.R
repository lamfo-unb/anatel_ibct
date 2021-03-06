library(lavaan) 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(psych)
library(parallel)
library(mvtnorm)
library(polycor)


df <- read.csv("C:\\Users\\amori\\OneDrive\\Documentos\\UnB\\LAMFO\\ANATEL\\BD_PRE.csv")
#Separar base de dados por operadora
df_afe_oi <- df %>% filter(OPERADORA == "OI")
df_afe_tim <- df %>% filter(OPERADORA == "TIM")
df_afe_claro <- df %>% filter(OPERADORA == "CLARO")
df_afe_algar <- df %>% filter(OPERADORA == "ALGAR")
df_afe_vivo <- df %>% filter(OPERADORA == "VIVO")
df_afe_sercomtel <- df %>% filter(OPERADORA == "SERCOMTEL")
df_afe_nextel <- df %>% filter(OPERADORA == "NEXTEL")

#separar bases em tipos: likert e sim x n�o 

df_afe_vivo_likert <- df_afe_vivo %>% select(B1_1, B1_2, C1_1, C1_2, D2_1, D2_2, D2_3, E1_1, E1_2, E1_3, A2_1, A2_2, A2_3, A3, A4, A5, F2, F4, F6)
df_afe_vivo_likert[df_afe_vivo_likert == "99"] <- NA
df_afe_vivo_sn <- df_afe_vivo %>% select(Q1, Q3, Q4, Q5, D1, F1, F3, F5 , I1, I2, A1_1, A1_2, A1_3, A1_4)
df_afe_vivo_sn[df_afe_vivo_sn == "99"] <- NA
df_afe_vivo_sn <- df_afe_vivo_sn %>% 
                    mutate(A1_1 = ifelse(is.na(A1_1), 0, 1),
                            A1_2 = ifelse(is.na(A1_2), 0, 1),
                            A1_3 = ifelse(is.na(A1_3), 0, 1),
                            A1_4 = ifelse(is.na(A1_4), 0, 1)) 
df_afe_vivo_sn[df_afe_vivo_sn == "2"] <- 0 

######################### An�lise Fatorial Exporat�ria - Escala Likert####################################
                           
#percentual de NA 
# total de linhas
n = nrow(df_afe_vivo_likert)

# porcentagem de NA por coluna
round(colSums(is.na(df_afe_vivo_likert))*100/n, 2)
#Colunas com mais de 60% de NA foram desconsideradas 
df_afe_vivo_likert <- df_afe_vivo_likert %>% select(-A4, -A5, -F2, -F4, -F6)
#calcular matriz de correla��o polic�rica e �ndice KMO 
df_afe_vivo_likert_na <- na.omit(df_afe_vivo_likert)
corpoly <- hetcor(df_afe_vivo_likert_na, use="complete.obs", ML = TRUE)

corpoly2 <- corpoly$correlations

round(corpoly2,3)

kmo <- function(x)
{
  x <- subset(x, complete.cases(x)) 
  corpoly <- hetcor(x, use="complete.obs", ML = TRUE)
  r <- corpoly$correlations # Correla��o polic�rica
  r2 <- r^2 
  i <- solve(r)
  d <- diag(i) 
  p2 <- (-i/sqrt(outer(d, d)))^2 
  diag(r2) <- diag(p2) <- 0 
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
} 

kmo(df_afe_vivo_likert_na)
# [1] 0.9335901 - Fator�vel 

#an�lise fatorial 1 - pca + an�lise fatorial

acpcor <- prcomp(df_afe_vivo_likert_na, scale = TRUE)
summary(acpcor)
screeplot(acpcor,npcs = 15, type = "lines")
plot(1:ncol(df_afe_vivo_likert_na), acpcor$sdev^2, type = "b", xlab = "Componente",
          ylab = "Vari�ncia", pch = 20, cex.axis = 1.3, cex.lab = 1.3)


k <- 2 #2 fatores selecionados
carfat = acpcor$rotation[, 1:k] %*% diag(acpcor$sdev[1:k])
carfatr = varimax(carfat)
#sem rota��o
plot(carfat, pch = 20, col = "red", xlab = "Fator 1", ylab = "Fator 2")
text(carfat, rownames(carfat), adj = 1)
#com rota��o
plot(carfatr$loadings, pch = 20, col = "red", xlab = "Fator 1", ylab = "Fator 2")
text(carfatr$loadings, rownames(carfat), adj = 1)

#an�lise fatorial - psych 

parallel <- fa.parallel(df_afe_vivo_likert_na, fm = "minres", fa = "fa")

#Parallel analysis suggests that the number of factors =  5  and the number of components =  NA 

for(i in c(2:5)){
  fact_i <- fa(df_afe_vivo_likert_na,nfactors = i,rotate = "varimax",fm="minres")
  print(fact_i$loadings,cutoff = 0.3)}

#No PCA, foi sugerido dois fatores quanto na An�lise Paralela foram sugeridos 5,

#analise fatorial - factanal()
for(i in c(2:5)){
fator_i <- factanal(covmat = corpoly2, factors = i, rotation = "varimax")
print(fator_i$loadings, cutoff = 0.3)}


#########################################An�lise Fatorial Explorat�ria - Vari�veis Bin�rias#####################################################

#percentual de NA 
# total de linhas
n = nrow(df_afe_vivo_sn)

# porcentagem de NA por coluna
round(colSums(is.na(df_afe_vivo_sn))*100/n, 2)
#desconsiderando as colunas com mais de 30% de NA 
df_afe_vivo_sn <- df_afe_vivo_sn %>% select(-F5, -I2)

#calcular matriz de correla��o polic�rica  
df_afe_vivo_sn_na <- na.omit(df_afe_vivo_sn)
#df_afe_vivo_sn_na <- sapply(df_afe_vivo_sn_na, as.factor)
#corpoly_sn <- hetcor(df_afe_vivo_sn_na, ML = TRUE)
cor_sn <- tetrachoric(df_afe_vivo_sn_na)

#analise fatorial - factanal()

fit_2 <- factanal(covmat = cor_sn$rho, factors = 2, rotation = "varimax")
print(fit_2$loadings,cutoff = 0.3)

#analise fatorial - psysh - fa()

fa_2 <- fa(r = cor_sn$rho, nfactors = 2, rotate = "varimax")
print(fa_2$loadings,cutoff = 0.3)
