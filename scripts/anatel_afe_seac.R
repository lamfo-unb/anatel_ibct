library(lavaan) 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(psych)
library(parallel)
library(mvtnorm)
library(polycor)

#df_pos <- read.csv("C:\\Users\\amori\\OneDrive\\Documentos\\UnB\\LAMFO\\ANATEL\\BD_POS.csv")
df_seac <- read.csv("C:\\Users\\amori\\OneDrive\\Documentos\\UnB\\LAMFO\\ANATEL\\BD_SEAC.csv")
#df_scm <- read.csv("C:\\Users\\amori\\OneDrive\\Documentos\\UnB\\LAMFO\\ANATEL\\BD_SCM.csv")
#df_stfc <- read.csv("C:\\Users\\amori\\OneDrive\\Documentos\\UnB\\LAMFO\\ANATEL\\BD_STFC.csv")

#############################Análise Fatorial - Televisão por Assinatura

#Separar bases: likert x binária 
  
df_seac_likert <- df_seac %>% select(B1_1, B1_2, C1_1, C1_2, A2_1, A2_2, A2_3, A3, A4, E2, E4, E6, E8, F2_1, F2_2, F2_3,F4_1, F4_2, F4_3)
df_seac_likert[df_seac_likert == "99"] <- NA
df_seac_sn <- df_seac %>% select(Q1, Q3, Q4, Q5, E1, E3, E5, E7, F1, F3, I1, I2, A1_1, A1_2, A1_3, A1_4)
df_seac_sn[df_seac_sn == "99"] <- NA
df_seac_sn <- df_seac_sn %>% 
  mutate(A1_1 = ifelse(is.na(A1_1), 0, 1),
         A1_2 = ifelse(is.na(A1_2), 0, 1),
         A1_3 = ifelse(is.na(A1_3), 0, 1),
         A1_4 = ifelse(is.na(A1_4), 0, 1)) 
df_seac_sn[df_seac_sn == "2"] <- 0 

##########################################Análise Fatorial Exporatória - Escala Likert
#retirar variáveis com mais de 50% de NA 
df_seac_likert_na <- df_seac_likert %>% select(which(colMeans(is.na(.)) < 0.5))
df_seac_sn_na <- df_seac_sn %>% select(which(colMeans(is.na(.)) < 0.5))

#retirar linhas com NA 

df_seac_likert_na <- na.omit(df_seac_likert_na)
df_seac_sn_na <- na.omit(df_seac_sn_na)

#Calcular correlações policóricas 

corpoly_lik <- hetcor(df_seac_likert_na, use="complete.obs", ML = TRUE)
corpoly2_lik <- corpoly_lik$correlations
corpoly_sn <- tetrachoric(df_seac_sn_na)
corpoly2_sn <- corpoly_sn$rho

#Índice KMO - Dados Likert 
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

kmo(df_seac_likert_na)
#[1] 0.7706465 - fatorável 

##################### AFE - Dados de Escala Likert 
#análise fatorial 1 - pca + análise fatorial

acpcor <- prcomp(df_seac_likert_na, scale = TRUE)
summary(acpcor)
screeplot(acpcor,npcs = 15, type = "lines")
plot(1:ncol(df_seac_likert_na), acpcor$sdev^2, type = "b", xlab = "Componente",
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

parallel <- fa.parallel(df_seac_likert_na, fm = "minres", fa = "fa")

#Parallel analysis suggests that the number of factors =  3  and the number of components =  NA 

  fact_3 <- fa(df_seac_likert_na,nfactors = 3,rotate = "varimax",fm="minres")
  print(fact_3$loadings,cutoff = 0.3)
  result_fact <- list(fact_3$loadings, cutoff=0.3)


#analise fatorial - factanal()
  fator_3 <- factanal(covmat = corpoly2_lik, factors = 3, rotation = "varimax")
  print(fator_3$loadings, cutoff = 0.3)
  result_factanal <- list(fator_3$loadings, cutoff=0.3)



#######################AFE - Variáveis Binárias 

#analise fatorial - factanal() - Fator = 3

fit_3 <- factanal(covmat = corpoly2_sn, factors = 3, rotation = "varimax")
result_sn_3 <- list(fit_3$loadings, cutoff = 0.3)

#analise fatorial - psysh - fa() - Fator = 3

fa_3 <- fa(r = corpoly2_sn, nfactors = 3, rotate = "varimax")
result_sn_3 <- list(fa_3$loadings, cutoff = 0.3)
