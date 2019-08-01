library(lavaan) 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(psych)
library(parallel)
library(mvtnorm)
library(polycor)

df_pos <- read.csv("C:\\Users\\amori\\OneDrive\\Documentos\\UnB\\LAMFO\\ANATEL\\BD_POS.csv")
#df_seac <- read.csv("C:\\Users\\amori\\OneDrive\\Documentos\\UnB\\LAMFO\\ANATEL\\BD_SEAC.csv")
#df_scm <- read.csv("C:\\Users\\amori\\OneDrive\\Documentos\\UnB\\LAMFO\\ANATEL\\BD_SCM.csv")
#df_stfc <- read.csv("C:\\Users\\amori\\OneDrive\\Documentos\\UnB\\LAMFO\\ANATEL\\BD_STFC.csv")

#############################Análise Fatorial - Telefone Móvel - Pós-Paga

#Separar bases: likert x binária 

df_pos_likert <- df_pos %>% select(B1_1, B1_2, C1_1, C1_2, D2_1, D2_2, D2_3, E1_1, E1_2, A2_1, A2_2, A2_3, A3, A4, A5, F2, F4, F6, F8, F10)
df_pos_likert[df_pos_likert == "99"] <- NA
df_pos_sn <- df_pos %>% select(Q1, Q3, Q4, Q5, D1, F1, F3, F5 , F7, F9, I1, I2, A1_1, A1_2, A1_3, A1_4)
df_pos_sn[df_pos_sn == "99"] <- NA
df_pos_sn <- df_pos_sn %>% 
  mutate(A1_1 = ifelse(is.na(A1_1), 0, 1),
         A1_2 = ifelse(is.na(A1_2), 0, 1),
         A1_3 = ifelse(is.na(A1_3), 0, 1),
         A1_4 = ifelse(is.na(A1_4), 0, 1)) 
df_pos_sn[df_pos_sn == "2"] <- 0 

##########################################Análise Fatorial Exporatória - Escala Likert
#retirar variáveis com mais de 50% de NA 
df_pos_likert_na <- df_pos_likert %>% select(which(colMeans(is.na(.)) < 0.5))
df_pos_sn_na <- df_pos_sn %>% select(which(colMeans(is.na(.)) < 0.5))

#retirar linhas com NA 

df_pos_likert_na <- na.omit(df_pos_likert_na)
df_pos_sn_na <- na.omit(df_pos_sn_na)

#Calcular correlações policóricas 

corpoly_lik <- hetcor(df_pos_likert_na, use="complete.obs", ML = TRUE)
corpoly2_lik <- corpoly_lik$correlations
corpoly_sn <- tetrachoric(df_pos_sn_na)
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

kmo(df_pos_likert_na)
#[1] 0.9231694 - fatorável 

##################### AFE - Dados de Escala Likert 
#análise fatorial 1 - pca + análise fatorial

acpcor <- prcomp(df_pos_likert_na, scale = TRUE)
summary(acpcor)
screeplot(acpcor,npcs = 15, type = "lines")
plot(1:ncol(df_pos_likert_na), acpcor$sdev^2, type = "b", xlab = "Componente",
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

parallel <- fa.parallel(df_pos_likert_na, fm = "minres", fa = "fa")

#Parallel analysis suggests that the number of factors =  5  and the number of components =  NA 
result_fact <- list()
for(i in c(2:5)){
  fact_i <- fa(df_pos_likert_na,nfactors = i,rotate = "varimax",fm="minres")
  print(fact_i$loadings,cutoff = 0.3)
  result_fact[[i]] <- fact_i$loadings}
#No PCA, foi sugerido dois fatores quanto na Análise Paralela foram sugeridos 5,

#analise fatorial - factanal()
result <- list()
for(i in c(2:5)){
  fator_i <- factanal(covmat = corpoly2_lik, factors = i, rotation = "varimax")
  print(fator_i$loadings, cutoff = 0.3)
  result[[i]] <- fator_i$loadings
}


#######################AFE - Variáveis Binárias 

#analise fatorial - factanal() - Fator = 4 

fit_4 <- factanal(covmat = corpoly2_sn, factors = 4, rotation = "varimax")
result_sn_4 <- list(fit_4$loadings, cutoff = 0.3)

#analise fatorial - psysh - fa()

fa_3 <- fa(r = corpoly2_sn, nfactors = 3, rotate = "varimax")
result_sn_3 <- list(fa_3$loadings, cutoff = 0.3)
