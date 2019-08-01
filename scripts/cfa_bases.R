library(lavaan)

## Utilizando as bases com as variáveis likert e binárias tratadas na análise fatorial
## exploratória e os fatores encontrados a partir dessa análise, é possível realizar a
## análise fatorial confirmatória.

## Subset dos fatores likert utilizando a sugestão do pca feito na análise exploratória
df_likert_na <- df_likert_na %>% 
  mutate_if(is.numeric, function(x) factor(x))

fatores_likert <- list()
##  B1_1 + B1_2 + C1_1 + C1_2 + D2_1 + D2_2 + D2_3 + E1_1 + E1_2 + E1_3 + A2_1 + A2_2 + A2_3 + A3
fatores_likert <- "fator1 =~ B1_1 + B1_2 + E1_2 + A2_1 + A2_2 + A2_3 + A3
                   fator2 =~ B1_2 + D2_1 + D2_2 + D2_3
                   fator3 =~ B1_1 + B1_2 + E1_1 + E1_2 + E1_3
                   fator4 =~ C1_1 + C1_2
                   fator5 =~ B1_1 + B1_2
                  "

fit <- matrixpls(corpoly2, fatores_likert)


fit <- cfa(fatores_likert, data = df_likert_na)
summary(fit)


fatores_bin <- "fator1 =~ F1 + F3
                fator2 =~ D1 + A1_2 + A1_4
                fator3 =~ A1_3 + A1_4
                fator4 =~ A1_1 + A1_4
                fator5 =~ D1"



## Função que roda a análise fatorial confirmatória com base em um data frame
## e um modelo definido por uma string.
cfa_boot <- function(df, model){
  fit <- matrixpls.boot(df, model, R = 2000)
  summary(fit)
}

## Likert Pré-pago modelo com 5 fatores

fatores_likert <- "fator1 =~ B1_1 + B1_2 + E1_2 + A2_1 + A2_2 + A2_3 + A3
                   fator2 =~ B1_2 + D2_1 + D2_2 + D2_3
                   fator3 =~ B1_1 + B1_2 + E1_1 + E1_2 + E1_3
                   fator4 =~ C1_1 + C1_2
                   fator5 =~ B1_1 + B1_2
                  "
cfa_boot(df_likert_na, fatores_likert)

## Binária Pré-Pago modelo com 5 fatores
fatores_bin <- "fator1 =~ F1 + F3
                fator2 =~ D1 + A1_2 + A1_4
                fator3 =~ A1_3 + A1_4
                fator4 =~ A1_1 + A1_4
                fator5 =~ D1"

cfa_boot(df_sn_na, fatores_bin)

## Likert Pós-Pago modelo com 5 fatores
fatores_likert <- "fator1 =~ B1_1 + B1_2 + E1_1 + E1_2 + A2_1 + A2_2 + A2_3 + A3
                   fator2 =~ B1_2 + C1_2 +  D2_1 + D2_2 + D2_3
                   fator3 =~ B1_1 + B1_2 + E1_1 + E1_2
                   fator4 =~ C1_1 + C1_2
                   fator5 =~ B1_1 + B1_2
                  "

cfa_boot(df_likert_na, fatores_likert)


## Binráia Pós-Pago modelo com 4 fatores
fatores_bin <- "fator1 =~ F1 + F3 + F5 + F7 + F9 + A1_1 + A1_4
                   fator2 =~ I1 + I2
                   fator3 =~ A1_2 + A1_4
                   fator4 =~ A1_3 + A1_4
                  "

cfa_boot(df_pos_sn_na, fatores_bin)
