if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, haven, stargazer, fixest, modelsummary, sandwich, rosetta, writexl, psych, pander, knitr, car, easystats, scales)
options(scipen = 999)

###Questão 1----

##Arrumando a base

dir <- getwd()

q1_3base <- read_dta("data/censo_2000_rio.dta") %>%  #Leitura da base
  filter(v0103 == 3304557)   #Delimitando para englobar apenas Rio cidade
  
#Selecionando as variáveis de interesse

q1_3base <- q1_3base %>% select(c(v0401, v4752, v4300, v0408, v0439, v4513, v0453, v4620, v4621, v4622, v0464))

#Renomeando as variáveis

names(q1_3base) <- c("d_mulher", "idade", "educa", "branco", "trab", "salario", "horas", "n_fil", "n_fil_o", "n_fil_a", "ult_fil")

#Remoção das entradas do campo Raça que possuem valor 9. Pois este valor representa "Ignorado". 
#Remoção também dos indivíduos que possuem trabalho mas tem salário 0. Checar esse segundo filtro. 

q1_3base <- q1_3base %>% filter(branco != 9) %>% filter(!(trab == 1 & salario == 0))

#Criando as variáveis pedidas.

q1_3base <- q1_3base %>% mutate(d_mulher = ifelse(d_mulher == 2, 1, 0),
                          trab = ifelse(trab == 1, 1, 0),
                          idade_2 = (idade^2),
                          sal_hr = (salario/horas),
                          ln_sal_hr = ifelse(sal_hr == 0, NA, log(sal_hr)), #Decisão de não utilizar os valores de trabalhos não remunerados
                          d_fil = ifelse(n_fil == 0, 0, 1),
                          branco = ifelse(branco == 1, 1, 0),
                          ult_fil = ifelse(ult_fil == 2, 1, 0))

#Filtrando para pessoas entre 18 a 60

q1_3base <- q1_3base %>% filter(d_mulher == 1 & idade<=60 & idade>=18)

#Relocando as colunas

q1_3base <- q1_3base[c(1:2, 12, 3:7, 13:14, 8:11, 15)]


##Tabela de medidas

q1tab_medidas <- describe(q1_3base, skew = F)

q1tab_medidas <- q1tab_medidas[-3, c(3,4,6,5)]

row.names(q1tab_medidas) <- c("Dummy - Mulher", "Idade", "Anos de estudo", "Raça", "Dummy - Trabalho", "Salário", "Horas trabalhadas", "Salário/hora", "ln(Salário/hora)", "Nº de filhos", "Nº de filhos homens", "Nº de filhas mulheres", "Sexo do último filho", "Dummy - Possui filhos")

names(q1tab_medidas) <- c("Média", "Desvio-Padrão", "Máximo", "Mínimo")

q1tab_medidas <- as_tibble(q1tab_medidas, rownames = "Variáveis")

##Tabela de diferença de médias

dat <- q1_3base

temp <- meanDiff.multi(dat = q1_3base, x = "d_fil", y = c("trab", "salario", "horas"))

rm(dat)

q1tabdif <- temp$results.compiled

rm(temp)

q1tabdif <- q1tabdif[c(2, 5:8, 20)] %>% mutate(difmedia = mean1 - mean2, .after = mean2)

q1tabdif <- q1tabdif %>% mutate(stderrdif = c(t.test(filter(q1_3base, d_fil == 1)$trab, filter(q1_3base, d_fil == 0)$trab, alternative = "two.sided")$stderr,
                                              t.test(filter(q1_3base, d_fil == 1)$salario, filter(q1_3base, d_fil == 0)$salario, alternative = "two.sided")$stderr,
                                              t.test(filter(q1_3base, d_fil == 1)$horas, filter(q1_3base, d_fil == 0)$horas, alternative = "two.sided")$stderr))


names(q1tabdif) <- c("Variáveis", "Média (sem filhos)", "Média (com filhos)", "Diferença de médias", "Desvio-Padrão (sem filhos)", "Desvio-Padrão (com filhos)", "P-valor", "Desvio-Padrão da diferença") 

write_xlsx(q1tabdif,"C:\\Users\\Pedro\\Google Drive\\Mestrado\\Microeconometria\\Trabalho3_IV\\q1tabdif.xlsx")

q1_3base <- q1_3base %>% mutate_at(vars(d_mulher, branco, d_fil, ult_fil), as.factor)

###Questão 2----

#Regressões pedidas

q2reg1 <- feols(trab ~ d_fil, data = q1_3base, se = "standard")

q2reg2 <- feols(trab ~ d_fil, data = q1_3base, se = "hetero")

#Tabela com as regressões

q2tab1 <- modelsummary(list("MQO Simples" = q2reg1, "MQO Robusto" = q2reg2),
                       stars = c('*' = .1, '**' = .05, '***' = .01),
                       estimate  = c("{estimate}{stars}"),
                       gof_omit = "^(R2 Within|R2 Pseudo|Std|FE|AIC|BIC|Log.Lik)",
                       fmt = 4,
                       output = "latex")

###Questão 3----

#Regressões pedidas

q3reg1 <- feols(trab ~ d_fil + idade + idade_2, data = q1_3base, se = "hetero")

q3reg2 <- feols(trab ~ d_fil + idade + idade_2 + educa, data = q1_3base, se = "hetero")

q3reg3 <- feols(trab ~ d_fil + idade + idade_2 + educa + branco, data = q1_3base, se = "hetero")

#Tabela com as regressões

q3tab1 <- modelsummary(list("(1)" = q3reg1, "(2)" = q3reg2, "(3)" = q3reg3),
                       stars = c('*' = .1, '**' = .05, '***' = .01),
                       estimate  = c("{estimate}{stars}"),
                       gof_omit = "^(R2 Within|R2 Pseudo|Num|Std|FE|AIC|BIC|Log.Lik)",
                       digits = 2,
                       output = "latex")

###Questão 4----

q4_8base <- q1_3base %>% filter(n_fil == 2|n_fil == 3) %>% mutate(d_tresfil = ifelse(n_fil == 3, 1, 0))

##Tabela de diferença de médias

dat <- q4_8base

temp <- meanDiff.multi(dat, x = "d_tresfil", y = c("trab", "salario", "horas"))

rm(dat)

q4tabdif <- temp$results.compiled

rm(temp)

q4tabdif <- q4tabdif[c(2, 5:8, 20)] %>% mutate(difmedia = mean1 - mean2, .after = mean2)

q4tabdif <- q4tabdif %>% mutate(stderrdif = c(t.test(filter(q1_3base, d_fil == 1)$trab, filter(q1_3base, d_fil == 0)$trab, alternative = "two.sided")$stderr,
                                              t.test(filter(q1_3base, d_fil == 1)$salario, filter(q1_3base, d_fil == 0)$salario, alternative = "two.sided")$stderr,
                                              t.test(filter(q1_3base, d_fil == 1)$horas, filter(q1_3base, d_fil == 0)$horas, alternative = "two.sided")$stderr))



names(q4tabdif) <- c("Variáveis", "Média (2 filhos)", "Média (3 filhos)", "Diferença de médias", "Desvio Padrão (2 filhos)", "Desvio Padrão (3 filhos)", "P-valor") 

write_xlsx(q4tabdif,"C:\\Users\\Pedro\\Google Drive\\Mestrado\\Microeconometria\\Trabalho3_IV\\q4tabdif.xlsx")

###Questão 5----

##Regressões pedidas

q5reg1 <- feols(trab ~ d_tresfil + idade + idade_2 + educa + branco, data = q4_8base, se = "hetero")

q5reg2 <- feols(horas ~ d_tresfil + idade + idade_2 + educa + branco, data = filter(q4_8base, trab == 1), se = "hetero")

q5reg3 <- feols(ln_sal_hr ~ d_tresfil + idade + idade_2 + educa + branco, data = filter(q4_8base, trab == 1), se = "hetero")

#Tabela com as regressões

q5tab1 <- modelsummary(list("Trabalho" = q5reg1, "Horas" = q5reg2, "ln(Salário/Hora)" = q5reg3),
                       stars = c('*' = .1, '**' = .05, '***' = .01),
                       estimate  = c("{estimate}{stars}"),
                       gof_omit = "^(R2 Within|R2 Pseudo|Std|FE|AIC|BIC|Log.Lik)",
                       digits = 2,
                       output = "latex")

###Questão 6----

###Questão 7----

q4_8base <- q4_8base %>% mutate(d_filcas = ifelse(n_fil == n_fil_o | n_fil == n_fil_a, 0, 
                                              ifelse(ult_fil == 1 & n_fil_o == 2 | ult_fil == 0 & n_fil_a == 2, 0, 1)))

q7proporcao <- data.frame(table(q4_8base$n_fil), table(q4_8base$n_fil)/sum(table(q4_8base$n_fil))) %>% select(c(1,2,4))

q7proporcao$Freq.1 <- percent(q7proporcao$Freq.1)

kable(q7proporcao, col.names = c("Nº de filhos", "Observações", "Proporção"), format = "latex")

q7reg1 <- feols(d_tresfil ~ d_filcas + idade + idade_2 + educa + branco, data = q4_8base, se = "hetero")

q7reg2 <- feols(d_tresfil ~ d_filcas + idade + idade_2 + educa + branco, data = filter(q4_8base, trab == 1), se = "hetero")

q7tab1 <- modelsummary(list("(1)" = q7reg1, "(2)" = q7reg2),
                       stars = c('*' = .1, '**' = .05, '***' = .01),
                       estimate  = c("{estimate}{stars}"),
                       gof_omit = "^(R2 Within|R2 Pseudo|FE|AIC|BIC|Log.Lik)",
                       digits = 2,
                       output = "latex")


#Teste F para o instrumento

linearHypothesis(q7reg1, "d_filcas = 0", rhs=NULL, test=c("F"), vcov. = vcovHC(q7reg1, type = "HC3")) 

linearHypothesis(q7reg2, "d_filcas = 0", rhs=NULL, test=c("F"), vcov. = vcovHC(q7reg2, type = "HC3"))


###Questão 8----

q4_8base <- q4_8base %>% mutate(d_tresfil_prev = q7reg1$fitted.values)

q4_8base_filtro <- filter(q4_8base, trab == 1) %>% mutate(d_tresfil_prev = q7reg2$fitted.values)

q8reg1 <- feols(trab ~ d_tresfil_prev + idade + idade_2 + educa + branco, data = q4_8base, se = "hetero")

q8reg2 <- feols(horas ~ d_tresfil_prev + idade + idade_2 + educa + branco, data = q4_8base_filtro, se = "hetero")

q8reg3 <- feols(ln_sal_hr ~ d_tresfil_prev + idade + idade_2 + educa + branco, data = q4_8base_filtro, se = "hetero")

#Tabela com as regressões

q8tab1 <- modelsummary(list("Trabalho" = q8reg1, "Horas" = q8reg2, "ln(Salário/Hora)" = q8reg3),
                       stars = c('*' = .1, '**' = .05, '***' = .01),
                       estimate  = c("{estimate}{stars}"),
                       gof_omit = "^(R2 Within|R2 Pseudo|FE|AIC|BIC|Log.Lik)",
                       digits = 2,
                       output = "latex")

###Questão 9----

q9reg1 <- feols(trab ~ idade + idade_2 + educa + branco | d_tresfil ~ d_filcas, data = q4_8base, se = "hetero")

q9reg2 <- feols(horas ~  idade + idade_2 + educa + branco | d_tresfil ~ d_filcas, data = filter(q4_8base, trab == 1), se = "hetero")

q9reg3 <- feols(ln_sal_hr ~  idade + idade_2 + educa + branco | d_tresfil ~ d_filcas, data = filter(q4_8base, trab == 1), se = "hetero")


summary(q9reg1)

summary(q9reg2)

summary(q9reg3)

#Tabela com as regressões

q9tab1 <- modelsummary(list("Trabalho" = q9reg1, "Horas" = q9reg2, "ln(Salário/Hora)" = q9reg3),
                       stars = c('*' = .1, '**' = .05, '***' = .01),
                       estimate  = c("{estimate}{stars}"),
                       gof_omit = "^(R2 Within|R2 Pseudo|FE|AIC|BIC|Log.Lik)",
                       digits = 2,
                       output = "latex")
