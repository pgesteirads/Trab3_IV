if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, haven, stargazer, fixest, modelsummary, rosetta, writexl, psych)

###Questão 1----

##Arrumando a base

dir <- getwd()

censo <- read_dta("data/censo_2000_rio.dta") %>%  #Leitura da base
  filter(v0103 == 3304557)   #Delimitando para englobar apenas Rio cidade
  
#Selecionando as variáveis de interesse

censo <- censo %>% select(c(v0401, v4752, v4300, v0408, v0439, v4513, v0453, v4620, v4621, v4622, v0464))

#Renomeando as variáveis

names(censo) <- c("d_mulher", "idade", "educa", "branco", "trab", "salario", "horas", "n_fil", "n_fil_o", "n_fil_a", "ult_fil")

#Criando as variáveis pedidas

censo <- censo %>% mutate(d_mulher = ifelse(d_mulher == 2, 1, 0),
                          trab = ifelse(trab == 1, 1, 0),
                          idade_2 = (idade^2),
                          sal_hr = (salario/horas),
                          ln_sal_hr = log(sal_hr),
                          d_fil = ifelse(n_fil == 0, 0, 1),
                          ult_fil = ifelse(ult_fil == 2, 1, 0))


#Filtrando para pessoas entre 18 a 60

censo <- censo %>% filter(d_mulher == 1 & idade<=60 & idade>=18)

#Relocando as colunas

censo <- censo[c(1:2, 12, 3:7, 13:14, 8:11, 15)]

censo <- censo %>% mutate_at(vars(d_mulher, branco, d_fil, ult_fil), as.factor)

##Tabela de medidas

#q1tab_medidas <- data.frame("Média" = sapply(censo[-3], mean, na.rm = TRUE),
#                           "Desvio Padrão" = sapply(censo[-3], sd, na.rm = TRUE),
#                           "Máximo" = sapply(censo[-3], max, na.rm = TRUE),
#                           "Mínimo" = sapply(censo[-3], min, na.rm = TRUE))

q1tab_medidas <- datasummary_skim(censo[-c(3,10)], histogram = F)

##Tabela de diferença de médias

dat <- censo

temp <- meanDiff.multi(dat = censo, x = "d_fil", y = c("trab", "salario", "horas"))

rm(dat)

q1tabdif <- temp$results.compiled

rm(temp)

q1tabdif <- q1tabdif[c(2, 5:8, 20)] %>% mutate(difmedia = mean1 - mean2, .after = mean2)

names(q1tabdif) <- c("Variáveis", "Média (sem filhos)", "Média (com filhos)", "Diferença de médias", "Desvio Padrão (sem filhos)", "Desvio Padrão (com filhos)", "P-valor") 

write_xlsx(q1tabdif,"C:\\Users\\Pedro\\Google Drive\\Mestrado\\Microeconometria\\Trabalho3_IV\\q1tabdif.xlsx")




###Questão 2----

#Regressões pedidas

q2reg1 <- feols(trab ~ d_fil, data = censo, se = "standard")

q2reg2 <- feols(trab ~ d_fil, data = censo, se = "hetero")

#Tabela com as regressões

q2tab1 <- modelsummary(list("MQO Simples" = q2reg1, "MQO Robusto" = q2reg2),
                       stars = c('*' = .1, '**' = .05, '***' = .01),
                       estimate  = c("{estimate}{stars}"),
                       gof_omit = "^(R2 Within|R2 Pseudo|Num|Std|FE|AIC|BIC|Log.Lik)")

###Questão 3----

#Regressões pedidas

q3reg1 <- feols(trab ~ d_fil + idade + idade_2, data = censo, se = "hetero")

q3reg2 <- feols(trab ~ d_fil + idade + idade_2 + educa, data = censo, se = "hetero")

q3reg3 <- feols(trab ~ d_fil + idade + idade_2 + educa + branco, data = censo, se = "hetero")

#Tabela com as regressões

q3tab1 <- modelsummary(list("(1)" = q3reg1, "(2)" = q3reg2, "(3)" = q3reg3),
                       stars = c('*' = .1, '**' = .05, '***' = .01),
                       estimate  = c("{estimate}{stars}"),
                       gof_omit = "^(R2 Within|R2 Pseudo|Num|Std|FE|AIC|BIC|Log.Lik)")

###Questão 4----

q4base <- censo %>% filter(n_fil == 2|n_fil == 3) %>% mutate(d_tresfil = ifelse(n_fil == 3, 1, 0))

##Tabela de diferença de médias

dat <- q4base

temp <- meanDiff.multi(dat, x = "d_tresfil", y = c("trab", "salario", "horas"))

rm(dat)

q4tabdif <- temp$results.compiled

rm(temp)

q4tabdif <- q4tabdif[c(2, 5:8, 20)] %>% mutate(difmedia = mean1 - mean2, .after = mean2)

names(q4tabdif) <- c("Variáveis", "Média (2 filhos)", "Média (3 filhos)", "Diferença de médias", "Desvio Padrão (2 filhos)", "Desvio Padrão (3 filhos)", "P-valor") 

write_xlsx(q4tabdif,"C:\\Users\\Pedro\\Google Drive\\Mestrado\\Microeconometria\\Trabalho3_IV\\q4tabdif.xlsx")

###Questão 5----

##Regressões pedidas

q5reg1 <- feols(trab ~ d_tresfil + idade + idade_2 + educa + branco, data = q4base, se = "hetero")

q5reg2 <- feols(horas ~ d_tresfil + idade + idade_2 + educa + branco, data = filter(q4base, trab == 1), se = "hetero")

q5reg3 <- feols(ln_sal_hr ~ d_tresfil + idade + idade_2 + educa + branco, data = filter(q4base, trab == 1), se = "hetero")

#Tabela com as regressões

q5tab1 <- modelsummary(list("(1)" = q5reg1, "(2)" = q5reg2, "(3)" = q5reg3),
                       stars = c('*' = .1, '**' = .05, '***' = .01),
                       estimate  = c("{estimate}{stars}"),
                       gof_omit = "^(R2 Within|R2 Pseudo|Num|Std|FE|AIC|BIC|Log.Lik)")

###Questão 6----

###Questão 7----

q4base <- q4base %>% mutate(d_filcas = ifelse(n_fil == n_fil_o | n_fil == n_fil_a, 0, 
                                              ifelse(ult_fil == 1 & n_fil_o == 2 | ult_fil == 0 & n_fil_a == 2, 0, 1)))

q7reg1 <- feols(d_tresfil ~ d_filcas + idade + idade_2 + educa + branco, data = q4base, se = "hetero")

###Questão 8----

q4base <- q4base %>% mutate(d_tresfil_prev = q7reg1$fitted.values) #Confirmar se é isso mesmo

q8reg1 <- feols(trab ~ d_tresfil_prev + idade + idade_2 + educa + branco, data = q4base, se = "hetero")

q8reg2 <- feols(horas ~ d_tresfil_prev + idade + idade_2 + educa + branco, data = filter(q4base, trab == 1), se = "hetero")

q8reg3 <- feols(ln_sal_hr ~ d_tresfil_prev + idade + idade_2 + educa + branco, data = filter(q4base, trab == 1), se = "hetero")

###Questão 9----

q9reg1 <- feols(trab ~ 1 | d_tresfil ~ d_filcas + idade + idade_2 + educa + branco, data = q4base, se = "hetero")

q9reg2 <- feols(horas ~ 1 | d_tresfil ~ d_filcas + idade + idade_2 + educa + branco, data = filter(q4base, trab == 1), se = "hetero")

q9reg3 <- feols(ln_sal_hr ~ 1 | d_tresfil ~ d_filcas + idade + idade_2 + educa + branco, data = filter(q4base, trab == 1), se = "hetero")
