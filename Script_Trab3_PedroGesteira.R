if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, haven, stargazer)

##Questão 1----

dir <- getwd()

censo <- read_dta("data/censo_2000_rio.dta") %>%  #Leitura da base
  filter(v0103 == 3304557)   #Delimitando para englobar apenas Rio cidade
  
#Selecionando as variáveis de interesse

censo <- select(c(v0401, v4752, v4300, v0408, v0439, v4513, v0453, v4620, v4621, v4622))

Renomeando as variáveis

names(censo) <- c("d_mulher", "idade", "educa", "branco", "trab", "salario", "horas", "n_fil", "n_fil_o", "n_fil_a")

#Criando as variáveis pedidas

censo <- censo %>% mutate(d_mulher = ifelse(d_mulher == 2, 1, 0),
                          trab = ifelse(trab == 1, 1, 0),
                          idade_2 = (idade^2),
                          sal_hr = (salario/horas),
                          ln_sal_hr = log(sal_hr),
                          d_fil = ifelse(n_fil == 0, 0, 1))


#Filtrando para pessoas entre 18 a 60

censo <- censo %>% filter(d_mulher == 1 & idade<=60 & idade>=18)

#Relocando as colunas

censo <- censo[c(1:2, 11, 3:7, 12:13, 8:10, 14)]
