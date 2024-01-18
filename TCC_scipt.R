#################################################################################################
# Projeto: TCC - Relação Dívida/PIB => Crescimento
# Objetivo: A) Selecionar estudo mais adequado para replicar com dados da América Latina: Ash et al. (2020)
#           B) Baixar e tratar dados dos países da América Latina
#           C) Realizar regressão, com os controles corretos e diferentes métodos de estimação
#           # Proposta de tempo 1971 (pois há dados para dívida de 1970) até 2015 (pois há dados para o pib até 2019)
# Responsável: João Lins
# Data de início: 26/11/2023
#################################################################################################

#Git: italopedrosa
#mudança

#limpando
rm(list = ls())
options(scipen = 10000, digits = 4)

#Packages
library(tidyverse)
library(openxlsx)
library(XLConnect)
library(readxl)
library(stringr)
library(plm)
library(ivreg)
library(zoo)


#A) Heimberger####

  #Base de artigos Heimberger
  BaseH <- read.csv("base_heimberger.csv")


  #Apenas obs de artigos que lidam com reversão causal e endogeneidade 
  BaseH_filtrada <- dplyr::filter(base_heimberger, TacklingEndogeneity == 1)

  # -0.001 = 
  #Quais os papers com estimações mais próximas à -0.001 ?
  arrange(select(BaseH, 1:3, coef.corr = CorrelationCoefficientCorrected) %>% mutate(diferença = coef.corr  - (-0.001)), abs(diferença)) 


  
  
  Ash <- BaseH %>% filter(paperid == 5)
#Montando Base###
  

#-----------------------Montando Base--------------------------#
  
 
#B) Montando Base ####
  
#----------------MBAYE----------------------------------------------#
  #Pegando apenas informações relativas à dívida pública
  Base_Mbaye <- read.xlsx("G:\\Meu Drive\\Economia Facul\\TCC\\TCC\\base_mbaye.xlsx", sheet = "rawdata",startRow = 3,colNames = T, cols = c(1:5, 12:15,25:28))
  
  #Filtrando apenas América Latina
    #Simplificando nomes da Venezuela e Bahamas
    #Pegando apenas colunas desejadas
  Base_Mbaye.AL <- Base_Mbaye %>% filter(dept == "WHD", ! country %in% c("United States", "Canada")) %>%
    mutate(country = trimws(gsub("[^a-zA-Z &.].*", "", country))) %>% 
    select(-1,-2,-5)
    #mutate(country = ifelse(test = grepl(pattern = ",", country) ==1 , sapply(strsplit(country, ","), `[`, 1), country))
  
  #Ano Mínimo com dados disponíveis para cada país
  Lista_ano_min <- Base_Mbaye.AL %>% filter(! is.na(Base_Mbaye.AL$ps_data)|! is.na(Base_Mbaye.AL$nfps_data)| ! is.na(Base_Mbaye.AL$gg_data)| ! is.na(Base_Mbaye.AL$cg_data)) %>% group_by(country) %>% summarise(ano_min = min(year))
  
  #Países com dados pelo menos desde 1970
  Lista_paises_aptos <- Lista_ano_min %>% filter(ano_min <= 1970)
  
  #Filtrando Países Aptos e Dados apenas a partir de 1970
  #24 países com 49 anos para cada
    #Renomeando colunas
  Base_Mbaye.AL <- Base_Mbaye.AL %>% filter(country %in% Lista_paises_aptos$country, year >= 1970) %>% 
    rename_with(~sub("_data", "",.), contains("_data"))
  
#---------------PWT1001-----------------------------------------------#
  #Dados PWT #### Puxando colunas desejadas \\ simplificando nomes (Bahama, Bolivia e Venezuela possuiem nomes ligeiramente diferentes)
    #Renomeando colunas
  Base_PWT <- read.xlsx("G:\\Meu Drive\\Economia Facul\\TCC\\TCC\\base_pwt1001.xlsx", sheet = "Data", colNames = T, cols = c(2,4,7,14,29,42:45)) %>% 
    mutate(country = trimws(gsub("[^a-zA-Z &.].*", "", country))) %>% 
    rename(realgdp = 4, exchg_rate = 5, g_share = 6, x_share = 7, m_share = 8, r_share = 9)
  
  
  #Filtrando países aptos e dados a partir de 1970
  Base_PWT.AL <- dplyr::filter(Base_PWT, country %in% Lista_paises_aptos$country, year >= 1969)
  

#---------------WDI-----------------------------------------------#
  #Dados WDI #### Transformando linhas em colunas e colunas em linhas
  #Renomeando colunas, limpando dados de país e ano, passando valores para numeric  
  Base_WDI <- read.xlsx("G:\\Meu Drive\\Economia Facul\\TCC\\TCC\\base_wdi.xlsx", sheet = "Data") %>% select(-2,-4) %>% filter(!grepl("^Last|Data",Country.Name))
  Base_WDI <- Base_WDI%>% 
    pivot_longer(cols = c(3:ncol(Base_WDI)), names_to = "year") %>% select(1,3,2,4) %>%
    group_by(Country.Name) %>% 
    pivot_wider(names_from = Series.Name, values_from = c(value)) %>% ungroup() %>% 
    select(country = 1,2,age_dependency=3, urban_pop = 4,   gross_savings = 6, inflation_cp = 5, inflation_def = 7) 
  Base_WDI <- Base_WDI %>%    
    mutate(country = trimws(gsub("[^a-zA-Z &.].*", "", country)), year = as.numeric(substr(year,1,4)), across(c(3:7), as.numeric))
  
 

#----Criando e Transformando Variáveis-------------------------#
  #Base Mbaye: 1970 - 2018
  #Base PWT: 1970 - 2019
  #Base WDI: 1970 - 2022
  #Algumas modificações em cada variável devem ser feitas
    #1) Nas bases Mbaye e WDI: Apenas usar uma variável para o (a) nível de endividamento, bem como (b) inflação de um país
    #2) Na base PWT: Transformar dados primários naqueles que serão usados como controles
        # criar: pib per capita; crescimento do PIB (média 5 anos na frente); crescimento pop;  comercio/pib; 
  
  ######### Rodar sem arbitrariedade dos dados da dívida pública
  #1)
  # a)Quais países tem deficiência nos dados da dívida do governo central (variável mais presente entre os países da AL)?
  # prioridade: cg > gg > nfps > ps
  Base_Mbaye.AL %>% group_by(country) %>% summarise_all(~sum(!is.na(.))) %>% filter(cg < 49)
  
  #View(Base_Mbaye.AL %>% filter(is.na(cg)))
  #View(Base_Mbaye.AL %>% filter(country == "Nicaragua"))
  
  # Nicaragua tem muitas observações com a dívida = public sector, que é bem diferente. Será que é melhor tirar
  
  Base_Mbaye.AL.limpa <- Base_Mbaye.AL %>% mutate(debt_gdp = ifelse(!is.na(cg), cg,
                                                              ifelse(!is.na(gg), gg,
                                                                     ifelse(!is.na(nfps), nfps, ps)))) %>% 
    select(country, year, debt_gdp)
  
  # b)Quais países tem deficiência nos dados do deflator do pib (variável mais presente entre os países da AL)?
  # melhor usar o deflator do que o "ao consumidor", pois tem mais dados
  # Barbados (a partir de 75), Grenada (a partir de 78) e Venezuela (até 2014) não tem dados para toda a série
  
  #print(Base_WDI %>% group_by(country) %>% summarise_all(~sum(!is.na(.))) %>% filter(inflation_def < 53), n =100)
  #print(Base_WDI %>% filter(country == "Venezuela"))
  
  Base_WDI.limpa <- Base_WDI %>% mutate(inflation = ifelse(!is.na(inflation_def), inflation_def, inflation_cp)) %>% 
    select(c(1:5), inflation)
  
  #2) 
  Base_PWT.AL.limpa <- Base_PWT.AL %>% group_by(country) %>% 
    mutate(realgdp.pc = realgdp/pop, g.realgdp.pc = log(realgdp.pc) - log(dplyr::lag(realgdp.pc)), g.avg5.realgdp.pc = (log(dplyr::lead(realgdp.pc, 5)) - log(dplyr::lead(realgdp.pc,1)))/5, g.pop = log(pop) - log(dplyr::lag(pop)), trade.gdp = (x_share + abs(m_share)) + r_share) %>%
    ungroup() %>% 
    select((1:3),5,g_share,c(10:14))
  
    #Foma diferente de calcular taxa de crescimento (???)
    Base_PWT.AL.limpa2 <- Base_PWT.AL %>% group_by(country) %>% 
      mutate(realgdp.pc = realgdp/pop, g.realgdp.pc = (realgdp.pc/dplyr::lag(realgdp.pc)) - 1, g.avg5.realgdp.pc = ((dplyr::lead(realgdp.pc, 5)/dplyr::lead(realgdp.pc,1)) -1 )/5, g.pop = (pop/dplyr::lag(pop)) - 1, trade.gdp = (x_share + abs(m_share)) + r_share) %>%
      ungroup() %>% 
      select((1:3),5,g_share,c(10:14))
    
  #----------Consolidando Base com Variáveis de interesse-------------------------#
  #Juntando informações, filtrando pelo último ano em que a média do cresciemento futuro está disponível
  #Multiplicando crescimentos por 100 para ficar em % e níveis colocados em log
  # 
  #Retirando países com menos de 1M de habitantes em 2014 ("Bahamas","Barbados","Grenada", "Guyana", "St. Vincent and the Grenadines")
  #Retirando países por outros motivos
  Base_Consolidada <- left_join(Base_Mbaye.AL.limpa, Base_PWT.AL.limpa2, by = c("country", "year")) %>% 
    left_join(Base_WDI.limpa, by = c("country", "year")) %>% filter(year < 2015) %>% 
    dplyr::select(1:2,g.avg5.realgdp.pc ,debt_gdp, g.realgdp.pc, pop, g.pop,age_dependency,realgdp.pc, urban_pop, exchg_rate, trade.gdp, g_share, gross_savings, inflation) %>% 
    mutate(country = as.factor(country),g.avg5.realgdp.pc = g.avg5.realgdp.pc * 100, g.realgdp.pc = g.realgdp.pc*100,
           g.pop = g.pop*100, pop = log(pop), realgdp.pc = log(realgdp.pc), exchg_rate = log(exchg_rate), trade.gdp = trade.gdp*100, g_share = g_share *100  ) %>% 
    filter(!country %in% c("Bahamas","Barbados","Grenada", "Guyana", "St. Vincent and the Grenadines","Ecuador", "Nicaragua"))
                                                                        
  
  Base_Consolidada2 <- Base_Consolidada %>% group_by(country) %>% mutate(debt_gdp =  na.approx(debt_gdp))
  
  
  
  ggplot(data = Base_Consolidada2,aes(x = year, y = debt_gdp) ) +
    geom_line() +
    facet_wrap(.~country)
  #rm(list = setdiff(ls(), c("Base_Consolidada", "Lista_paises_aptos")))
  
   
  
  
#C) Regressão ####
  #newey west (correção de heterocedasticidade e autocorrelação)
  #vcovnw
#Bloco Sem Especificações de Ash et al. (2020) ----------#   
    
  #Crescimento Contemporâneo; Sem Controle
    modelo1 <- plm(g.realgdp.pc ~ debt_gdp, data = Base_Consolidada, model = "pooling")
    summary(modelo1, vcov = vcovNW)  
      #Resultado: -0.01999 com significância a 0.01
    
    #Crescimento Médio Futuro; Sem Controle
    modelo2 <- plm(g.avg5.realgdp.pc ~ debt_gdp, data = Base_Consolidada, model = "pooling")
    summary(modelo2, vcov = vcovNW)
      #Resultado: -0.000361 sem significância  
    
    #Crescimento Médio Futuro; Com Controle
    modelo3 <- plm(g.avg5.realgdp.pc ~ debt_gdp + pop + g.pop + age_dependency + realgdp.pc + urban_pop + exchg_rate + trade.gdp + g_share + gross_savings+ inflation, data = Base_Consolidada, model = "pooling")
    summary(modelo3, vcov = vcovNW)
      #Resultado: +0.004744 sem significância  
  
#Bloco Com Especificações de Ash et al. (2020) ----------#
  
  #Com FE de tempo
    
    #Crescimento Contemporâneo; Sem Controle
    modelo4 <- plm(g.realgdp.pc ~ debt_gdp, data = Base_Consolidada, effect = "time")
    summary(modelo4, vcov = vcovNW)  
      #Resultado: -0.01028 com significância a 0.01
    
    #Crescimento Médio Futuro; Sem Controle
    modelo5 <- plm(g.avg5.realgdp.pc ~ debt_gdp, data = Base_Consolidada, effect = "time")
    summary(modelo5, vcov = vcovNW)
      #Resultado: -0.00303 sem significância
    
    #Crescimento Médio Futuro; Com Controle
    modelo6 <- plm(g.avg5.realgdp.pc ~ debt_gdp + pop + g.pop + age_dependency + realgdp.pc + urban_pop + exchg_rate + trade.gdp + g_share + gross_savings+ inflation, data = Base_Consolidada, effect = "time")
    summary(modelo6)
    #Resultado: -0.008903 sem significância 
  
  #Com FE de tempo + Controle de Crescimento Lagado
    
    #Crescimento Contemporâneo; Sem Controle
    modelo7 <- plm(g.realgdp.pc ~ debt_gdp + lag(g.realgdp.pc,1), data = Base_Consolidada, effect = "time",index = c("country", "year"))
    summary(modelo7, vcov = vcovNW)  
    #Resultado: -0.00745 com significância a 0.05
    
    #Crescimento Médio Futuro; Sem Controle
    modelo8 <- plm(g.avg5.realgdp.pc ~ debt_gdp + lag(g.realgdp.pc,1), data = Base_Consolidada, effect = "time",index = c("country", "year"))
    summary(modelo8, vcov = vcovNW)
    #Resultado: -0.00256 sem significância
    
    #Crescimento Médio Futuro; Com Controle
    modelo9 <- plm(g.avg5.realgdp.pc ~ debt_gdp + lag(g.realgdp.pc,1) + pop + g.pop + age_dependency + realgdp.pc + urban_pop + exchg_rate + trade.gdp + g_share + gross_savings+ inflation, data = Base_Consolidada, effect = "time",index = c("country", "year"))
    summary(modelo9, vcov = vcovNW)
    #Resultado: -0.008516 sem significância
    
  #Com FE de tempo + IV da dívida em t-5
    
    #Crescimento Contemporâneo; Sem Controle
    modelo10 <- plm(g.realgdp.pc ~ debt_gdp | . - debt_gdp + lag(debt_gdp,5), data = Base_Consolidada, effect = "time",index = c("country", "year"))
    summary(modelo10, vcov = vcovNW)
    #Resultado: -0.00724 sem significancia
    
    #Crescimento Médio Futuro; Sem Controle
    modelo11 <- plm(g.avg5.realgdp.pc ~ debt_gdp| . - debt_gdp + lag(debt_gdp,5), data = Base_Consolidada, effect = "time",index = c("country", "year"))
    summary(modelo11, vcov = vcovNW)
    #Resultado: 0.00379 sem significância
    
    #Crescimento Médio Futuro; Com Controle
    modelo12 <- plm(g.avg5.realgdp.pc ~ debt_gdp + pop + g.pop + age_dependency + realgdp.pc + urban_pop + exchg_rate + trade.gdp + g_share + gross_savings+ inflation| . - debt_gdp + lag(debt_gdp,5), data = Base_Consolidada, effect = "time",index = c("country", "year"))
    summary(modelo12, vcov = vcovNW)
    #Resultado: 0.0077858 sem significância
    
  #Com FE de tempo + Controle de Crescimento Lagado + IV da dívida em t-5
    
    #Crescimento Contemporâneo; Sem Controle
    modelo13 <- plm(g.realgdp.pc ~ debt_gdp + lag(g.realgdp.pc,1)| . - debt_gdp + lag(debt_gdp,5), data = Base_Consolidada, effect = "time",index = c("country", "year"))
    summary(modelo13, vcov = vcovNW)
    #Resultado: -0.00594 sem significancia
    
    #Crescimento Médio Futuro; Sem Controle
    modelo14 <- plm(g.avg5.realgdp.pc ~ debt_gdp + lag(g.realgdp.pc,1)| . - debt_gdp + lag(debt_gdp,5), data = Base_Consolidada, effect = "time",index = c("country", "year"))
    summary(modelo14, vcov = vcovNW)
    #Resultado: 0.00413 sem significância
    
    #Crescimento Médio Futuro; Com Controle
    modelo15 <- plm(g.avg5.realgdp.pc ~ debt_gdp + lag(g.realgdp.pc,1) + pop + g.pop + age_dependency + realgdp.pc + urban_pop + exchg_rate + trade.gdp + g_share + gross_savings+ inflation| . - debt_gdp + lag(debt_gdp,5), data = Base_Consolidada, effect = "time", index = c("country", "year"))
    summary(modelo15, vcov = vcovNW)
    #Resultado: 0.0075700 sem significância
  
  #Com FE de tempo e de país 
    
    #Crescimento Contemporâneo; Sem Controle
    modelo16 <- plm(g.realgdp.pc ~ debt_gdp, data = Base_Consolidada,model = "within", effect = "twoways",index = c("country", "year"))
    summary(modelo16, vcov = vcovNW)  
    #Resultado: -0.00701 com significância a 0.05
    
    #Crescimento Médio Futuro; Sem Controle
    modelo17 <- plm(g.avg5.realgdp.pc ~ debt_gdp, data = Base_Consolidada,model = "within", effect = "twoways",index = c("country", "year"))
    summary(modelo17, vcov = vcovNW)
    #Resultado: -0.000688 sem significância
    
    #Crescimento Médio Futuro; Com Controle
    modelo18 <- plm(g.avg5.realgdp.pc ~ debt_gdp + pop + g.pop + age_dependency + realgdp.pc + urban_pop + exchg_rate + trade.gdp + g_share + gross_savings+ inflation, data = Base_Consolidada,model = "within", effect = "twoways",index = c("country", "year"))
    summary(modelo18, vcov = vcovNW)
    #Resultado: 0.0110775 sem significância 
    
  #Com FE de tempo e de país + IV da dívida em t-5
    
    #Crescimento Contemporâneo; Sem Controle
    modelo19 <- plm(g.realgdp.pc ~ debt_gdp| . - debt_gdp + lag(debt_gdp,5), data = Base_Consolidada,model = "within", effect = "twoways",index = c("country", "year"))
    summary(modelo19, vcov = vcovNW)  
    #Resultado: 0.0224 com significância a 0.05
    
    #Crescimento Médio Futuro; Sem Controle
    modelo20 <- plm(g.avg5.realgdp.pc ~ debt_gdp| . - debt_gdp + lag(debt_gdp,5), data = Base_Consolidada,model = "within", effect = "twoways",index = c("country", "year"))
    summary(modelo20, vcov = vcovNW)
    #Resultado: 0.0281 sem significância
    
    #Crescimento Médio Futuro; Com Controle
    modelo21 <- plm(g.avg5.realgdp.pc ~ debt_gdp + pop + g.pop + age_dependency + realgdp.pc + urban_pop + exchg_rate + trade.gdp + g_share + gross_savings+ inflation| . - debt_gdp + lag(debt_gdp,5), data = Base_Consolidada,model = "within", effect = "twoways",index = c("country", "year"))
    summary(modelo21, vcov = vcovNW)
    #Resultado: 0.023057 sem significância 
    
    plmtest(g.avg5.realgdp.pc ~ debt_gdp + pop + g.pop + age_dependency + realgdp.pc + urban_pop + exchg_rate + trade.gdp + g_share + gross_savings+ inflation| . - debt_gdp + lag(debt_gdp,5),data = Base_Consolidada, effect = "twoways", type ="ghm")
    #rejeita-se a hipótese de
    
    
