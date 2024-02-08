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
  BaseH <- read.csv(file = "G:\\Meu Drive\\Economia Facul\\TCC\\TCC\\base_heimberger.csv")


  #Apenas obs de artigos que lidam com reversão causal e endogeneidade 
  BaseH_filtrada <- dplyr::filter(base_heimberger, TacklingEndogeneity == 1)

  # -0.001 = 
  #Quais os papers com estimações mais próximas à -0.001 ?
  arrange(select(BaseH, 1:3, coef.corr = CorrelationCoefficientCorrected) %>% mutate(diferença = coef.corr  - (-0.001)), abs(diferença)) 


  
  
  Ash <- BaseH %>% filter(paperid == 5)

 
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
  Base_PWT <- read.xlsx("G:\\Meu Drive\\Economia Facul\\TCC\\TCC\\base_pwt1001.xlsx", sheet = "Data", colNames = T, cols = c(2,4,6,7,29,42:45)) %>% 
    mutate(country = trimws(gsub("[^a-zA-Z &.].*", "", country))) %>% 
    rename(realgdp = 3, exchg_rate = 5, g_share = 6, x_share = 7, m_share = 8, r_share = 9) %>% 
    select(1,2,4,3, everything())
  
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
  Lista_divida <- Base_Mbaye.AL %>% group_by(country) %>% summarise_all(~sum(!is.na(.)))
  
  
  #Base que Escolhe só países que possuem dados completos de dívida pública (cg ou nfps)
  #Panama e Nicaragua são retirados pois 
  Base_Mbaye.AL.mix <- Base_Mbaye.AL %>% mutate(debt_gdp = ifelse(!country %in% c("Ecuador","Dominican Republic", "Panama" ), cg,
                                                                  ifelse(!country %in% c("Panama"),nfps, gg))) %>% 
    select(country, year, debt_gdp) %>% dplyr::filter(country != "Nicaragua") %>%  ungroup()
      #Para Jamaica e Uruguai, falta algum (pouco) dado da dívida.mix
  
  #Base que usa apenas dívida do governo central, priorizando coerência
    #Países com muitos dados de cg faltantes: Rep. Domenicana, Equador, Nicaragua e Panamá
  
  Base_Mbaye.AL.cg <-  Base_Mbaye.AL %>% rename(debt_gdp =cg) %>%
    filter(country %in% (Lista_divida %>% filter(cg>40))$country) %>% 
    select(country, year, debt_gdp) %>% ungroup()
      #Para Jamaica, Mexico, St Vicent, Uruguai falta algum (pouco) dado da dívida cg
      
      #Justificando o uso da estatística nfsp em vez de cg para Equador e RepDom
          equador <- Base_Mbaye.AL %>% filter(country == "Ecuador", year>=1990) %>% select(1:6)
          cor(equador$cg, equador$nfps)
          repdom <- Base_Mbaye.AL %>% filter(country == "Dominican Republic", year>=2000) %>% select(1:6)
          cor(repdom$cg, repdom$nfps)
  
      #Checando se há valores faltando
        print(Base_Mbaye.AL.mix %>% group_by(country) %>% summarise_all(~sum(!is.na(.))), n =30)
        ggplot(data = Base_Mbaye.AL.cg ,aes(x = year, y = debt_gdp) ) + geom_line() + facet_wrap(.~country)
      
  #Interpolação de dados faltantes
    Base_Mbaye.AL.mix <- Base_Mbaye.AL.mix %>% group_by(country) %>% mutate(debt_gdp =  na.approx(debt_gdp)) %>% ungroup()
    Base_Mbaye.AL.cg <- Base_Mbaye.AL.cg %>% group_by(country) %>% mutate(debt_gdp =  na.approx(debt_gdp)) %>% ungroup()
  
  
  
  # b)Quais países tem deficiência nos dados do deflator do pib (variável mais presente entre os países da AL)?
  # melhor usar o deflator do que o "ao consumidor", pois tem mais dados
  # Barbados (a partir de 75), Grenada (a partir de 78) e Venezuela (até 2014) não tem dados para toda a série
  
  #print(Base_WDI %>% group_by(country) %>% summarise_all(~sum(!is.na(.))) %>% filter(inflation_def < 53), n =100)
  #print(Base_WDI %>% filter(country == "Venezuela"))
  
  Base_WDI.limpa <- Base_WDI %>% mutate(inflation = ifelse(!is.na(inflation_def), inflation_def, inflation_cp)) %>% 
    select(c(1:5), inflation)
  
    #Checando se há valores faltando
    print(Base_WDI.limpa %>% group_by(country) %>% summarise_all(~sum(!is.na(.))), n =30)
    #Quase não há dados para gross_savings!!!!
  
  
  #2)
  #Forma log para calcular taxa de crescimento
  Base_PWT.AL.limpa1 <- Base_PWT.AL %>% group_by(country) %>% 
    mutate(realgdp.pc = realgdp/pop, g.realgdp.pc = log(realgdp.pc) - log(dplyr::lag(realgdp.pc)), g.avg5.realgdp.pc = (log(dplyr::lead(realgdp.pc, 6)) - log(dplyr::lead(realgdp.pc,1)))/5, g.pop = log(pop) - log(dplyr::lag(pop)), trade.gdp = (x_share + abs(m_share)) + r_share) %>%
    ungroup() %>% 
    select((1:3),5,g_share,c(10:14))
  
  #Foma padrão para calcular taxa de crescimento 
  Base_PWT.AL.limpa2 <- Base_PWT.AL %>% group_by(country) %>% 
    mutate(realgdp.pc = realgdp/pop, g.realgdp.pc = (realgdp.pc/dplyr::lag(realgdp.pc)) - 1, g.avg5.realgdp.pc = ((dplyr::lead(realgdp.pc, 6)/dplyr::lead(realgdp.pc,1)) -1 )/5, g.pop = (pop/dplyr::lag(pop)) - 1, trade.gdp = (x_share + abs(m_share)) + r_share) %>%
    ungroup() %>% 
    select((1:3),5,g_share,c(10:14))
  
  #Foma média ipsis litteris de cálculo da média da taxa de crescimento
  Base_PWT.AL.limpa3 <- Base_PWT.AL %>% group_by(country) %>% 
    mutate(realgdp.pc = realgdp/pop, g.realgdp.pc = (realgdp.pc/dplyr::lag(realgdp.pc)) - 1, g.avg5.realgdp.pc = (dplyr::lead(g.realgdp.pc,1) + dplyr::lead(g.realgdp.pc,2) +dplyr::lead(g.realgdp.pc,3)+dplyr::lead(g.realgdp.pc,4)+dplyr::lead(g.realgdp.pc,5))/5, g.pop = (pop/dplyr::lag(pop)) - 1, trade.gdp = (x_share + abs(m_share)) + r_share) %>%
    ungroup() %>% 
    select((1:3),5,g_share,c(10:14))
  
  
  
  
  
  #----------Consolidando Base com Variáveis de interesse-------------------------#
  
  ### BASE PRINCIPAL
    
    #Juntando informações, filtrando pelo último ano em que a média do cresciemento futuro está disponível
    Base_Consolidada <- left_join(Base_Mbaye.AL.mix, Base_PWT.AL.limpa2, by = c("country", "year")) %>% 
      left_join(Base_WDI.limpa, by = c("country", "year")) %>% filter(year <=2013)
    
    #Checando se há valores faltando
      print(Base_Consolidada %>% group_by(country) %>% summarise_all(~sum(!is.na(.))) %>% select(-c(2:5)), n =30)
      print(Base_Consolidada  %>% filter(rowSums(is.na(.)) > 0))
    
    #Ordenando Colunas e retirando variável de Gross_Savings por falta de dados!!!!
    Base_Consolidada <- Base_Consolidada %>% 
      dplyr::select(1:2,g.avg5.realgdp.pc ,debt_gdp, g.realgdp.pc, pop, g.pop,age_dependency,realgdp.pc, urban_pop, exchg_rate, trade.gdp, g_share, inflation)
      
    #Multiplicando crescimentos por 100 para ficar em % e níveis colocados em log
    #Retirando países com menos de 1M de habitantes em 2014 ("Bahamas","Barbados","Grenada", "Guyana", "St. Vincent and the Grenadines")
    #Retirando países por outros motivos (Ecuador, Panama, El Salvador utiliza Dólar USD, Nicarágua possui dados ruins para a dívida)
    Lista_final <- Base_Consolidada %>% group_by(country) %>% filter(year==2013 & pop >= 1, !country %in% c("Panama", "Nicaragua")) %>% select(country) %>% pull()
    
    Base_Consolidada <-  Base_Consolidada %>% 
      mutate(country = as.factor(country),g.avg5.realgdp.pc = g.avg5.realgdp.pc * 100, g.realgdp.pc = g.realgdp.pc*100,
             g.pop = g.pop*100, pop = log(pop), realgdp.pc = log(realgdp.pc), exchg_rate = log(exchg_rate), trade.gdp = trade.gdp*100, g_share = g_share *100  ) %>% 
      filter(country %in% Lista_final) %>% ungroup()
    
      
  
  ggplot(data = Base_Consolidada,aes(x = year, y = debt_gdp) ) +
    geom_line() +
    facet_wrap(.~country)
  ggplot(data = Base_Consolidada,aes(x = year, y = g.avg5.realgdp.pc) ) +
    geom_line() +
    facet_wrap(.~country)
  #rm(list = setdiff(ls(), c("Base_Consolidada", "Lista_paises_aptos")))
  
  
  ### VARIAÇÕES DA BASE PRINCIPAL
  
  
#C) Regressão ####
  #newey west (correção de heterocedasticidade e autocorrelação)
  #vcovnw
#Bloco Sem Especificações de Ash et al. (2020) ----------#   
    
  #Crescimento Contemporâneo; Sem Controle
    modelo1 <- plm(g.realgdp.pc ~ debt_gdp, data = Base_Consolidada, model = "pooling")
    summary(modelo1, vcov = vcovNW)
      #Resultado: -0.02667 com significância a 0.001
    
    #Crescimento Médio Futuro; Sem Controle
    modelo2 <- plm(g.avg5.realgdp.pc ~ debt_gdp, data = Base_Consolidada, model = "pooling")
    summary(modelo2, vcov = vcovNW)
      #Resultado: 0.00818 sem significância  
    
    #Crescimento Médio Futuro; Com Controle
    modelo3 <- plm(g.avg5.realgdp.pc ~ debt_gdp + pop + g.pop + age_dependency + realgdp.pc + urban_pop + exchg_rate + trade.gdp + g_share + inflation, data = Base_Consolidada, model = "pooling")
    summary(modelo3, vcov = vcovNW)
      #Resultado: 0.007176 sem significância  
  
#Bloco Com Especificações de Ash et al. (2020) ----------#
  
  #Com FE de tempo
    
    #Crescimento Contemporâneo; Sem Controle
    modelo4 <- plm(g.realgdp.pc ~ debt_gdp, data = Base_Consolidada, effect = "time")
    summary(modelo4, vcov = vcovNW)  
      #Resultado: -0.02135 com significância a 0.01
    
    #Crescimento Médio Futuro; Sem Controle
    modelo5 <- plm(g.avg5.realgdp.pc ~ debt_gdp, data = Base_Consolidada, effect = "time")
    summary(modelo5, vcov = vcovNW)
      #Resultado: -0.00161 sem significância
    
    #Crescimento Médio Futuro; Com Controle
    modelo6 <- plm(g.avg5.realgdp.pc ~ debt_gdp + pop + g.pop + age_dependency + realgdp.pc + urban_pop + exchg_rate + trade.gdp + g_share + inflation, data = Base_Consolidada, effect = "time")
    summary(modelo6)
    #Resultado: -0.003594 sem significância 
  
  #Com FE de tempo + Controle de Crescimento Lagado
    
    #Crescimento Contemporâneo; Sem Controle
    modelo7 <- plm(g.realgdp.pc ~ debt_gdp + lag(g.realgdp.pc,1), data = Base_Consolidada, effect = "time",index = c("country", "year"))
    summary(modelo7, vcov = vcovNW)  
    #Resultado: -0.01450 com significância a 0.01
    
    #Crescimento Médio Futuro; Sem Controle
    modelo8 <- plm(g.avg5.realgdp.pc ~ debt_gdp + lag(g.realgdp.pc,1), data = Base_Consolidada, effect = "time",index = c("country", "year"))
    summary(modelo8, vcov = vcovNW)
    #Resultado: -0.000993 sem significância
    
    #Crescimento Médio Futuro; Com Controle
    modelo9 <- plm(g.avg5.realgdp.pc ~ debt_gdp + lag(g.realgdp.pc,1) + pop + g.pop + age_dependency + realgdp.pc + urban_pop + exchg_rate + trade.gdp + g_share + inflation, data = Base_Consolidada, effect = "time",index = c("country", "year"))
    summary(modelo9, vcov = vcovNW)
    #Resultado: -0.003034 sem significância
    
  #Com FE de tempo + IV da dívida em t-5
    
    #Crescimento Contemporâneo; Sem Controle
    modelo10 <- plm(g.realgdp.pc ~ debt_gdp | . - debt_gdp + lag(debt_gdp,5), data = Base_Consolidada, effect = "time",index = c("country", "year"))
    summary(modelo10, vcov = vcovNW)
    #Resultado: -0.00487 sem significancia
    
    #Crescimento Médio Futuro; Sem Controle
    modelo11 <- plm(g.avg5.realgdp.pc ~ debt_gdp| . - debt_gdp + lag(debt_gdp,5), data = Base_Consolidada, effect = "time",index = c("country", "year"))
    summary(modelo11, vcov = vcovNW)
    #Resultado: 0.0110 sem significância
    
    #Crescimento Médio Futuro; Com Controle
    modelo12 <- plm(g.avg5.realgdp.pc ~ debt_gdp + pop + g.pop + age_dependency + realgdp.pc + urban_pop + exchg_rate + trade.gdp + g_share +  inflation| . - debt_gdp + lag(debt_gdp,5), data = Base_Consolidada, effect = "time",index = c("country", "year"))
    summary(modelo12, vcov = vcovNW)
    #Resultado: 0.016574 sem significância
    
  #Com FE de tempo + Controle de Crescimento Lagado + IV da dívida em t-5
    
    #Crescimento Contemporâneo; Sem Controle
    modelo13 <- plm(g.realgdp.pc ~ debt_gdp + lag(g.realgdp.pc,1)| . - debt_gdp + lag(debt_gdp,5), data = Base_Consolidada, effect = "time",index = c("country", "year"))
    summary(modelo13, vcov = vcovNW)
    #Resultado: -0.00408 sem significancia
    
    #Crescimento Médio Futuro; Sem Controle
    modelo14 <- plm(g.avg5.realgdp.pc ~ debt_gdp + lag(g.realgdp.pc,1)| . - debt_gdp + lag(debt_gdp,5), data = Base_Consolidada, effect = "time",index = c("country", "year"))
    summary(modelo14, vcov = vcovNW)
    #Resultado: 0.0111 sem significância
    
    #Crescimento Médio Futuro; Com Controle
    modelo15 <- plm(g.avg5.realgdp.pc ~ debt_gdp + lag(g.realgdp.pc,1) + pop + g.pop + age_dependency + realgdp.pc + urban_pop + exchg_rate + trade.gdp + g_share + inflation| . - debt_gdp + lag(debt_gdp,5), data = Base_Consolidada, effect = "time", index = c("country", "year"))
    summary(modelo15, vcov = vcovNW)
    #Resultado: 0.016367 sem significância
  
  #Com FE de tempo e de país 
    
    #Crescimento Contemporâneo; Sem Controle
    modelo16 <- plm(g.realgdp.pc ~ debt_gdp, data = Base_Consolidada,model = "within", effect = "twoways",index = c("country", "year"))
    summary(modelo16, vcov = vcovNW)  
    #Resultado: -0.02795 com significância a 0.01
    
    #Crescimento Médio Futuro; Sem Controle
    modelo17 <- plm(g.avg5.realgdp.pc ~ debt_gdp, data = Base_Consolidada,model = "within", effect = "twoways",index = c("country", "year"))
    summary(modelo17, vcov = vcovNW)
    #Resultado: 0.0115 sem significância
    
    #Crescimento Médio Futuro; Com Controle
    modelo18 <- plm(g.avg5.realgdp.pc ~ debt_gdp + pop + g.pop + age_dependency + realgdp.pc + urban_pop + exchg_rate + trade.gdp + g_share + inflation, data = Base_Consolidada,model = "within", effect = "twoways",index = c("country", "year"))
    summary(modelo18, vcov = vcovNW)
    #Resultado: -0.001763 sem significância 
    
  #Com FE de tempo e de país + IV da dívida em t-5
    
    #Crescimento Contemporâneo; Sem Controle
    modelo19 <- plm(g.realgdp.pc ~ debt_gdp| . - debt_gdp + lag(debt_gdp,5), data = Base_Consolidada,model = "within", effect = "twoways",index = c("country", "year"))
    summary(modelo19, vcov = vcovNW)  
    #Resultado: 0.0445 sem significância
    
    #Crescimento Médio Futuro; Sem Controle
    modelo20 <- plm(g.avg5.realgdp.pc ~ debt_gdp| . - debt_gdp + lag(debt_gdp,5), data = Base_Consolidada,model = "within", effect = "twoways",index = c("country", "year"))
    summary(modelo20, vcov = vcovNW)
    #Resultado: 0.0947 com significância a 0.05
    
    #Crescimento Médio Futuro; Com Controle
    modelo21 <- plm(g.avg5.realgdp.pc ~ debt_gdp + pop + g.pop + age_dependency + realgdp.pc + urban_pop + exchg_rate + trade.gdp + g_share +  inflation| . - debt_gdp + lag(debt_gdp,5), data = Base_Consolidada,model = "within", effect = "twoways",index = c("country", "year"))
    summary(modelo21, vcov = vcovNW)
    #Resultado: 0.095749 sem significância 
    
    plmtest(g.avg5.realgdp.pc ~ debt_gdp + pop + g.pop + age_dependency + realgdp.pc + urban_pop + exchg_rate + trade.gdp + g_share +  inflation| . - debt_gdp + lag(debt_gdp,5),data = Base_Consolidada, effect = "twoways", type ="ghm")
    #rejeita-se a hipótese de
    
    

    
    
#D) Robustez ####
    
  
    
    
    
    