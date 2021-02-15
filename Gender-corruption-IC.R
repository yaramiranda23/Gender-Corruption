## Baixando pacotes necessários 

library(tidyverse)
library(plm)
library(stargazer)


# Arrumando a base de dados 
#Base com todas as variáveis de econometria 
database <- read.csv("https://raw.githubusercontent.com/yaramiranda23/Econometria/master/database1.csv") %>% 
  select(Time, code, countryname, women, Colony, HDI, Rule_of_law, employedwomen) 
summary(database)
unique(database$countryname)
databasena <- database %>% drop_na()
unique(databasena$countryname)

#Adicionando base de dados de corrupção - Banco Mundial 
BM <- read.csv("https://raw.githubusercontent.com/yaramiranda23/Econometria/master/Corrup.csv")
BM <- select(BM, Time, Country.Code, Control.of.Corruption..Estimate..CC.EST.)
BM <- rename(BM, code = "Country.Code", corruption_index = "Control.of.Corruption..Estimate..CC.EST.")
BM$Time <- as.numeric(BM$Time)
databaseBM <- inner_join(database, BM, by = c("Time", "code"))


#Adicionando a variável PIB
GDP <- read.csv("https://raw.githubusercontent.com/yaramiranda23/Econometria/master/GDP.csv")


GDP <- GDP %>%  select('Time', 'Country.Code', "GDP.per.capita..current.US....NY.GDP.PCAP.CD.", "GDP..current.US....NY.GDP.MKTP.CD.") %>% 
  rename(code = "Country.Code", GDPpercap = "GDP.per.capita..current.US....NY.GDP.PCAP.CD.", 
         GDPall = "GDP..current.US....NY.GDP.MKTP.CD.") 
databaseBM <- inner_join(databaseBM, GDP, by = c('code', 'Time'))%>% distinct() # Continua com 167

databaseBMna <- databaseBM %>% drop_na()
unique(databaseBMna$countryname)

#Adicionando a variável sistema eleitoral 
#elec <- read.csv("https://raw.githubusercontent.com/yaramiranda23/Econometria/master/qogsystem.csv")%>% rename(code = "ccodealp", Time = "year", electoral_system = "gol_est") %>% 
  #select(!cname) %>% select(!X) %>% select(!region_label) 

#elecateg <- mutate (elec, electoral_system_majoritarian = ifelse(electoral_system == 1, 
                                                                 #"1", "0"))
#elecateg$electoral_system_majoritarian <- as.numeric(elecateg$electoral_system_majoritarian)

#databaseBM <- left_join(databaseBM, elecateg, by = c('code', 'Time')) #%>% select(Time:GDPpercap, electoral_system) 
#summary(databaseBM)

#databaseBMna <- databaseBM %>% drop_na()
#unique(databaseBMna$countryname)


#Transformando
databaseBM$women <- as.numeric(gsub(",", ".", databaseBM$women))
databaseBM$HDI <- as.numeric(gsub(",", ".", databaseBM$HDI))
databaseBM$Rule_of_law <- as.numeric(gsub(",", ".", databaseBM$Rule_of_law))
databaseBM$employedwomen <- as.numeric(gsub(",", ".", databaseBM$employedwomen))
databaseBM$GDPpercap <- as.numeric(gsub(",", ".", databaseBM$GDPpercap))

databaseBM <- databaseBM %>% filter(code != "BGR") %>% filter(code != "MNG") %>% filter(code !="MYS") %>% filter(code != "SDN") #Tirando a Bulgaria e mongolia 
databaseBM$corruption_index<- as.numeric(databaseBM$corruption_index)
summary(databaseBM)


#Resumo da base
summary(databaseBM)
databaseBM1 <- databaseBM %>% drop_na()
unique(databaseBM1$countryname)


#################### REGRESSÕES SEM COLÔNIA 

#Fixed effect
regFE <- plm(corruption_index ~ women+
               Rule_of_law+employedwomen+ GDPpercap - 1,
             data = databaseBM, 
             index = c("countryname", "Time"), 
             model = "within")
summary(regFE)

#Random effect
regRE <- plm(corruption_index ~ women+ +
               Rule_of_law+employedwomen+ GDPpercap  - 1,
             data = databaseBM, 
             index = c("countryname", "Time"), 
             model = "random")
summary(regRE)

# First difference
regFD <- plm(corruption_index ~ women +
               Rule_of_law+employedwomen+ GDPpercap - 1,
             data = databaseBM, 
             index = c("countryname", "Time"), 
             model = "fd")
summary(regFD)

#Pooled OLS
regpooling <- plm(corruption_index  ~ women+
                    Rule_of_law+employedwomen+ GDPpercap - 1,
                  data = databaseBM, 
                  index = c("countryname", "Time"), 
                  model = "pooling")
summary(regpooling)

#Tabela
stargazer(regFE,regFD, regRE, regpooling,   
          type = "text",
          align = TRUE,
          title = "Results", 
          column.labels=c("Fixed Effect","First Difference", "Random Effect", "Pooled OLS"), 
          covariate.labels=c("percentage of Woman in parliament", "Rule of Law", "Women Economic rights: percentage of employed women", "GDP per capita"))


#################### REGRESSÕES EM ESCADA ##########################

reg1 <- plm(corruption_index ~ women - 1,
             data = databaseBM, 
             index = c("countryname", "Time"), 
             model = "pooling")

reg2 <- plm(corruption_index ~ women + Rule_of_law -  1,
              data = databaseBM, 
              index = c("countryname", "Time"), 
              model = "pooling")

reg3 <- plm(corruption_index ~ women + Rule_of_law +  GDPpercap   - 1,
              data = databaseBM, 
              index = c("countryname", "Time"), 
              model = "pooling")


reg4 <- plm(corruption_index ~ women + Rule_of_law + GDPpercap  + employedwomen  - 1,
            data = databaseBM, 
            index = c("countryname", "Time"), 
            model = "pooling")


reg5 <- plm(corruption_index ~ women + Rule_of_law + GDPpercap  + employedwomen  + Colony - 1,
            data = databaseBM, 
            index = c("countryname", "Time"), 
            model = "pooling")

stargazer(reg, reg1, reg2, reg3, reg4, reg5,
          type = "text",
          align = TRUE,
          title = "Results Pooling OLS", 
          #column.labels=c("Fixed Effect","First Difference", "Random Effect", "Pooled OLS"), 
          covariate.labels=c("percentage of Woman in parliament", "Rule of Law", "GDP per capita", "Women Economic rights: percentage of employed women", "Colony"))



###### 2º hipótese
#Developed (>0.80 é very high human development, segundo UNDP)
datadev <- databaseBM %>% filter(HDI > 0.790) %>% distinct() 

regdeveloped <- plm(corruption_index ~ women +
                      Rule_of_law +  GDPpercap + employedwomen+ +Colony - 1,
                    data = datadev, 
                    index = c("countryname", "Time"), 
                    model = "within")



#Undeveloped ( 0.550–0.699 for medium human development, segundo UNDP)
dataundev <- databaseBM %>% filter(HDI < 0.699 & HDI > 0.550)  %>% distinct ()

regdeveloping <- plm(corruption_index ~ women +
                       Rule_of_law +  GDPpercap + employedwomen+ +Colony - 1,
                     data = dataundev, 
                     index = c("countryname", "Time"), 
                     model = "within")

stargazer(regdeveloped,regdeveloping,  
          type = "text",
          align = TRUE,
          title = "Results: FE", 
          column.labels=c("Very high HD","Medium HD"), 
          covariate.labels=c("percentage of Woman in parliament", "Rule of Law", "GDP per capita","Women Economic rights: percentage of employed women", "Colony"))





