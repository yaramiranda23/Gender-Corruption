## Baixando pacotes necessários 

library(tidyverse)
library(plm)
library(stargazer)


# Arrumando a base de dados 
#Base com todas as variáveis de econometria 
database <- read.csv("https://raw.githubusercontent.com/yaramiranda23/Econometria/master/database1.csv") %>% 
  select(Time, code, countryname, women, Colony, HDI, Rule_of_law, employedwomen) 
summary(database)


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


#Adicionando a variável de openness 
openness <- read.csv("openness.csv")
openness <- openness %>% select(Country.Code, Time, Imports.of.goods.and.services....of.GDP...NE.IMP.GNFS.ZS.) %>% 
  rename(code = "Country.Code", openness = "Imports.of.goods.and.services....of.GDP...NE.IMP.GNFS.ZS.")
databaseBM <- inner_join(databaseBM, openness, by = c('code', 'Time'))%>% distinct() 

#Adicionando variável de democracia eleitoral 
democ <- read.csv("Democracy.csv")
democ <- democ %>% select(geo, time, Electoral.pluralism.index..EIU.) %>% 
  rename(code = "geo", democ = "Electoral.pluralism.index..EIU.", Time = "time") %>% filter(Time != 2018) %>% filter(Time !=2019)
democ$code <- toupper(democ$code)
databaseBM <- inner_join(databaseBM, democ, by = c('code', 'Time'))%>% distinct()

head(databaseBM)


#Transformando
databaseBM$women <- as.numeric(gsub(",", ".", databaseBM$women))
databaseBM$HDI <- as.numeric(gsub(",", ".", databaseBM$HDI))
databaseBM$Rule_of_law <- as.numeric(gsub(",", ".", databaseBM$Rule_of_law))
databaseBM$employedwomen <- as.numeric(gsub(",", ".", databaseBM$employedwomen))
databaseBM$GDPpercap <- as.numeric(gsub(",", ".", databaseBM$GDPpercap))
databaseBM$openness <- as.numeric(gsub(",", ".", databaseBM$openness))
databaseBM$democ <- as.numeric(gsub(",", ".", databaseBM$democ))

databaseBM <- databaseBM %>% filter(code != "BGR") %>% filter(code != "MNG") %>% filter(code !="MYS") %>% filter(code != "SDN") #Tirando a Bulgaria e mongolia 
databaseBM$corruption_index<- as.numeric(databaseBM$corruption_index)



#Resumo da base
summary(databaseBM)
head(databaseBM)

write.csv(databaseBM, "databaseBM.csv")



#################### REGRESSÕES SEM COLÔNIA 

#Fixed effect
regFE <- plm(corruption_index ~ women+
               Rule_of_law+employedwomen+ log(GDPpercap) - 1,
             data = databaseBM, 
             index = c("countryname", "Time"), 
             model = "within")
summary(regFE)

#Random effect
regRE <- plm(corruption_index ~ women+ +
               Rule_of_law+employedwomen+ log(GDPpercap) - 1,
             data = databaseBM, 
             index = c("countryname", "Time"), 
             model = "random")
summary(regRE)

# First difference
regFD <- plm(corruption_index ~ women +
               Rule_of_law+employedwomen+ log(GDPpercap) - 1,
             data = databaseBM, 
             index = c("countryname", "Time"), 
             model = "fd")
summary(regFD)

#Pooled OLS
regpooling <- plm(corruption_index  ~ women+
                    Rule_of_law+employedwomen+ log(GDPpercap) - 1,
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


######### Regressão Eduardo 
teste <- lm(corruption_index~women + log(GDPpercap), data = databaseBM)
stargazer(teste,  type = "text")


#################### REGRESSÕES EM ESCADA ##########################

reg1 <- plm(corruption_index ~ women - 1,
             data = databaseBM, 
             index = c("countryname", "Time"), 
             model = "within")

reg2 <- plm(corruption_index ~ women + Rule_of_law -  1,
              data = databaseBM, 
              index = c("countryname", "Time"), 
              model = "within")

reg3 <- plm(corruption_index ~ women + Rule_of_law +  log(GDPpercap)   - 1,
              data = databaseBM, 
              index = c("countryname", "Time"), 
              model = "within")


reg4 <- plm(corruption_index ~ women + Rule_of_law + log(GDPpercap)  + employedwomen  - 1,
            data = databaseBM, 
            index = c("countryname", "Time"), 
            model = "within")


reg5 <- plm(corruption_index ~ women + Rule_of_law + log(GDPpercap)  + employedwomen  + as.factor(Colony) - 1,
            data = databaseBM, 
            index = c("countryname", "Time"), 
            model = "within")

stargazer(reg1, reg2, reg3, reg4, reg5,
          type = "text",
          align = TRUE,
          title = "Results Pooling OLS", 
          #column.labels=c("Fixed Effect","First Difference", "Random Effect", "Pooled OLS"), 
          covariate.labels=c("percentage of Woman in parliament", "Rule of Law", "GDP per capita", "Women Economic rights: percentage of employed women", "Colony"))



###### 2º hipótese
#Developed (>0.80 é very high human development, segundo UNDP)
datadev <- databaseBM %>% filter(HDI > 0.800) %>% distinct() 
unique(datadev$countryname)

regdeveloped <- plm(corruption_index ~ women +
                      Rule_of_law +  log(GDPpercap) + employedwomen + Colony - 1,
                    data = datadev, 
                    index = c("countryname", "Time"), 
                    model = "within")



#Undeveloped ( 0.550–0.699 for medium human development, segundo UNDP)
dataundev <- databaseBM %>% filter(HDI < 0.750 & HDI > 0.600)  %>% distinct ()
unique(dataundev$countryname)

regdeveloping <- plm(corruption_index ~ women +
                       Rule_of_law +  log(GDPpercap) + employedwomen+ + Colony - 1,
                     data = dataundev, 
                     index = c("countryname", "Time"), 
                     model = "within")


log.transform <- function(x,off=0.1){if(sign(min(x,na.rm=T))!=1){x <- x + abs(min(x,na.rm=T)) + off}
  
  x <- log(x)
  
  return(x)}

regdeveloped <- lm(corruption_index ~ women+
              
              Rule_of_law + employedwomen + log.transform(GDPpercap) + factor(Time)
            
            ,data=datadev)

regdeveloping <- lm(corruption_index ~ women+
                      
                      Rule_of_law + employedwomen + log.transform(GDPpercap) + factor(Time)
                    
                    ,data=dataundev)


stargazer(regdeveloped,regdeveloping,  
          type = "text",
          align = TRUE,
          title = "Results: FE", 
          column.labels=c("Very high HD","Medium HD"), 
          covariate.labels=c("percentage of Woman in parliament", "Rule of Law","Women Economic rights: percentage of employed women",  "GDP per capita"))






