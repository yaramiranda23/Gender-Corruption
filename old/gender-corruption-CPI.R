## Baixando pacotes necessários 

library(tidyverse)
library(plm)
library(stargazer)

databaseBM <- read.csv("https://raw.githubusercontent.com/yaramiranda23/Gender-Corruption/main/databaseBM.csv")
CPI <- read.csv("https://raw.githubusercontent.com/yaramiranda23/Econometrics-Final-Project/master/CPI.csv") %>% select(!X)
databaseCPI <- inner_join(databaseBM, CPI)
databaseCPI <- filter(databaseCPI, democ > 50)


# A more efficient way to log transform variables
log.transform <- function(x,off=0.1){if(sign(min(x,na.rm=T))!=1){x <- x + abs(min(x,na.rm=T)) + off}
  x <- log(x)
  return(x)}


##### Regressões 

#Fixed effect
regFE <- plm(CPI_score ~ women+
               Rule_of_law+ log.transform(GDPpercap) + openness + democ + as.factor(Colony) - 1,
             data = databaseCPI, 
             index = c("countryname"), 
             model = "within", effect = "time")


#Random effect
regRE <- plm(CPI_score ~ women+
               Rule_of_law + log.transform(GDPpercap)  + openness + democ +  as.factor(Colony) - 1,
             data = databaseCPI, 
             index = c("countryname"), 
             model = "random", effect = "time")

# First difference
regFD <- plm(CPI_score ~ women+
               Rule_of_law+ log.transform(GDPpercap) + openness + democ + as.factor(Colony)- 1,
             data = databaseCPI, 
             index = c("countryname"), 
             model = "fd")#, effect = "time")

#Pooled OLS
regpooling <- plm(CPI_score~ women+
                    Rule_of_law+ log.transform(GDPpercap) + openness + democ + as.factor(Colony) - 1,
                  data = databaseCPI, 
                  index = c("countryname"), 
                  model = "pooling", effect = "time")

### TABELA 4: QUATRO MÉTODOS DE REGRESSÃO PARA A 1º HIPÓTESE (apêndice)
stargazer(regFE,regFD, regRE, regpooling,   
          type = "text",
          align = TRUE,
          title = "Results - CPI", 
          column.labels=c("Fixed Effect", "First Difference", "Random Effect", "Pooled OLS"), 
          covariate.labels=c("percentage of Woman in parliament", "Rule of Law", "GDP per capita (log)",  
                             "Economic Openness", "Electoral democracy", "Colony"))



#################### REGRESSÕES EM ESCADA ##########################

reg1 <- plm(CPI_score ~ women - 1,
            data = databaseCPI, 
            index = c("countryname"), 
            model = "within", effect = "time")

reg2 <- plm(CPI_score ~ women +Rule_of_law - 1,
            data = databaseCPI, 
            index = c("countryname"), 
            model = "within", effect = "time")

reg3 <- plm(CPI_score ~ women + Rule_of_law +  log(GDPpercap)   - 1,
            data = databaseCPI, 
            index = c("countryname"), 
            model = "within", effect = "time")

reg4 <- plm(CPI_score ~ women + Rule_of_law + log(GDPpercap)  + employedwomen  - 1,
            data = databaseCPI, 
            index = c("countryname"), 
            model = "within", effect = "time")


reg5 <- plm(CPI_score ~ women + Rule_of_law + log(GDPpercap)  + employedwomen  + openness - 1,
            data = databaseCPI, 
            index = c("countryname"), 
            model = "within", effect = "time")

reg6 <- plm(CPI_score ~ women + Rule_of_law + log(GDPpercap)  + employedwomen  + openness + democ - 1,
            data = databaseCPI, 
            index = c("countryname"), 
            model = "within", effect = "time")

reg7 <- plm(CPI_score ~ women + Rule_of_law + log(GDPpercap)  + employedwomen  + openness + democ + as.factor(Colony) - 1,
            data = databaseCPI, 
            index = c("countryname"), 
            model = "within", effect = "time")

##### TABELA 5: REGRESSÃO DE FIXED EFFECT EM ESCADA (para apêndice)
stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7,
          type = "text",
          align = TRUE,
          title = "FE", 
          #column.labels=c("Fixed Effect","First Difference", "Random Effect", "Pooled OLS"), 
          covariate.labels=c("percentage of Woman in parliament", "Rule of Law", "GDP per capita", 
                             "Women Economic rights: percentage of employed women", "Economic Openness", "Electoral democracy","Colony"))


########################################
############# 2º HIPÓTESE ##############
########################################

#Developed (>0.80 é very high human development, segundo UNDP)
datadev <- databaseCPI %>% filter(HDI > 0.85) %>% distinct() 


regdeveloped <- plm(CPI_score ~ women+
                      Rule_of_law+employedwomen+ log.transform(GDPpercap) +  openness + democ + Colony,
                    data = datadev, 
                    index = c("countryname"), 
                    model = "within", effect = "time")


#Developing ( 0.550–0.699 for medium human development, segundo UNDP)
datadeveloping <- databaseCPI %>% filter(HDI < 0.80 & HDI > 0.65)  %>% distinct ()

regdeveloping <- plm(CPI_score ~ women+
                       Rule_of_law+employedwomen+ log.transform(GDPpercap) +  openness + democ +  Colony,
                     data = datadeveloping, 
                     index = c("countryname"), 
                     model = "within", effect = "time")

#Autocratic
databaseCPI <- inner_join(databaseBM, CPI)
databaseCPI_autoc <- filter(databaseCPI, democ == 0)
unique(databaseCPI_autoc$countryname)

regautoc <- plm(CPI_score ~ women+
                  Rule_of_law+employedwomen+ log.transform(GDPpercap) +  openness,
                data = databaseCPI_autoc, 
                index = c("countryname"), 
                model = "within", effect = "time")


####### TABELA 3: TESTE DA 2º HIPÓTESE. BASE DIVIDIDA POR DESENVOLVIMENTO 
stargazer(regdeveloped,regdeveloping, regautoc,
          type = "text",
          align = TRUE,
          title = "Results: FE", 
          column.labels=c("HDI > 0.85","HDI 0.65 - 0.80", "Autocracies"), 
          covariate.labels=c("percentage of Woman in parliament", "Rule of Law",
                             "Women Economic rights: percentage of employed women", "GDP per capita (log)", "Economic Openness", "Electoral democracy", "Colony"))



