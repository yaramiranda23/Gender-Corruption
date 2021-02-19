## Baixando pacotes necessários 

library(tidyverse)
library(plm)
library(stargazer)

databaseBM <- read.csv("https://raw.githubusercontent.com/yaramiranda23/Gender-Corruption/main/databaseBM.csv")
databaseBM <- filter(databaseBM, democ > 50)
unique(databaseBM$countryname)

# A more efficient way to log transform variables
log.transform <- function(x,off=0.1){if(sign(min(x,na.rm=T))!=1){x <- x + abs(min(x,na.rm=T)) + off}
  x <- log(x)
  return(x)}


#################### REGRESSÕES 

#Fixed effect (Only year FEs in this model. Alternative way to do it without plm function)

#regFE <- lm(corruption_index ~ women+
              #Rule_of_law + employedwomen + log.transform(GDPpercap) + factor(Time)
            #,data=databaseBM)
#stargazer(regFE,   type = "text")

# EXPLANATION BY EM: The code below will run the exact same FE regression as above but using the plm command 
# instead of lm. The results are exactly the same. 
# See how I indexed by 'countryname' only and specified 'time' with the effect command so R 
# understands that you want this to be a time fixed effects regression. I suggest you check to see if you
# need to make corrections in all other regressions that you have using the plm command.

regFE <- plm(corruption_index ~ women+
                Rule_of_law+ log.transform(GDPpercap) + openness + democ + as.factor(Colony) - 1,
              data = databaseBM, 
              index = c("countryname"), 
              model = "within", effect = "time")



#Random effect
regRE <- plm(corruption_index ~ women+
               Rule_of_law + log.transform(GDPpercap) + openness + democ +  as.factor(Colony) - 1,
             data = databaseBM, 
             index = c("countryname"), 
             model = "random", effect = "time")


# First difference
regFD <- plm(corruption_index ~ women+
               Rule_of_law+ log.transform(GDPpercap)+ openness + democ + as.factor(Colony)- 1,
             data = databaseBM, 
             index = c("countryname"), 
             model = "fd")#, effect = "time")

#Pooled OLS
regpooling <- plm(corruption_index ~ women+
               Rule_of_law+ log.transform(GDPpercap) + openness + democ + as.factor(Colony) - 1,
             data = databaseBM, 
             index = c("countryname"), 
             model = "pooling", effect = "time")


### TABELA 1: 4 MÉTODOS DE REGRESSÃO PARA A 1º HIPÓTESE
stargazer(regFE,regFD, regRE, regpooling,   
          type = "text",
          align = TRUE,
          title = "Results", 
          column.labels=c("Fixed Effect", "First Difference", "Random Effect", "Pooled OLS"), 
          covariate.labels=c("percentage of Woman in parliament", "Rule of Law", "GDP per capita (log)", "Economic Openness", "Electoral democracy", "Colony"))


#################### REGRESSÕES EM ESCADA ##########################

reg1 <- plm(corruption_index ~ women - 1,
                  data = databaseBM, 
                  index = c("countryname"), 
                  model = "within", effect = "time")

reg2 <- plm(corruption_index ~ women +Rule_of_law - 1,
            data = databaseBM, 
            index = c("countryname"), 
            model = "within", effect = "time")

reg3 <- plm(corruption_index ~ women + Rule_of_law +  log(GDPpercap)   - 1,
            data = databaseBM, 
            index = c("countryname"), 
            model = "within", effect = "time")

reg4 <- plm(corruption_index ~ women + Rule_of_law + log(GDPpercap)  + employedwomen  - 1,
            data = databaseBM, 
            index = c("countryname"), 
            model = "within", effect = "time")
          

reg5 <- plm(corruption_index ~ women + Rule_of_law + log(GDPpercap)  + employedwomen  + openness - 1,
            data = databaseBM, 
            index = c("countryname"), 
            model = "within", effect = "time")

reg6 <- plm(corruption_index ~ women + Rule_of_law + log(GDPpercap)  + employedwomen  + openness + democ - 1,
            data = databaseBM, 
            index = c("countryname"), 
            model = "within", effect = "time")

reg7 <- plm(corruption_index ~ women + Rule_of_law + log(GDPpercap)  + employedwomen  + openness + democ + as.factor(Colony) - 1,
            data = databaseBM, 
            index = c("countryname"), 
            model = "within", effect = "time")

##### TABELA 2: REGRESSÃO DE FIXED EFFECT EM ESCADA (para apêndice)
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
datadev <- databaseBM %>% filter(HDI > 0.85) %>% distinct() 


regdeveloped <- plm(corruption_index ~ women+
               Rule_of_law+employedwomen+ log.transform(GDPpercap) +  openness + democ + Colony,
             data = datadev, 
             index = c("countryname"), 
             model = "within", effect = "time")


#Developing ( 0.550–0.699 for medium human development, segundo UNDP)
datadeveloping <- databaseBM %>% filter(HDI < 0.80 & HDI > 0.65)  %>% distinct ()

regdeveloping <- plm(corruption_index ~ women+
                      Rule_of_law+employedwomen+ log.transform(GDPpercap) +  openness + democ +  Colony,
                    data = datadeveloping, 
                    index = c("countryname"), 
                    model = "within", effect = "time")

#Autocratic
databaseBM <- read.csv("databaseBM.csv")
databaseBM_autoc <- filter(databaseBM, democ == 0)
unique(databaseBM_autoc$countryname)

regautoc <- plm(corruption_index ~ women+
                       Rule_of_law+employedwomen+ log.transform(GDPpercap) +  openness,
                     data = databaseBM_autoc, 
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


