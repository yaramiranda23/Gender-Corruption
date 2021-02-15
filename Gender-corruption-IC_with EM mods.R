
## Baixando pacotes necessários 

library(tidyverse)
library(plm)
library(stargazer)
library(Zelig) #Added for testing panel regressions

databaseBM <- read.csv("databaseBM.csv")

# A more efficient way to log transform variables
log.transform <- function(x,off=0.1){if(sign(min(x,na.rm=T))!=1){x <- x + abs(min(x,na.rm=T)) + off}
  x <- log(x)
  return(x)}


#################### REGRESSÕES SEM COLÔNIA 

#Fixed effect (Only year FEs in this model. Alternative way to do it without plm function)

regFE <- lm(corruption_index ~ women+
                  Rule_of_law + employedwomen + log.transform(GDPpercap) + factor(Time)
                ,data=databaseBM)
stargazer(regFE,   
          type = "text")

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


##### teste
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





