install.packages("foreign")
library(foreign)


mexdata<-read.dta("D:/Fall 2017/Data/Total Merge/Files to Merge/lagmex.dta")



install.packages("pco")
library(pco)

install.packages("plm")
library(plm)

install.packages("punitroots")
library(punitroots)



install.packages("fUnitRoots")
require(fUnitRoots)


install.packages("tseries")
library(tseries)



########## Chapter 4 Draft 1 Table 1


fixed1<-plm(hogpop~strikepopdiv+threatpopdiv+maquila_employpop+strikepopdiv_diff+threatpopdiv_diff, data=mexdata, index=c("code","year"), model="within", effect="twoways")
summary(fixed1)



fixed2<-plm(hogpop~strikepopdiv+threatpopdiv+maquila_employpop+lpop+homicide+gini1+analfa+desoc+presyear, data=mexdata, index=c("code","year"), model="within", effect="twoways")
summary(fixed2)



fixed3<-plm(hogpop~strikepopdiv+threatpopdiv+maquila_employpop+lagpri+lpop+homicide+gini1+analfa+desoc+presyear, data=mexdata, index=c("code","year"), model="within", effect="twoways")
summary(fixed3)



fixed4<-plm(hogpop~strikepopdiv+threatpopdiv+maquila_employpop+lagpri+strikepopdiv_diff+threatpopdiv_diff+lpop+homicide+gini1+analfa+desoc+presyear, data=mexdata, index=c("code","year"), model="within", effect="twoways")
summary(fixed4)


install.packages("stargazer")
library(stargazer)


# put in a table with SEs clustered by municipio
stargazer(fixed1, fixed2, fixed3, fixed4,
          title="Municipal & Year Fixed Effects", 
          column.labels=c("Model 1", "Model 2", "Model 3", "Model 4"), 
          df=FALSE, digits=2)


###################################### Budget PC


fixed5<-plm(budgetpc~strikepopdiv+threatpopdiv+maquila_employpop+strikepopdiv_diff+threatpopdiv_diff, data=mexdata, index=c("code","year"), model="within", effect="twoways")
summary(fixed5)



fixed6<-plm(budgetpc~strikepopdiv+threatpopdiv+maquila_employpop+lpop+homicide+gini1+analfa+desoc+presyear, data=mexdata, index=c("code","year"), model="within", effect="twoways")
summary(fixed6)



fixed7<-plm(budgetpc~strikepopdiv+threatpopdiv+maquila_employpop+lagpri+lpop+homicide+gini1+analfa+desoc+presyear, data=mexdata, index=c("code","year"), model="within", effect="twoways")
summary(fixed7)



fixed8<-plm(budgetpc~strikepopdiv+threatpopdiv+maquila_employpop+lagpri+strikepopdiv_diff+threatpopdiv_diff+lpop+homicide+gini1+analfa+desoc+presyear, data=mexdata, index=c("code","year"), model="within", effect="twoways")
summary(fixed8)

# put in a table with SEs clustered by municipio
stargazer(fixed5, fixed6, fixed7, fixed8,
          title="Municipal & Year Fixed Effects", 
          column.labels=c("Model 5", "Model 6", "Model 7", "Model 8"), 
          df=FALSE, digits=2)
