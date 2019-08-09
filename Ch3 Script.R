install.packages("foreign")
library(foreign)


mexdata<-read.dta("F:/Fall 2017/Data/Total Merge/Files to Merge/lagmex.dta")



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


#### All of this is working weird. Lets make some tables instead. WITHOUT YEAR EFFECTS


fixed1<-plm(pri_share~lpop+homicide+gini1+analfa+desoc+presyear+strikepopdiv, data=mexdata, index=c("code","year"), model="within")
summary(fixed1)



fixed2<-plm(pri_share~lpop+homicide+gini1+analfa+desoc+presyear+threatpopdiv, data=mexdata, index=c("code","year"), model="within")
summary(fixed2)



fixed3<-plm(pri_share~lpop+homicide+gini1+analfa+desoc+presyear+maquila_employpop, data=mexdata, index=c("code","year"), model="within")
summary(fixed3)


fixed4<-plm(pri_share~lpop+homicide+gini1+analfa+desoc+presyear+strikepopdiv+threatpopdiv+maquila_employpop, data=mexdata, index=c("code","year"), model="within")
summary(fixed4)


##### NOW WITH YEARS



fixed5<-plm(pri_share~lpop+homicide+gini1+analfa+desoc+presyear+strikepopdiv, data=mexdata, index=c("code","year"), model="within", effect="twoways")
summary(fixed5)



fixed6<-plm(pri_share~lpop+homicide+gini1+analfa+desoc+presyear+threatpopdiv, data=mexdata, index=c("code","year"), model="within", effect="twoways")
summary(fixed6)



fixed7<-plm(pri_share~lpop+homicide+gini1+analfa+desoc+presyear+maquila_employpop, data=mexdata, index=c("code","year"), model="within", effect="twoways")
summary(fixed7)



fixed8<-plm(pri_share~lpop+homicide+gini1+analfa+desoc+presyear+strikepopdiv+threatpopdiv+maquila_employpop, data=mexdata, index=c("code","year"), model="within", effect="twoways")
summary(fixed8)



install.packages("stargazer")
library(stargazer)


# put in a table with SEs clustered by municipio
stargazer(fixed1, fixed2, fixed3, fixed4,
          title="Municipal Fixed Effects", 
          column.labels=c("Model 1", "Model 2", "Model 3", "Model 4"), 
          df=FALSE, digits=4)


stargazer(fixed5, fixed6, fixed7, fixed8,
          title="Municipal & Year Fixed Effects",
          column.labels=c("Model 5", "Model 6", "Model 7", "Model 8"), 
          df=FALSE, digits=4)



##################################################### Including Coalition Data




fixed9<-plm(pricoal~lpop+homicide+gini1+analfa+desoc+presyear+strikepopdiv, data=mexdata, index=c("code","year"), model="within")
summary(fixed9)



fixed10<-plm(pricoal~lpop+homicide+gini1+analfa+desoc+presyear+threatpopdiv, data=mexdata, index=c("code","year"), model="within")
summary(fixed10)



fixed11<-plm(pricoal~lpop+homicide+gini1+analfa+desoc+presyear+maquila_employpop, data=mexdata, index=c("code","year"), model="within")
summary(fixed11)


fixed12<-plm(pricoal~lpop+homicide+gini1+analfa+desoc+presyear+strikepopdiv+threatpopdiv+maquila_employpop, data=mexdata, index=c("code","year"), model="within")
summary(fixed12)




fixed13<-plm(pricoal~lpop+homicide+gini1+analfa+desoc+presyear+strikepopdiv, data=mexdata, index=c("code","year"), model="within", effect="twoways")
summary(fixed13)



fixed14<-plm(pricoal~lpop+homicide+gini1+analfa+desoc+presyear+threatpopdiv, data=mexdata, index=c("code","year"), model="within", effect="twoways")
summary(fixed14)



fixed15<-plm(pricoal~lpop+homicide+gini1+analfa+desoc+presyear+maquila_employpop, data=mexdata, index=c("code","year"), model="within", effect="twoways")
summary(fixed15)



fixed16<-plm(pricoal~lpop+homicide+gini1+analfa+desoc+presyear+strikepopdiv+threatpopdiv+maquila_employpop, data=mexdata, index=c("code","year"), model="within", effect="twoways")
summary(fixed16)


stargazer(fixed13, fixed14, fixed15, fixed16,
          title="Municipal & Year Fixed Effects",
          column.labels=c("Model 9", "Model 10", "Model 11", "Model 12"), 
          df=FALSE, digits=4)


##################################################### Arellano-Bond




##################################################### Error Correction Model


############ Unit Roots in Key Variables



Y<-cbind(mexdata$pri_share)
X<-cbind(mexdata$lpop, mexdata$homicide, mexdata$gini1, mexdata$analfa, mexdata$desoc, mexdata$presyear, mexdata$strikepopdiv, mexdata$threatpopdiv, mexdata$maquila_employpop)

d.model<-cbind(mexdata$code, mexdata$year, Y, X)

colnames(d.model)<-c("code", "year", "prishare", "population", "homicide", "gini", "analfa", "desoc", "presyear", "strikepopdiv", "threatpopdiv", "maquila_employpop")



pdata<-plm.data(mexdata, index=c("code", "year"))

purtest(pdata$pri_share, data=pdata, test=c("madwu", "Pm", "invnormal", "logit"), lags=1, pmax=10)

############ Cointegration


install.packages("urca")
library(urca)

install.packages("vars")
library(vars)



############ ECM






