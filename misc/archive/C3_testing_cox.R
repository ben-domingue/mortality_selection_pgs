load("~/hrs/mortality/all.Rdata")
df[df$rabyear %in% 1910:1959,]->df

ifelse(df$ragender=="1.male","Male","Female")->df$ragender
ifelse(df$white==1,"White","Non-white")->df$white
split(df,list(df$ragender,df$white))->dfL
library(survival)
library(survey)
