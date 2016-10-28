library(ipw)
library(survey)

load(file="~/hrs/mortality/df.Rdata")
grep("^vN",names(df))->i1
#grep("^pc",names(df))->i2
df[,c("hhidpn","subjectID",names(df)[c(i1)])]->tmp

load("~/hrs/mortality/all.Rdata")
df[df$white==1,]->df
df[df$rabyear %in% 1910:1959,]->df
#special stuff
ifelse(df$radyear<2006,1,0)->df$early.death
ifelse(is.na(df$early.death),0,df$early.death)->df$early.death
abs(1-df$early.death)->df$no.death
#

merge(df,tmp,all.x=TRUE)->df
read.table("/hrsshare/cleaned-N/hrs_le_WH.eigenvec",header=FALSE)->eig
names(eig)<-c("family","subjectID",paste("pc",1:20,sep=""))
merge(df,eig,all.x=TRUE)->df
df$radyear-df$rabyear -> df$age.death
ifelse(!is.na(df$radyear),1,0)->df$dead


## "no.death~(raedyrs+bmi+height+smoke+cesd+diab+heart+alz+srh)*rabyear"->fm
## formula(fm)->fm
## df[,all.vars(fm)]->tmp
## df[rowSums(is.na(tmp))==0,]->df
## glm(fm,df.train,family="binomial")->mod
## df[!(1:nrow(df) %in% test.index),]->df.test
## predict(mod,df.test)->df.test$fitted
## df.test->df


formula("~(raedyrs+bmi+height+smoke+cesd+diab+heart+alz+srh)*rabyear")->fm.selection
df[,all.vars(fm.selection)]->tmp
df[rowSums(is.na(tmp))==0,]->df
ipwpoint(no.death,data=df,family="binomial",link="logit",numerator=~1,denominator=~(raedyrs+bmi+height+smoke+cesd+diab+heart+alz+srh)*rabyear,trunc=0.01)->wt
wt$weights.trunc->df$wt

mod<-list()
glm(dead~rabyear+factor(ragender),df,family="binomial")->mod$base
c("vN_alz_nc.profile","vN_bmi_nc.profile","vN_cardio_nc.profile","vN_diabetes_log_nc.profile","vN_eversmoke_nc.profile","vN_psych_log_nc.profile","vN_scz_log_nc.profile")->scores
for (nm in scores) {
    glm(paste("dead~rabyear+factor(ragender)+",nm),df,family="binomial")->mod[[nm]]
}
paste("dead~pc1+pc2+pc3+pc4+rabyear+factor(ragender)+",paste(scores,collapse="+"))->fm.full
glm(fm.full,df,family="binomial")->mod$full

mod<-list()
glm(dead~rabyear+factor(ragender),df,family="binomial")->mod$base
c("vN_alz_nc.profile","vN_bmi_nc.profile","vN_cardio_nc.profile","vN_diabetes_log_nc.profile","vN_eversmoke_nc.profile","vN_psych_log_nc.profile","vN_scz_log_nc.profile")->scores
for (nm in scores) {
    glm(paste("dead~rabyear+factor(ragender)+",nm),df,family="binomial")->mod[[nm]]
}
paste("dead~pc1+pc2+pc3+pc4+rabyear+factor(ragender)+",paste(scores,collapse="+"))->fm.full
glm(fm.full,df,family="binomial")->mod$full
lapply(mod,function(x) summary(x)$coef)
mod->mod.nowt

mod<-list()
svyglm(dead~rabyear+factor(ragender),design = svydesign(~ 1, weights = ~ wt,data=df))->mod$base
c("vN_alz_nc.profile","vN_bmi_nc.profile","vN_cardio_nc.profile","vN_diabetes_log_nc.profile","vN_eversmoke_nc.profile","vN_psych_log_nc.profile","vN_scz_log_nc.profile")->scores
for (nm in scores) {
    svyglm(paste("dead~rabyear+factor(ragender)+",nm),design = svydesign(~ 1, weights = ~ wt,data=df))->mod[[nm]]
}
paste("dead~pc1+pc2+pc3+pc4+rabyear+factor(ragender)+",paste(scores,collapse="+"))->fm.full
glm(fm.full,df,family="binomial")->mod$full
lapply(mod,function(x) summary(x)$coef)
mod->mod.wt






