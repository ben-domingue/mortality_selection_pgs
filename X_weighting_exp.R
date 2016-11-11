library(survival)
library(survey)
library(ipw)
load(file="~/hrs/mortality/df.Rdata")


#special stuff
ifelse(df$radyear<2006,1,0)->df$early.death
ifelse(is.na(df$early.death),0,df$early.death)->df$early.death
abs(1-df$early.death)->df$no.death
#

df[df$white==1,]->df
df[df$rabyear %in% 1919:1955,]->df

####################################################################################################################

formula("~raedyrs + bmi + height + smoke + cesd + diab + heart +  gender+rabyear")->fm.selection #no alz
df[,all.vars(fm.selection),drop=FALSE]->tmp
df[rowSums(is.na(tmp))==0,]->tmp
ipwpoint(no.death,data=tmp,family="binomial",link="logit",numerator=~raedyrs + bmi + height + smoke + cesd + diab + heart +  gender,denominator=~rabyear,trunc=0.01)->wt.stab
data.frame(hhidpn=tmp$hhidpn,wt.stab=wt.stab$weights)->foo
merge(df,foo,all=TRUE)->df
ipwpoint(no.death,data=tmp,family="binomial",link="logit",numerator=~1,denominator=~raedyrs + bmi + height + smoke + cesd + diab + heart +  gender,trunc=0.01)->wt
data.frame(hhidpn=tmp$hhidpn,wt=wt$weights.trunc,wt.notrunc=wt$ipw.weights)->foo
merge(df,foo,all=TRUE)->df
glm(no.death~raedyrs + bmi + height + smoke + cesd + diab + heart +  gender,tmp,family="binomial")->m
data.frame(hhidpn=tmp$hhidpn,pr=m$fitted)->foo
merge(df,foo,all=TRUE)->df


par(mfrow=c(2,2))
plot(density(df$wt.stab[df$geno==0],na.rm=TRUE))
lines(density(df$wt.stab[df$geno==1],na.rm=TRUE),col="red")
plot(density(df$wt[df$geno==0],na.rm=TRUE))
lines(density(df$wt[df$geno==1],na.rm=TRUE),col="red")
plot(density(df$wt.notrunc[df$geno==0],na.rm=TRUE))
lines(density(df$wt.notrunc[df$geno==1],na.rm=TRUE),col="red")
plot(density(df$pr[df$geno==0],na.rm=TRUE))
lines(density(df$pr[df$geno==1],na.rm=TRUE),col="red")

df[!is.na(df$wt),]->df
df$rabyear-mean(df$rabyear,na.rm=TRUE)->df$rabyear.mc
f<-function(nm) {
    m<-list()
    fm<-paste(nm,"~rabyear.mc")
    lm(fm,df)->m$raw
    svyglm(fm,design = svydesign(~ 1, weights = ~ wt.stab,data=df))->m$stab
    svyglm(fm,design = svydesign(~ 1, weights = ~ wt,data=df))->m$wt
    svyglm(fm,design = svydesign(~ 1, weights = ~ wt.notrunc,data=df))->m$notrunc
    lapply(m,coef)
}
f("raedyrs")
f("vN_education2_nc.pgs")
