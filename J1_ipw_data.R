#this creates data for IPW

library(survival)
library(survey)
library(ipw)
load(file="~/hrs/mortality/df.Rdata")

#load("~/hrs/mortality/all.Rdata")
#df[df$rabyear %in% years,]->df

#special stuff
ifelse(df$radyear<2006,1,0)->df$early.death
ifelse(is.na(df$early.death),0,df$early.death)->df$early.death
abs(1-df$early.death)->df$no.death
#

df[df$white==1,]->df
df[df$rabyear %in% 1919:1955,]->df
    
####################################################################################################################################
#weighting
wt_fun<-function(df) {
                                        #formula("~raedyrs + bmi + height + smoke + cesd + diab + heart + alz + gender+rabyear")->fm.selection
    formula("~raedyrs + bmi + height + smoke + cesd + diab + heart +  gender+rabyear")->fm.selection #no alz
    df[,all.vars(fm.selection)]->tmp
    df[rowSums(is.na(tmp))==0,]->tmp
    ipwpoint(no.death,data=tmp,family="binomial",link="logit",numerator=~1,denominator=~raedyrs + bmi + height + smoke + cesd + diab + heart + gender+ rabyear,trunc=0.01)->wt
    print(table(wt$weights==wt$weights.trunc))
    wt$weights.trunc->tmp$wt.byear
    tmp[,c("hhidpn","wt.byear")]->wt
    merge(df,tmp,all.x=TRUE)
}
wt_fun(df)->df

wt_fun<-function(df) {
    #formula("~raedyrs + bmi + height + smoke + cesd + diab + heart + alz + gender")->fm.selection
    formula("~raedyrs + bmi + height + smoke + cesd + diab + heart +  gender")->fm.selection #no alz
    df[,all.vars(fm.selection)]->tmp
    df[rowSums(is.na(tmp))==0,]->tmp
    ipwpoint(no.death,data=tmp,family="binomial",link="logit",numerator=~1,denominator=~raedyrs + bmi + height + smoke + cesd + diab + heart + gender,trunc=0.01)->wt
    wt$weights.trunc->tmp$wt.health
    print(table(wt$weights==wt$weights.trunc))
    tmp[,c("hhidpn","wt.health")]->wt
    merge(df,tmp,all.x=TRUE)
}
wt_fun(df)->df

#standardize phenotype
fun<-function(df,nm) {
    std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
    df[[nm]]->x
    std(x)->df[[paste(nm,".std",sep="")]]
    df
}
for (nm in c("bmi","height","raedyrs","smoke")) {
    split(df,df$ragender)->L
    lapply(L,fun,nm)->L
    data.frame(do.call("rbind",L))->df
}

df[!is.na(df$wt.byear),]->df

df$rabyear->hold
(df$rabyear-mean(df$rabyear,na.rm=TRUE))->df$rabyear
hold->df$rabyear.raw

NULL->rownames(df)
save(df,file="~/hrs/mortality/association.Rdata")



load(file="~/hrs/mortality/association.Rdata")
fun<-function(df,wt.nm,nm) {
    df[[wt.nm]]->df$wt
    plot(NULL,xlim=c(0.1,3.2),ylim=c(0,12),xlab="Weights",ylab="Density")
    mtext(side=3,line=1,nm)
    lines(density(df$wt[df$geno==0]),lty=1,col="black",lwd=3)
    lines(density(df$wt[df$geno==1]),lty=1,col="gray",lwd=3)
    legend("topright",bty="n",lwd=3,col=c("black","gray"),c("not genotyped","genotyped"))
}
par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(3.3,3.3,2.3,1))
split(df,df$ragender)->tmp
fun(tmp[["1.male"]],wt.nm="wt.health",nm=c("Males, health only"))
fun(tmp[["2.female"]],wt.nm="wt.health",nm=c("Females, health only"))
fun(tmp[["1.male"]],wt.nm="wt.byear",nm=c("Males, with birthyear"))
fun(tmp[["2.female"]],wt.nm="wt.byear",nm=c("Females, with birthyear"))



    
























