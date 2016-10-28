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
fun<-function(df,wt.nm) {
    df[[wt.nm]]->df$wt
    plot(NULL,xlim=c(0.1,3.2),ylim=c(0,12),xlab="Weights",ylab="Density",main=wt.nm)
    lines(density(df$wt[df$geno==0 & df$ragender=="2.female"]),lty=1,col="black",lwd=2)
    lines(density(df$wt[df$geno==1 & df$ragender=="2.female"]),lty=2,col="black",lwd=2)
    lines(density(df$wt[df$geno==0 & df$ragender=="1.male"]),lty=1,col="gray",lwd=2)
    lines(density(df$wt[df$geno==1 & df$ragender=="1.male"]),lty=2,col="gray",lwd=2)
    legend("topright",bty="n",lwd=2,lty=c(1,2,1,2),col=c("black","black","gray","gray"),c("Female, not genotyped","Female, genotyped","Male, not genotyped","Male, genotyped"))
}
par(mfrow=c(2,1))
fun(df,wt.nm="wt.byear")
fun(df,wt.nm="wt.health")



    
























