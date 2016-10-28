 load("~/hrs/mortality/all.Rdata")
df[df$rabyear %in% 1910:1959,]->df

df$year.last.interview-df$year.first.interview -> df$duration.time

ifelse(df$ragender=="1.male","Male","Female")->df$ragender
ifelse(df$white==1,"White","Non-white")->df$white
split(df,list(df$ragender,df$white))->dfL
library(survival)


fun<-function(nm,dfL) {
    dfL[[nm]]->df
    df[df$geno==0,]->tmp
    survfit(Surv(time=age,event=dead)~1,tmp,conf.int=FALSE)->mod
    plot(mod$time,-log(-log(mod$surv)),type="l",main=nm,xlab="Age",ylab="-log(-log(Hazard))")
        #
    df[df$geno==1,]->tmp
    survfit(Surv(time=age,event=dead)~1,tmp,conf.int=FALSE)->mod
    lines(mod$time,-log(-log(mod$surv)),type="l")
}

png("/tmp/par.png",units="in",height=10,width=8,res=150,pointsize=13)
par(mfrow=c(2,2),mar=c(3,3,1.5,0.5),mgp=c(1.5,.6,0),oma=rep(.5,4))
lapply(names(dfL),fun,dfL)
dev.off()


#sho residuals
fun<-function(nm,dfL) {
    dfL[[nm]]->df
    coxph(Surv(time=duration.time,event=dead)~geno*age.first.interview+geno*rabyear,df,control=coxph.control(iter=5000))->mod
    zph.mod<- cox.zph(mod, transform= "rank")
}
lapply(names(dfL),fun,dfL)->sho
names(dfL)->names(sho)

#added 1-20-2016
png("/tmp/sho.png",units="in",height=10,width=8,res=150,pointsize=13)
par(mfrow=c(2,2))
fun<-function(zph.mod,nm) {
    plot(zph.mod[1],main=nm)
    abline(h=0, lty=3)
}
for (i in 1:length(sho)) fun(sho[[i]],nm=names(sho)[i]) 
dev.off()

#correlation of sho residuals
fun<-function(nm,dfL) {
    dfL[[nm]]->df
    coxph(Surv(time=duration.time,event=dead)~geno*age.first.interview+geno*rabyear,df,control=coxph.control(iter=5000))->mod
    zph.mod<- cox.zph(mod, transform= "rank")
    rank(df$radyear[df$dead==1])->ran #does this seem right?
    cor(cbind(ran,zph.mod$y))[,1]
}
lapply(names(dfL),fun,dfL)->sho
names(dfL)->names(sho)
do.call("rbind",sho)->tab




## #logrank
## fun<-function(nm,dfL) {
##     print(nm)
##     dfL[[nm]]->df
##     #coxph(Surv(time=duration.time,event=dead)~geno*age.first.interview+geno*rabyear,df,control=coxph.control(iter=5000))->mod
##     survdiff(Surv(time=age,event=dead)~geno,df)->mod
##     print(mod$chisq)
## }
## lapply(names(dfL),fun,dfL=dfL)

## #wald test
## fun<-function(nm,dfL) {
##     print(nm)
##     dfL[[nm]]->df
##     coxph(Surv(time=duration.time,event=dead)~geno*age.first.interview+geno*rabyear,df,control=coxph.control(iter=5000))->mod
##     summary(mod)$waldtest
## }
## lapply(names(dfL),fun,dfL=dfL)



## coxph(Surv(time=duration.time,event=dead)~ragender+white+geno*age.first.interview+geno*rabyear,df,control=coxph.control(iter=5000))->mod
## zph.mod<- cox.zph(mod, transform= "rank")
## all.vars(mod$formula)->vars
## df[,vars]->tmp
## df[rowSums(is.na(tmp))==0,]->tmp
## by(zph.mod$y,tmp$ragender[tmp$dead==1],colMeans)
## by(zph.mod$y,tmp$white[tmp$dead==1],colMeans)








