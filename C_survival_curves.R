#cox survival curves separately by race/sex and genotyped/not



load("~/hrs/mortality/all.Rdata")
df[df$rabyear %in% 1910:1959,]->df

## #special stuff
## ifelse(df$radyear<2006,1,0)->df$early.death
## ifelse(is.na(df$early.death),0,df$early.death)->df$early.death
## abs(1-df$early.death)->df$no.death
## #

df$year.last.interview-df$year.first.interview -> df$duration.time

ifelse(df$ragender=="1.male","Male","Female")->df$ragender
ifelse(df$white==1,"White","Non-white")->df$white
split(df,list(df$ragender,df$white))->dfL
library(survival)
library(survey)



fun<-function(nm,dfL) {
    print(nm)
    dfL[[nm]]->df
    coxph(Surv(time=duration.time,event=dead)~geno*age.first.interview+geno*rabyear,df,control=coxph.control(iter=15000))->mod
    #coxph(Surv(time=duration.time,event=dead)~age.first.interview+geno*rabyear,df,control=coxph.control(iter=15000))->mod
    print(mod)
    ##
    #plot(NULL,xlab="Age",ylab="",xlim=c(50,max(df$age,na.rm=TRUE)),ylim=c(.5,1),main=nm)
    #byear<-c(1928,1938,1950)
    byear<-c(1930,1945)
    fi<-60
    #fi<-c(55,65)
    plot(NULL,xlab="Age",ylab="",xlim=c(min(fi),80),ylim=c(.5,1),main=nm)    
    #fi<-c(60)
    #col<-c("black","red","blue")
    lwd<-c(2,4)
    for (i in 1:length(byear)) {
        for (j in 1:length(fi)) {
            col<-rgb(c(0,1,0),c(0,0,0),c(0,0,1),alpha=1-(j-1)/2) #
            data.frame(geno=1,rabyear=byear[i],age.first.interview=fi[j])->z
            survfit(mod,newdata=z)->mod2
            #lines(fi[j]+mod2$time,mod2$surv,lwd=lwd[j],col=col[i])
            lines(fi[j]+mod2$time,mod2$surv,lwd=1,col=col[i])
            data.frame(geno=0,rabyear=byear[i],age.first.interview=fi[j])->z
            survfit(mod,newdata=z)->mod2
            #lines(fi[j]+mod2$time,mod2$surv,lty=2,lwd=lwd[j],col=col[i])
            lines(fi[j]+mod2$time,mod2$surv,lty=3,lwd=lwd[j],col=col[i])
        }
    }
    for (i in seq(50,110,by=10)) abline(v=i,lty=3,lwd=.7,col="gray")
    for (i in seq(0,1,by=.1)) abline(h=i,lty=3,lwd=.7,col="gray")
}

png("/tmp/surv4.png",units="in",height=10,width=8,res=150,pointsize=13)
par(mfrow=c(2,2),mar=c(3,4,1.5,0.5),mgp=c(1.5,.75,0),oma=rep(.5,4))
for (i in 1:length(dfL)) fun(names(dfL)[i],dfL)
dev.off()
