#this looks at the survival curves based on only those who lived till 2006 [greatly reduced differences, used in SI]

load("~/hrs/mortality/all.Rdata")
df[df$rabyear %in% 1910:1959,]->df
df$year.last.interview-df$year.first.interview -> df$duration.time

df[df$radyear>=2008 | is.na(df$radyear),]->df2
mean(df$rabyear,na.rm=TRUE)
mean(df2$rabyear,na.rm=TRUE)
library(survival)


#comparing full sample to those who at least lived till 2006
coxph(Surv(time=age,event=dead)~rabyear*geno+age.first.interview*geno,df,control=coxph.control(iter=10000))->mod1
coxph(Surv(time=age,event=dead)~rabyear*geno+age.first.interview*geno,df2,control=coxph.control(iter=10000))->mod2



ifelse(df2$ragender=="1.male","Male","Female")->df2$ragender
ifelse(df2$white==1,"White","Non-white")->df2$white
split(df2,list(df2$ragender,df2$white))->df2L
library(survival)
library(survey)


fun<-function(nm,dfL) {
    print(nm)
    dfL[[nm]]->df
    coxph(Surv(time=duration.time,event=dead)~geno*age.first.interview+geno*rabyear,df,control=coxph.control(iter=5000))->mod
    print(mod)
    ##
    #plot(NULL,xlab="Age",ylab="",xlim=c(50,max(df$age,na.rm=TRUE)),ylim=c(.5,1),main=nm)
    plot(NULL,xlab="Age",ylab="",xlim=c(60,80),ylim=c(.5,1),main=nm)    
    byear<-c(1928,1938,1950)
    #fi<-c(55,65)
    fi<-c(60)
    #col<-c("black","red","blue")
    lwd<-c(2,4)
    for (i in 1:length(byear)) {
        for (j in 1:length(fi)) {
            col<-rgb(c(0,1,0),c(0,0,0),c(0,0,1),alpha=1-(j-1)/2) #
            data.frame(geno=1,rabyear=byear[i],age.first.interview=fi[j])->z
            survfit(mod,newdata=z)->mod2
            lines(fi[j]+mod2$time,mod2$surv,lwd=lwd[j],col=col[i])
            data.frame(geno=0,rabyear=byear[i],age.first.interview=fi[j])->z
            survfit(mod,newdata=z)->mod2
            lines(fi[j]+mod2$time,mod2$surv,lty=2,lwd=lwd[j],col=col[i])
        }
    }
    for (i in seq(50,110,by=10)) abline(v=i,lty=3,lwd=.4,col="gray")
    for (i in seq(0,1,by=.1)) abline(h=i,lty=3,lwd=.4,col="gray")
}
png("/tmp/surv4.png",units="in",height=10,width=8,res=150,pointsize=13)
par(mfrow=c(2,2),mar=c(3,4,1.5,0.5),mgp=c(1.5,.75,0),oma=rep(.5,4))
for (i in 1:length(df2L)) fun(names(df2L)[i],df2L)
dev.off()



#png("/tmp/km.png",units="in",height=4,width=8,res=150,pointsize=13)
#par(mfrow=c(2,2),mar=c(3,3,1.5,0.5),mgp=c(1.5,.6,0),oma=rep(.5,4))
#library(survey)
#svycoxph(Surv(time=age,event=dead)~geno+ragender,design = svydesign(~ 1, weights = ~ 1,data=df))->mod
#coxph(Surv(time=age,event=dead)~geno+ragender,df)->mod
#mod
#
## plot(NULL,xlab="Age",ylab="",xlim=c(0,max(df$age,na.rm=TRUE)),ylim=c(0,1),main="Males")
## data.frame(geno=1,ragender="1.male")->z
## survfit(mod,newdata=z)->mod2
## lines(mod2$time,mod2$surv)
## polygon(c(mod2$time,rev(mod2$time)),c(mod2$upper,rev(mod2$lower)),col=rgb(0.93,0.93,0.93,alpha=.55))
## #
## data.frame(geno=0,ragender="1.male")->z
## survfit(mod,newdata=z)->mod2
## lines(mod2$time,mod2$surv,lty=2)
## polygon(c(mod2$time,rev(mod2$time)),c(mod2$upper,rev(mod2$lower)),col=rgb(0.93,0.93,0.93,alpha=.55))
## legend("bottomleft",bty="n",c("Genotyped","Not genotyped"),lty=c(1,2))
## #
## plot(NULL,xlab="Age",ylab="",xlim=c(0,max(df$age,na.rm=TRUE)),ylim=c(0,1),main="Females")
## data.frame(geno=1,ragender="2.female")->z
## survfit(mod,newdata=z)->mod2
## lines(mod2$time,mod2$surv)
## polygon(c(mod2$time,rev(mod2$time)),c(mod2$upper,rev(mod2$lower)),col=rgb(0.93,0.93,0.93,alpha=.55))
## #
## data.frame(geno=0,ragender="2.female")->z
## survfit(mod,newdata=z)->mod2
## lines(mod2$time,mod2$surv,lty=2)
## polygon(c(mod2$time,rev(mod2$time)),c(mod2$upper,rev(mod2$lower)),col=rgb(0.93,0.93,0.93,alpha=.55))
## legend("bottomleft",bty="n",c("Genotyped","Not genotyped"),lty=c(1,2))
#
#coxph(Surv(time=age,event=dead)~geno+ragender,df2)->mod
#coxph(Surv(time=age,event=dead)~geno+ragender,df2)->mod
#mod
#
## plot(NULL,xlab="Age",ylab="",xlim=c(0,max(df$age,na.rm=TRUE)),ylim=c(0,1),main="Males")
## data.frame(geno=1,ragender="1.male")->z
## survfit(mod,newdata=z)->mod2
## lines(mod2$time,mod2$surv)
## polygon(c(mod2$time,rev(mod2$time)),c(mod2$upper,rev(mod2$lower)),col=rgb(0.93,0.93,0.93,alpha=.55))
## #
## data.frame(geno=0,ragender="1.male")->z
## survfit(mod,newdata=z)->mod2
## lines(mod2$time,mod2$surv,lty=2)
## polygon(c(mod2$time,rev(mod2$time)),c(mod2$upper,rev(mod2$lower)),col=rgb(0.93,0.93,0.93,alpha=.55))
## legend("bottomleft",bty="n",c("Genotyped","Not genotyped"),lty=c(1,2))
## #
## plot(NULL,xlab="Age",ylab="",xlim=c(0,max(df$age,na.rm=TRUE)),ylim=c(0,1),main="Females")
## data.frame(geno=1,ragender="2.female")->z
## survfit(mod,newdata=z)->mod2
## lines(mod2$time,mod2$surv)
## polygon(c(mod2$time,rev(mod2$time)),c(mod2$upper,rev(mod2$lower)),col=rgb(0.93,0.93,0.93,alpha=.55))
## #
## data.frame(geno=0,ragender="2.female")->z
## survfit(mod,newdata=z)->mod2
## lines(mod2$time,mod2$surv,lty=2)
## polygon(c(mod2$time,rev(mod2$time)),c(mod2$upper,rev(mod2$lower)),col=rgb(0.93,0.93,0.93,alpha=.55))
## legend("bottomleft",bty="n",c("Genotyped","Not genotyped"),lty=c(1,2))
## dev.off()
