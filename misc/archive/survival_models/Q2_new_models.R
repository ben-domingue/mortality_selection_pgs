load(file="scratch_data.Rdata")
library(survival)
library(survey)

coxph(Surv(time=duration.time,event=dead)~geno*age.first.interview,df,control=coxph.control(iter=5000))->mod
print(mod)
plot(NULL,xlab="Age",ylab="",xlim=c(55,85),ylim=c(.5,1))    
byear<-c(1928,1938,1950)
fi<-c(55,65)
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


coxph(Surv(time=duration.time,event=dead)~geno*age.first.interview+geno*rabyear,df,control=coxph.control(iter=5000))->mod
print(mod)
byear<-c(1928,1938,1950)
fi<-c(55,65)
lwd<-c(2,4)
par(mfrow=c(1,3))
for (i in 1:length(byear)) {
    plot(NULL,xlab="Age",ylab="",xlim=c(55,85),ylim=c(.5,1),main=byear[i])    
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


