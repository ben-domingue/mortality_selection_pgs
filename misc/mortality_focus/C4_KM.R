load("~/hrs/mortality/all.Rdata")

#special stuff
ifelse(df$radyear<2006,1,0)->df$early.death
ifelse(is.na(df$early.death),0,df$early.death)->df$early.death
abs(1-df$early.death)->df$no.death
#

ifelse(df$ragender=="1.male","Male","Female")->df$ragender
ifelse(df$white==1,"White","Non-white")->df$white
split(df,list(df$ragender,df$white))->dfL

library(survival)
fun<-function(nm,dfL) {
    dfL[[nm]]->df
    df[df$no.death==0,]->tmp
    survfit(Surv(time=age,event=dead)~1,tmp,conf.int=FALSE)->mod
    plot(NULL,ylim=c(0,1),main=nm,xlim=c(50,100),xlab="Age",ylab="")
    lines(mod$time,mod$surv,lty=1,lwd=3,col="black")
    c(mod$surv+1.96*mod$std.err,rev(mod$surv-1.96*mod$std.err))->y.ci
    ifelse(!is.finite(y.ci),2,y.ci)->y.ci
    polygon(c(mod$time,rev(mod$time)),y.ci,col=rgb(211,211,211,max=255,alpha=90))
    #
    df[df$no.death==1,]->tmp
    survfit(Surv(time=age,event=dead)~1,tmp,conf.int=FALSE)->mod
    #lines(mod,col="darkgray",lty=1,lwd=2,conf.int=FALSE)
    lines(mod$time,mod$surv,lty=1,lwd=3,col="gray")
    c(mod$surv+1.96*mod$std.err,rev(mod$surv-1.96*mod$std.err))->y.ci
    ifelse(!is.finite(y.ci),2,y.ci)->y.ci
    polygon(c(mod$time,rev(mod$time)),y.ci,col=rgb(211,211,211,max=255,alpha=90))
    #c("gen-no.female","gen-yes.female","gen-no.male","gen-yes.male")->txt
    #legend("bottomleft",bty="n",paste(txt,", N=",c(n1,n2,n3,n4),sep=""),col=cols,lty=1,lwd=2)
    legend("bottomleft",bty="n",c("Genotyped","Not Genotyped"),lty=1,lwd=3,col=c("darkgray","black"))
}

png("/tmp/km.png",units="in",height=6,width=8,res=150,pointsize=13)
par(mfrow=c(2,2),mar=c(3,3,1.5,0.5),mgp=c(1.5,.6,0),oma=rep(.5,4))
lapply(names(dfL),fun,dfL)
dev.off()

