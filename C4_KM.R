#this creates the KM figure separately for genotyped/non-genotyped in each group.

load("~/hrs/mortality/all.Rdata")


ifelse(df$ragender=="1.male","Male","Female")->df$ragender
ifelse(df$white==1,"White","Non-white")->df$white
split(df,list(df$ragender,df$white))->dfL

library(survival)
fun<-function(nm,dfL) {
    dfL[[nm]]->df
    df[df$geno==0,]->tmp
    survfit(Surv(time=age,event=dead)~1,tmp,conf.int=FALSE)->mod
    plot(NULL,ylim=c(0,1),main=nm,xlim=c(50,100),xlab="Age",ylab="")
    lines(mod$time,mod$surv,lty=1,lwd=3,col="black")
    c(mod$surv+1.96*mod$std.err,rev(mod$surv-1.96*mod$std.err))->y.ci
    ifelse(!is.finite(y.ci),2,y.ci)->y.ci
    polygon(c(mod$time,rev(mod$time)),y.ci,col=rgb(211,211,211,max=255,alpha=90))
    #
    df[df$geno==1,]->tmp
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

png("/tmp/fig2.png",units="in",height=6,width=8,res=150,pointsize=13)
par(mfrow=c(2,2),mar=c(3,3,1.5,0.5),mgp=c(1.5,.6,0),oma=rep(.5,4))
lapply(names(dfL),fun,dfL)
dev.off()


#appendix figures

library(survival)
fun_gender<-function(df,nm) {
    #gray(seq(.3,.7,length.out=4))->cols
    c("black","red","green","blue")->cols
    df[df$geno==0 & df$ragender=="Female",]->tmp
    nrow(tmp)->n1
    survfit(Surv(time=age,event=dead)~1,tmp,conf.int=FALSE)->mod
    plot(mod,col=cols[1],lty=1,lwd=2,main=nm,conf.int=FALSE,xlim=c(0,105))
    df[df$geno==1 & df$ragender=="Female",]->tmp
    nrow(tmp)->n2
    survfit(Surv(time=age,event=dead)~1,tmp,conf.int=FALSE)->mod
    lines(mod,col=cols[2],lty=1,lwd=2,conf.int=FALSE)
    df[df$geno==0 & df$ragender=="Male",]->tmp
    nrow(tmp)->n3
    survfit(Surv(time=age,event=dead)~1,tmp,conf.int=FALSE)->mod
    lines(mod,col=cols[3],lty=1,lwd=2,conf.int=FALSE)
    df[df$geno==1 & df$ragender=="Male",]->tmp
    nrow(tmp)->n4
    survfit(Surv(time=age,event=dead)~1,tmp,conf.int=FALSE)->mod
    lines(mod,col=cols[4],lty=1,lwd=2,conf.int=FALSE)
    #
    c("gen-no.female","gen-yes.female","gen-no.male","gen-yes.male")->txt
    legend("bottomleft",bty="n",paste(txt,", N=",c(n1,n2,n3,n4),sep=""),col=cols,lty=1,lwd=2)
}

png("/tmp/km.png",units="in",height=10,width=8,res=150,pointsize=13)
par(mar=c(3,3,1.5,0.5),mgp=c(1.5,.6,0),oma=rep(.5,4))
layout(matrix(c(1,1,2,3,4,5,6,7),4,2,byrow=TRUE))
fun_gender(df,nm="All")
as.character(df$racohbyr)->df$racohbyr
df[df$racohbyr!="0.not in any cohort",]->z
split(z,z$racohbyr)->tmp
for (i in 1:length(tmp)) fun_gender(tmp[[i]],nm=names(tmp)[i])
dev.off()

png("/tmp/km-slides.png",units="in",height=7,width=10,res=150,pointsize=13)
par(mfrow=c(2,3),mar=c(3,3,1.5,0.5),mgp=c(1.5,.6,0),oma=rep(.5,4))
as.character(df$racohbyr)->df$racohbyr
df[df$racohbyr!="0.not in any cohort",]->z
split(z,z$racohbyr)->tmp
for (i in 1:length(tmp)) fun_gender(tmp[[i]],nm=names(tmp)[i])
dev.off()



library(survival)
fun_race<-function(df,nm) {
    #gray(seq(.3,.7,length.out=4))->cols
    c("black","red","green","blue")->cols
    df[df$geno==0 & df$white=="Non-white",]->tmp
    nrow(tmp)->n1
    survfit(Surv(time=age,event=dead)~1,tmp,conf.int=FALSE)->mod
    plot(mod,col=cols[1],lty=1,lwd=2,main=nm,conf.int=FALSE,xlim=c(0,105))
    df[df$geno==1 & df$white=="Non-white",]->tmp
    nrow(tmp)->n2
    survfit(Surv(time=age,event=dead)~1,tmp,conf.int=FALSE)->mod
    lines(mod,col=cols[2],lty=1,lwd=2,conf.int=FALSE)
    df[df$geno==0 & df$white=="White",]->tmp
    nrow(tmp)->n3
    survfit(Surv(time=age,event=dead)~1,tmp,conf.int=FALSE)->mod
    lines(mod,col=cols[3],lty=1,lwd=2,conf.int=FALSE)
    df[df$geno==1 & df$white=="White",]->tmp
    nrow(tmp)->n4
    survfit(Surv(time=age,event=dead)~1,tmp,conf.int=FALSE)->mod
    lines(mod,col=cols[4],lty=1,lwd=2,conf.int=FALSE)
    #
    c("gen-no.NW","gen-yes.NW","gen-no.W","gen-yes.W")->txt
    legend("bottomleft",bty="n",paste(txt,", N=",c(n1,n2,n3,n4),sep=""),col=cols,lty=1,lwd=2)
}

png("/tmp/km-eth.png",units="in",height=10,width=8,res=150,pointsize=13)
par(mar=c(3,3,1.5,0.5),mgp=c(1.5,.6,0),oma=rep(.5,4))
layout(matrix(c(1,1,2,3,4,5,6,7),4,2,byrow=TRUE))
fun_race(df,nm="All")
as.character(df$racohbyr)->df$racohbyr
df[df$racohbyr!="0.not in any cohort",]->z
split(z,z$racohbyr)->tmp
for (i in 1:length(tmp)) fun_race(tmp[[i]],nm=names(tmp)[i])
dev.off()










## library(survival)
## fun<-function(df,nm) {
##     gray(seq(.3,.7,length.out=3))->cols
##     #df[df$geno==0 & df$ragender=="2.female",]->tmp
##     df[df$eth=="hispanic",]->tmp
##     nrow(tmp)->n1
##     survfit(Surv(time=age,event=dead)~1,tmp,conf.int=FALSE)->mod
##     plot(mod,col=cols[1],lty=1,lwd=2,main=nm,conf.int=FALSE,xlim=c(0,105))
##     #df[df$geno==1 & df$ragender=="2.female",]->tmp
##     df[df$eth=="nh white",]->tmp
##     nrow(tmp)->n2
##     survfit(Surv(time=age,event=dead)~1,tmp,conf.int=FALSE)->mod
##     lines(mod,col=cols[2],lty=1,lwd=2,conf.int=FALSE)
##     #df[df$geno==0 & df$ragender=="1.male",]->tmp
##     df[df$eth=="nh black",]->tmp
##     nrow(tmp)->n3
##     survfit(Surv(time=age,event=dead)~1,tmp,conf.int=FALSE)->mod
##     lines(mod,col=cols[3],lty=1,lwd=2,conf.int=FALSE)
##     #
##     c("hispanic","nh white","nh black")->txt
##     legend("bottomleft",bty="n",paste(txt,", N=",c(n1,n2,n3),sep=""),col=cols,lty=1,lwd=2)
## }
## ifelse(df$rahispan=="1. hispanic","hispanic",NA)->df$eth
## ifelse(df$raracem=="1.white/caucasian" & df$rahispan=="0. not hispanic","nh white",df$eth)->df$eth
## ifelse(df$raracem=="2.black/african american" & df$rahispan=="0. not hispanic","nh black",df$eth)->df$eth

## png("/tmp/km-eth.png",units="in",height=10,width=8,res=150,pointsize=13)
## par(mar=c(3,3,1.5,0.5),mgp=c(1.5,.6,0),oma=rep(.5,4))
## layout(matrix(c(1,1,2,3,4,5,6,7),4,2,byrow=TRUE))
## fun(df,nm="All")
## as.character(df$racohbyr)->df$racohbyr
## df[df$racohbyr!="0.not in any cohort",]->z
## split(z,z$racohbyr)->tmp
## for (i in 1:length(tmp)) fun(tmp[[i]],nm=names(tmp)[i])
## dev.off()
