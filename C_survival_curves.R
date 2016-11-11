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

df$rabyear-mean(df$rabyear)->df$rabyear
df$age.first.interview-mean(df$age.first.interview)->df$age.first.interview

                                        #diagnostics
## #mod is a list of lm objects
## table.lm<-function(mod) {
##   lapply(mod,function(x) names(coef(x)))->nms
##   unique(do.call("c",nms))->nms
##   length(nms)->nr
##   length(mod)->nc
##   mat.est<-mat.tstat<-matrix(NA,nr,nc)
##   for (j in 1:nc) {
##     summary(mod[[j]])$coef->foo
##     for (i in 1:nr) {
##       match(nms[i],rownames(foo))->index
##       if (length(index)>0) {
##         foo[index,1]->mat.est[i,j]
##         foo[index,5]->mat.tstat[i,j]
##       }
##     }
##   }
##   sapply(mod,function(x) length(residuals(x)))->N
##   out<-list()
##   new.nms<-list()
##   for (i in 1:nr) {
##     rbind(mat.est[i,],mat.tstat[i,])->out[[i]]
##     new.nms[[i]]<-c(nms[i],paste(nms[i],".pv",sep=""))
##   }
##   do.call("rbind",out)->out
##   rbind(out,N)->out
##   c(do.call("c",new.nms),"N")->rownames(out)
##   out
## }

##                                         #model evaluation
## fun<-function(df) {
##     m<-list()
##     coxph(Surv(time=duration.time,event=dead)~geno+age.first.interview+geno+rabyear,df,control=coxph.control(iter=15000))->m[[1]]
##     coxph(Surv(time=duration.time,event=dead)~geno*age.first.interview+geno+rabyear,df,control=coxph.control(iter=15000))->m[[2]]
##     coxph(Surv(time=duration.time,event=dead)~geno+age.first.interview+geno*rabyear,df,control=coxph.control(iter=15000))->m[[3]]
##     coxph(Surv(time=duration.time,event=dead)~geno*age.first.interview+geno*rabyear,df,control=coxph.control(iter=15000))->m[[4]]
##     coxph(Surv(time=duration.time,event=dead)~geno+rabyear+geno+year.first.interview,df,control=coxph.control(iter=15000))->m[[5]]
##     coxph(Surv(time=duration.time,event=dead)~geno*rabyear+geno+year.first.interview,df,control=coxph.control(iter=15000))->m[[6]]
##     coxph(Surv(time=duration.time,event=dead)~geno+rabyear+geno*year.first.interview,df,control=coxph.control(iter=15000))->m[[7]]
##     coxph(Surv(time=duration.time,event=dead)~geno*rabyear+geno*year.first.interview,df,control=coxph.control(iter=15000))->m[[8]]
##     m
## }
## #for (i in 1:length(dfL)) fun(names(dfL)[i],dfL)
## fun(dfL[[3]])->m
## table.lm(m)->tab



## fun<-function(df,fm) {
##     #coxph(Surv(time=duration.time,event=dead)~geno+age.first.interview+geno+rabyear,df,control=coxph.control(iter=15000))->mod
##     #coxph(Surv(time=duration.time,event=dead)~geno+age.first.interview+geno+rabyear+geno:age.first.interview,df,control=coxph.control(iter=15000))->mod
##     coxph(formula(paste("Surv(time=duration.time,event=dead)~",fm)),df,control=coxph.control(iter=15000))->mod
##     print(mod)
##     ##
##     #plot(NULL,xlab="Age",ylab="",xlim=c(50,max(df$age,na.rm=TRUE)),ylim=c(.5,1),main=nm)
##     #byear<-c(1928,1938,1950)
##     byear<-c(1930,1945)
##     fi<-60
##     plot(NULL,xlab="Age",ylab="",xlim=c(min(fi),80),ylim=c(.5,1))    
##     #col<-c("black","red","blue")
##     lwd<-c(2,4)
##     for (i in 1:length(byear)) {
##         for (j in 1:length(fi)) {
##             col<-rgb(c(0,1,0),c(0,0,0),c(0,0,1),alpha=1-(j-1)/2) #
##             data.frame(geno=1,rabyear=byear[i],age.first.interview=fi[j])->z
##             survfit(mod,newdata=z)->mod2
##             #lines(fi[j]+mod2$time,mod2$surv,lwd=lwd[j],col=col[i])
##             lines(fi[j]+mod2$time,mod2$surv,lwd=1,col=col[i])
##             data.frame(geno=0,rabyear=byear[i],age.first.interview=fi[j])->z
##             survfit(mod,newdata=z)->mod2
##             #lines(fi[j]+mod2$time,mod2$surv,lty=2,lwd=lwd[j],col=col[i])
##             lines(fi[j]+mod2$time,mod2$surv,lty=3,lwd=lwd[j],col=col[i])
##         }
##     }
##     #for (i in seq(50,110,by=10)) abline(v=i,lty=3,lwd=.7,col="gray")
##     #for (i in seq(0,1,by=.1)) abline(h=i,lty=3,lwd=.7,col="gray")
## }
## par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1,1))
## fun(dfL[[3]],fm="geno+age.first.interview+rabyear")
## fun(dfL[[3]],"geno+age.first.interview+rabyear+geno:age.first.interview")
## fun(dfL[[3]],"geno*rabyear+geno*age.first.interview")

#wait on below until we decide upon a model

fun<-function(nm,dfL) {
    print(nm)
    dfL[[nm]]->df
    #coxph(Surv(time=duration.time,event=dead)~geno+age.first.interview+geno+rabyear,df,control=coxph.control(iter=15000))->mod
    #coxph(Surv(time=duration.time,event=dead)~geno+age.first.interview+geno+rabyear+geno:age.first.interview,df,control=coxph.control(iter=15000))->mod
    coxph(Surv(time=duration.time,event=dead)~age.first.interview*geno+rabyear,df,control=coxph.control(iter=15000))->mod
    print(mod)
    ##
    #plot(NULL,xlab="Age",ylab="",xlim=c(50,max(df$age,na.rm=TRUE)),ylim=c(.5,1),main=nm)
    #byear<-c(1928,1938,1950)
    byear<-c(1930,1945)
    fi<-60
    plot(NULL,xlab="Age",ylab="",xlim=c(min(fi),80),ylim=c(.5,1),main=nm)    
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
    #for (i in seq(50,110,by=10)) abline(v=i,lty=3,lwd=.7,col="gray")
    #for (i in seq(0,1,by=.1)) abline(h=i,lty=3,lwd=.7,col="gray")
}

png("/tmp/surv4.png",units="in",height=10,width=8,res=150,pointsize=13)
par(mfrow=c(2,2),mar=c(3,4,1.5,0.5),mgp=c(1.5,.75,0),oma=rep(.5,4))
for (i in 1:length(dfL)) fun(names(dfL)[i],dfL)
dev.off()

      


#############################################################################
#differing age at first interview
fun<-function(nm,dfL) {
    print(nm)
    dfL[[nm]]->df
    coxph(Surv(time=duration.time,event=dead)~geno*age.first.interview+rabyear,df,control=coxph.control(iter=15000))->mod
    #coxph(Surv(time=duration.time,event=dead)~age.first.interview*geno+geno*rabyear,df,control=coxph.control(iter=15000))->mod
    print(mod)
    ##
    #plot(NULL,xlab="Age",ylab="",xlim=c(50,max(df$age,na.rm=TRUE)),ylim=c(.5,1),main=nm)
    byear<-c(1928,1938,1950)
    fi<-c(55,65)
    plot(NULL,xlab="Age",ylab="",xlim=c(min(fi),80),ylim=c(.5,1),main=nm)    
    col<-c("black","red","blue")
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
