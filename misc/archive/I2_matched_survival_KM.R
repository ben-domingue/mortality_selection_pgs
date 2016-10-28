## #split by race and gender
## load("~/hrs/mortality/all.Rdata")
## df[df$rabyear %in% 1910:1959,]->df
## ifelse(df$white==1,"wh","not")->df$white
## split(df,df$ragender)->tmp
## for (i in 1:length(tmp)) {
##     tmp[[i]]->z
##     (z$height-mean(z$height,na.rm=TRUE))/sd(z$height,na.rm=TRUE)->z$height
##     z->tmp[[i]]
## }
## data.frame(do.call("rbind",tmp))->df

## as.character(df$racohbyr)->df$racohbyr
## df[df$racohbyr!="0.not in any cohort",]->df
## df[df$racohbyr!="1.ahead",]->df
## df[df$racohbyr!="6.mid babyboomers",]->df


## split(df,list(df$ragender,df$white))->dfL
## library(survival)
## library(survey)
## "geno~(raedyrs+bmi+height+smoke+cesd+diab+heart+alz+srh)*rabyear"->fm.baseline

## ############################################################
## #First make the  picture focusing on matching for the genotyped

## mod.fun<-function(nm,fm,dfL) {
##     dfL[[nm]]->df
##     formula(fm)->fm
##     df[,all.vars(fm)]->tmp
##     df[rowSums(is.na(tmp))==0,]->df
##     glm(fm,df,family="binomial")->mod
##     fitted(mod)->df$fitted
##     quantile(df$fitted[df$geno==1],c(.7))->qu
##     df[df$fitted>qu,]->df.reduced
##     list(all=df,reduced=df.reduced)->L
##     plot(NULL,xlab="Age",ylab="",xlim=c(50,90),ylim=c(.5,1),main=nm)    
##     lwd<-c(1,3.5)
##     for (k in 1:length(L)) {
##         L[[k]]->df
##         #coxph(Surv(time=age,event=dead)~rabyear*geno+age.first.interview*geno,df,control=coxph.control(iter=5000))->mod
##         #survfit(Surv(time=age,event=dead)~rabyear*geno+age.first.interview*geno,df)->mod
##         survfit(Surv(time=age,event=dead)~rabyear+geno+age.first.interview,df)->mod
##         print(mod)
##         ##
##         byear<-c(1930,1945)
##         max(df$r11iwendy,na.rm=TRUE)->last.year
##         last.year-byear->max.age
##                                         #fi<-c(53,70)
##         fi<-60
##         col<-rgb(c(0,1,0),c(0,0,0),c(0,0,1),alpha=1-(k-1)/2) #c("black","red","blue")
##         for (i in 1:length(byear)) {
##             for (j in 1:length(fi)) {
##                 data.frame(geno=1,rabyear=byear[i],age.first.interview=fi[j])->z
##                 survfit(mod,newdata=z)->mod2
##                 mod2$time<=max.age[i]->index
##                 lines(mod2$time[index],mod2$surv[index],lwd=lwd[k],col=col[i])
##                 data.frame(geno=0,rabyear=byear[i],age.first.interview=fi[j])->z
##                 survfit(mod,newdata=z)->mod2
##                 mod2$time<=max.age[i]->index
##                 lines(mod2$time[index],mod2$surv[index],lty=2,lwd=lwd[k],col=col[i])
##             }
##         }
##     }
##     for (i in seq(50,110,by=10)) abline(v=i,lty=3,lwd=.4,col="gray")
##     for (i in seq(0,1,by=.1)) abline(h=i,lty=3,lwd=.4,col="gray")
## }
    
## png("/tmp/surv-matched-geno.png",units="in",height=10,width=8,res=150,pointsize=13)
## par(mfrow=c(2,2),mar=c(3,4,1.5,0.5),mgp=c(1.5,.75,0),oma=rep(.5,4))
## for (i in 1:length(dfL)) mod.fun(names(dfL)[i],dfL,fm=fm.baseline)
## dev.off()

## ############################################################
## #now make the  picture focusing on matching for the non-genotyped

## mod.fun<-function(nm,fm,dfL) {
##     dfL[[nm]]->df
##     formula(fm)->fm
##     df[,all.vars(fm)]->tmp
##     df[rowSums(is.na(tmp))==0,]->df
##     glm(fm,df,family="binomial")->mod
##     fitted(mod)->df$fitted
##     quantile(df$fitted[df$geno==0],.3)->qu
##     df[df$fitted<qu,]->df.reduced
##     list(all=df,reduced=df.reduced)->L
##     plot(NULL,xlab="Age",ylab="",xlim=c(50,90),ylim=c(.5,1),main=nm)    
##     lwd<-c(1,3.5)
##     for (k in 1:length(L)) {
##         L[[k]]->df
##         coxph(Surv(time=age,event=dead)~rabyear*geno+age.first.interview*geno,df,control=coxph.control(iter=5000))->mod
##         print(mod)
##         ##
##         byear<-c(1930,1945)
##         max(df$r11iwendy,na.rm=TRUE)->last.year
##         last.year-byear->max.age
##                                         #fi<-c(53,70)
##         fi<-60
##         col<-rgb(c(0,1,0),c(0,0,0),c(0,0,1),alpha=1-(k-1)/2) #c("black","red","blue")
##         for (i in 1:length(byear)) {
##             for (j in 1:length(fi)) {
##                 data.frame(geno=1,rabyear=byear[i],age.first.interview=fi[j])->z
##                 survfit(mod,newdata=z)->mod2
##                 mod2$time<=max.age[i]->index
##                 lines(mod2$time[index],mod2$surv[index],lwd=lwd[k],col=col[i])
##                 data.frame(geno=0,rabyear=byear[i],age.first.interview=fi[j])->z
##                 survfit(mod,newdata=z)->mod2
##                 mod2$time<=max.age[i]->index
##                 lines(mod2$time[index],mod2$surv[index],lty=2,lwd=lwd[k],col=col[i])
##             }
##         }
##     }
##     for (i in seq(50,110,by=10)) abline(v=i,lty=3,lwd=.4,col="gray")
##     for (i in seq(0,1,by=.1)) abline(h=i,lty=3,lwd=.4,col="gray")
## }
    
## png("/tmp/surv-matched-nogeno.png",units="in",height=10,width=8,res=150,pointsize=13)
## par(mfrow=c(2,2),mar=c(3,4,1.5,0.5),mgp=c(1.5,.75,0),oma=rep(.5,4))
## for (i in 1:length(dfL)) mod.fun(names(dfL)[i],dfL,fm=fm.baseline)
## dev.off()
