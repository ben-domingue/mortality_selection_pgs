set.seed(11013)

                                        #this creates data for IPW

library(survival)
library(survey)
library(ipw)
load(file="~/hrs/mortality/df.Rdata")

#special stuff
ifelse(df$radyear<2006,1,0)->df$early.death
ifelse(is.na(df$early.death),0,df$early.death)->df$early.death
abs(1-df$early.death)->df$no.death
#
df[df$rabyear %in% 1919:1955,]->df

                                        #survival differences pre/post matching
                                        #load("~/hrs/mortality/all.Rdata")
                                        #df[df$rabyear %in% 1910:1959,]->df
df$year.last.interview-df$year.first.interview -> df$duration.time

ifelse(df$ragender=="1.male","Male","Female")->df$ragender
ifelse(df$white==1,"White","Non-white")->df$white
split(df,list(df$ragender,df$white))->dfL

fm.list<-list(fm.health="no.death~raedyrs+bmi+height+smoke+cesd+diab+heart+srh",fm.byear="no.death~raedyrs+bmi+height+smoke+cesd+diab+heart+srh+rabyear")





## ##################################################
## #Different looking plot, changes horizontally instead of in same panel
## mod.fun<-function(nm,fm,dfL,oos=FALSE) {
##     dfL[[nm]]->df
##     formula(fm)->fm
##     df[,all.vars(fm)]->tmp
##     df[rowSums(is.na(tmp))==0,]->df
##     #
##     if (oos) {
##         sample(1:nrow(df),round(nrow(df)*.6))->test.index
##         df[test.index,]->df.train
##         glm(fm,df.train,family="binomial")->mod
##         df[!(1:nrow(df) %in% test.index),]->df.test
##         predict(mod,df.test)->df.test$fitted
##                                         #
##         quantile(df.test$fitted[df.test$geno==1],c(.7))->qu
##         df.test[df.test$fitted>qu,]->df.reduced
##         list(all=df.test,reduced=df.reduced)->L
##     } else {
##         glm(fm,df,family="binomial")->mod
##         predict(mod,df)->df$fitted
##                                         #
##         quantile(df$fitted[df$geno==1],c(.7))->qu
##         df[df$fitted>qu,]->df.reduced
##         list(all=df,reduced=df.reduced)->L
##     }
##     #lwd<-c(1,3.5)
##     tabL<-list()
##     for (k in 1:length(L)) {
##         plot(NULL,xlab="Age",ylab="",xlim=c(60,80),ylim=c(.5,1),main=ifelse(k==1,nm,paste(nm,", Matched",sep="")))   
##         L[[k]]->df
##         coxph(Surv(time=duration.time,event=dead)~geno*age.first.interview+geno*rabyear,df,control=coxph.control(iter=15000))->mod
##         print(mod)
##         ##
##         byear<-c(1930,1945)
##         max(df$r11iwendy,na.rm=TRUE)->last.year
##         last.year-byear->max.age
##                                         #fi<-c(53,70)
##         fi<-60
##         col<-c("black","red")#col<-rgb(c(0,1,0),c(0,0,0),c(0,0,1),alpha=1-(k-1)/2) #c("black","red","blue")
##         tab<-list()
##         for (i in 1:length(byear)) {
##             for (j in 1:length(fi)) {
##                 data.frame(geno=1,rabyear=byear[i],age.first.interview=fi[j])->z
##                 survfit(mod,newdata=z)->mod2
##                 #mod2$time<=max.age[i]->index
##                 lines(fi[j]+mod2$time,mod2$surv,lwd=2,col=col[i])
##                 abline(v=fi[j]+14,lty=2)
##                 #get data for table
##                 which(mod2$time==14)->index
##                 mod2$surv[index]->g1
##                 data.frame(geno=0,rabyear=byear[i],age.first.interview=fi[j])->z
##                 survfit(mod,newdata=z)->mod2
##                 #mod2$time<=max.age[i]->index
##                 lines(fi[j]+mod2$time,mod2$surv,lty=2,lwd=2,col=col[i])
##                 which(mod2$time==14)->index
##                 mod2$surv[index]->g0
##                 c(byear[i],fi[j],mod2$time[index],g0,g1,g1-g0)->tab[[paste(nm,byear[i])]]
##             }
##         }
##         do.call("rbind",tab)->tabL[[k]]
##         for (i in seq(50,110,by=10)) abline(v=i,lty=3,lwd=.7,col="gray")
##         for (i in seq(0,1,by=.1)) abline(h=i,lty=3,lwd=.7,col="gray")
##     }
##     tabL
## }
    
## for (ii in 1:length(fm.list)) {
##     png(paste("/tmp/surv-matched-geno-",ii,".png",sep=""),units="in",height=10,width=8,res=150,pointsize=13)
##     par(mfrow=c(2,2),mar=c(3,4,1.5,0.5),mgp=c(1.5,.75,0),oma=rep(.5,4))
##     tab<-list()
##     for (i in 3:length(dfL)) mod.fun(names(dfL)[i],dfL,fm=fm.list[[ii]])->tab[[names(dfL)[i]]]
##     dev.off()
## }



##################################################
#table, many iterations
mod.fun<-function(nm,fm,dfL,oos=FALSE) {
    dfL[[nm]]->df
    formula(fm)->fm
    df[,all.vars(fm)]->tmp
    df[rowSums(is.na(tmp))==0,]->df
    #
    if (oos) {
        sample(1:nrow(df),round(nrow(df)*.5))->test.index
        df[test.index,]->df.train
        glm(fm,df.train,family="binomial")->mod
        df[!(1:nrow(df) %in% test.index),]->df.test
        predict(mod,df.test)->df.test$fitted
                                        #
        quantile(df.test$fitted[df.test$geno==1],c(.7))->qu
        df.test[df.test$fitted>qu,]->df.reduced
        list(all=df.test,reduced=df.reduced)->L
    } else {
        glm(fm,df,family="binomial")->mod
        print(summary(mod)$coef)
        predict(mod,df)->df$fitted
                                        #
        quantile(df$fitted[df$geno==1],c(.7))->qu
        df[df$fitted>qu,]->df.reduced
        list(all=df,reduced=df.reduced)->L
    }        
    #lwd<-c(1,3.5)
    tabL<-list()
    for (k in 1:length(L)) {
        L[[k]]->df
        coxph(Surv(time=duration.time,event=dead)~geno*age.first.interview+geno*rabyear,df,control=coxph.control(iter=15000))->mod
        ##
        byear<-c(1930,1945)
        max(df$r11iwendy,na.rm=TRUE)->last.year
        last.year-byear->max.age
                                        #fi<-c(53,70)
        fi<-60
        tab<-list()
        for (i in 1:length(byear)) {
            for (j in 1:length(fi)) {
                data.frame(geno=1,rabyear=byear[i],age.first.interview=fi[j])->z
                survfit(mod,newdata=z)->mod2
                #get data for table
                which(mod2$time==14)->index
                mod2$surv[index]->g1
                data.frame(geno=0,rabyear=byear[i],age.first.interview=fi[j])->z
                survfit(mod,newdata=z)->mod2
                which(mod2$time==14)->index
                mod2$surv[index]->g0
                c(byear[i],fi[j],mod2$time[index],g0,g1,g1-g0)->tab[[paste(nm,byear[i])]]
            }
        }
        do.call("rbind",tab)->tabL[[k]]
    }
    tabL
}


set.seed(11213)
out<-list()
for (ii in 1:length(fm.list)) {
    tab<-list()
    for (i in 3:length(dfL)) {
        tab.tmp<-list()
        mod.fun(names(dfL)[i],dfL,fm=fm.list[[ii]])->tab.tmp
        do.call("cbind",tab.tmp)->tab3
        tab3->tab[[names(dfL)[i]]]
    }
    do.call("rbind",tab)->tab
    tab->out[[ii]]
}

##                                         #w/ oos
## set.seed(11213)
## out<-list()
## for (ii in 1:length(fm.list)) {
##     tab<-list()
##     for (i in 3:length(dfL)) {
##         tab.tmp<-list()
##         for (j in 1:100) {
##             mod.fun(names(dfL)[i],dfL,fm=fm.list[[ii]],oos=TRUE)->tab.tmp[[j]]
##         }
##         lapply(tab.tmp,"[[",1)->tab1
##         lapply(tab.tmp,"[[",2)->tab2
##         cbind(Reduce("+",tab1)/length(tab1),Reduce("+",tab2)/length(tab2))->tab3
##         tab3->tab[[names(dfL)[i]]]
##     }
##     do.call("rbind",tab)->tab
##     tab->out[[ii]]
## }







############################################################
#First make the  picture focusing on matching for the genotyped

## mod.fun<-function(nm,fm,dfL) {
##     dfL[[nm]]->df
##     formula(fm)->fm
##     df[,all.vars(fm)]->tmp
##     df[rowSums(is.na(tmp))==0,]->df
##     #
##     sample(1:nrow(df),round(nrow(df)*.6))->test.index
##     df[test.index,]->df.train
##     glm(fm,df.train,family="binomial")->mod
##     df[!(1:nrow(df) %in% test.index),]->df.test
##     predict(mod,df.test)->df.test$fitted
##     #
##     quantile(df.test$fitted[df.test$geno==1],c(.7))->qu
##     df.test[df.test$fitted>qu,]->df.reduced
##     list(all=df.test,reduced=df.reduced)->L
##     plot(NULL,xlab="Age",ylab="",xlim=c(60,80),ylim=c(.5,1),main=nm)    
##     lwd<-c(1,3.5)
##     tabL<-list()
##     for (k in 1:length(L)) {
##         L[[k]]->df
##         coxph(Surv(time=duration.time,event=dead)~geno*age.first.interview+geno*rabyear,df,control=coxph.control(iter=15000))->mod
##         print(mod)
##         ##
##         byear<-c(1930,1945)
##         max(df$r11iwendy,na.rm=TRUE)->last.year
##         last.year-byear->max.age
##                                         #fi<-c(53,70)
##         fi<-60
##         col<-rgb(c(0,1,0),c(0,0,0),c(0,0,1),alpha=1-(k-1)/2) #c("black","red","blue")
##         tab<-list()
##         for (i in 1:length(byear)) {
##             for (j in 1:length(fi)) {
##                 data.frame(geno=1,rabyear=byear[i],age.first.interview=fi[j])->z
##                 survfit(mod,newdata=z)->mod2
##                 #mod2$time<=max.age[i]->index
##                 lines(fi[j]+mod2$time,mod2$surv,lwd=lwd[k],col=col[i])
##                 #get data for table
##                 which(mod2$time==14)->index
##                 mod2$surv[index]->g1
##                 data.frame(geno=0,rabyear=byear[i],age.first.interview=fi[j])->z
##                 survfit(mod,newdata=z)->mod2
##                 #mod2$time<=max.age[i]->index
##                 lines(fi[j]+mod2$time,mod2$surv,lty=2,lwd=lwd[k],col=col[i])
##                 which(mod2$time==14)->index
##                 mod2$surv[index]->g0
##                 c(byear[i],fi[j],mod2$time[index],g0,g1,g1-g0)->tab[[paste(nm,byear[i])]]
##             }
##         }
##         do.call("rbind",tab)->tabL[[k]]
##     }
##     for (i in seq(50,110,by=10)) abline(v=i,lty=3,lwd=.4,col="gray")
##     for (i in seq(0,1,by=.1)) abline(h=i,lty=3,lwd=.4,col="gray")
##     tabL
## }
    
## png("/tmp/surv-matched-geno.png",units="in",height=10,width=8,res=150,pointsize=13)
## par(mfrow=c(2,2),mar=c(3,4,1.5,0.5),mgp=c(1.5,.75,0),oma=rep(.5,4))
## tab<-list()
## for (i in 1:length(dfL)) mod.fun(names(dfL)[i],dfL,fm=fm.baseline)->tab[[names(dfL)[i]]]
## dev.off()
## do.call("rbind",lapply(tab,"[[",1))->tab1
## do.call("rbind",lapply(tab,"[[",2))->tab2
## cbind(tab1,tab2)->tab


############################################################
#now make the  picture focusing on matching for the non-genotyped

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
##     plot(NULL,xlab="Age",ylab="",xlim=c(60,80),ylim=c(.5,1),main=nm)    
##     lwd<-c(1,3.5)
##     tabL<-list()
##     for (k in 1:length(L)) {
##         L[[k]]->df
##         #coxph(Surv(time=age,event=dead)~rabyear*geno+age.first.interview*geno,df,control=coxph.control(iter=5000))->mod
##         coxph(Surv(time=duration.time,event=dead)~geno*age.first.interview+geno*rabyear,df,control=coxph.control(iter=5000))->mod
##         print(mod)
##         ##
##         byear<-c(1930,1945)
##         max(df$r11iwendy,na.rm=TRUE)->last.year
##         last.year-byear->max.age
##                                         #fi<-c(53,70)
##         fi<-60
##         col<-rgb(c(0,1,0),c(0,0,0),c(0,0,1),alpha=1-(k-1)/2) #c("black","red","blue")
##         tab<-list()
##         for (i in 1:length(byear)) {
##             for (j in 1:length(fi)) {
##                 data.frame(geno=1,rabyear=byear[i],age.first.interview=fi[j])->z
##                 survfit(mod,newdata=z)->mod2
##                 #mod2$time<=max.age[i]->index
##                 lines(fi[j]+mod2$time,mod2$surv,lwd=lwd[k],col=col[i])
##                 #get data for table
##                 which(mod2$time==14)->index
##                 mod2$surv[index]->g1
##                 data.frame(geno=0,rabyear=byear[i],age.first.interview=fi[j])->z
##                 survfit(mod,newdata=z)->mod2
##                 #mod2$time<=max.age[i]->index
##                 lines(fi[j]+mod2$time,mod2$surv,lty=2,lwd=lwd[k],col=col[i])
##                 which(mod2$time==14)->index
##                 mod2$surv[index]->g0
##                 c(byear[i],fi[j],mod2$time[index],g0,g1,g1-g0)->tab[[paste(nm,byear[i])]]
##             }
##         }
##         do.call("rbind",tab)->tabL[[k]]
##     }
##     for (i in seq(50,110,by=10)) abline(v=i,lty=3,lwd=.4,col="gray")
##     for (i in seq(0,1,by=.1)) abline(h=i,lty=3,lwd=.4,col="gray")
##     tabL
## }
    
## png("/tmp/surv-matched-notgeno.png",units="in",height=10,width=8,res=150,pointsize=13)
## par(mfrow=c(2,2),mar=c(3,4,1.5,0.5),mgp=c(1.5,.75,0),oma=rep(.5,4))
## tab<-list()
## for (i in 1:length(dfL)) mod.fun(names(dfL)[i],dfL,fm=fm.baseline)->tab[[names(dfL)[i]]]
## dev.off()
## do.call("rbind",lapply(tab,"[[",1))->tab1
## do.call("rbind",lapply(tab,"[[",2))->tab2
## cbind(tab1,tab2)->tab

