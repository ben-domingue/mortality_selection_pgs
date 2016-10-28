## load("~/hrs/mortality/all.Rdata")
## df[df$rabyear %in% 1910:1960,]->df
## #df[df$radyear<2006,]->df

## formula("geno~(raedyrs+bmi+height+smoke+cesd+diab+heart+alz+gender)*rabyear")->fm
## df[,all.vars(fm)]->tmp
## df[rowSums(is.na(tmp))==0,]->df
## library(ipw)
## ipwpoint(geno,data=df,family="binomial",link="logit",numerator=~1,denominator=~(raedyrs+bmi+height+smoke+cesd+diab+heart+alz+gender)*rabyear,trunc=0.01)->wt
## wt$weights.trunc->df$wt
## wt$den.mod$fitted->z
## df[z<quantile(z,.1),]->df.lo
## df[z>quantile(z,.45) & z<quantile(z,.55),]->df.mid
## df[z>quantile(z,.9),]->df.hi

## library(survey)
## #svycoxph(Surv(time=age,event=dead)~geno+ragender,design = svydesign(~ 1, weights = ~ 1,data=df))->mod.nowt
## svycoxph(Surv(time=age,event=dead)~geno+ragender,design = svydesign(~ 1, weights = ~ wt,data=df))->mod.wt
## #svycoxph(Surv(time=age,event=dead)~geno+ragender,design = svydesign(~ 1, weights = ~ 1,data=df.mid))->mod.mid
## #svycoxph(Surv(time=age,event=dead)~geno+ragender,design = svydesign(~ 1, weights = ~ 1,data=df.lo))->mod.lo
## #svycoxph(Surv(time=age,event=dead)~geno+ragender,design = svydesign(~ 1, weights = ~ 1,data=df.hi))->mod.hi
## coxph(Surv(time=age,event=dead)~geno+ragender,df)->mod.nowt
## coxph(Surv(time=age,event=dead)~geno+ragender,df.mid)->mod.mid
## coxph(Surv(time=age,event=dead)~geno+ragender,df.lo)->mod.lo
## coxph(Surv(time=age,event=dead)~geno+ragender,df.hi)->mod.hi




## png("/tmp/km-wt.png",units="in",height=4,width=8,res=150,pointsize=13)
## par(mfrow=c(1,2),mar=c(3,3,1.5,0.5),mgp=c(1.5,.6,0),oma=rep(.5,4))
## library(survival)
## #
## plot(NULL,xlab="Age",ylab="",xlim=c(50,max(df$age,na.rm=TRUE)),ylim=c(0,1),main="Males")
## cols<-c("black","gray","green","blue","red")
## #list(mod.nowt,mod.wt,mod.lo,mod.mid,mod.hi)->mods
## list(mod.nowt,mod.lo)->mods
## for (i in 1:length(mods)) {
##     data.frame(geno=1,ragender="1.male")->z
##     survfit(mods[[i]],newdata=z)->mod2
##     lines(mod2$time,mod2$surv,col=cols[i],lwd=2)
##     polygon(c(mod2$time,rev(mod2$time)),c(mod2$upper,rev(mod2$lower)),col=rgb(0.93,0.93,0.93,alpha=.55))
##                                         #
##     data.frame(geno=0,ragender="1.male")->z
##     survfit(mods[[i]],newdata=z)->mod2
##     lines(mod2$time,mod2$surv,lty=2,col=cols[[i]],lwd=2)
## polygon(c(mod2$time,rev(mod2$time)),c(mod2$upper,rev(mod2$lower)),col=rgb(0.93,0.93,0.93,alpha=.55))
## }
## #legend("bottomleft",bty="n",c("Genotyped","Not genotyped","No weights","Weights","Lo","Mid","Hi"),lty=c(1,2,1,1,1,1,1),lwd=2,col=c("black","black","black","gray","green","blue","red"))
## legend("bottomleft",bty="n",c("Genotyped","Not genotyped","Matched-Genotyped","Matched-Not Genotyped"),lty=c(1,2,1,2),lwd=2,col=c("black","black","gray","gray"),cex=.7)
## library(survival)
## #
## plot(NULL,xlab="Age",ylab="",xlim=c(50,max(df$age,na.rm=TRUE)),ylim=c(0,1),main="Females")
## for (i in 1:length(mods)) {
##     data.frame(geno=1,ragender="2.female")->z
##     survfit(mods[[i]],newdata=z)->mod2
##     lines(mod2$time,mod2$surv,col=cols[i],lwd=2)
##                                         #
##     data.frame(geno=0,ragender="2.female")->z
##     survfit(mods[[i]],newdata=z)->mod2
##     lines(mod2$time,mod2$surv,lty=2,col=cols[[i]],lwd=2)
## }
## #legend("bottomleft",bty="n",c("Genotyped","Not genotyped","No weights","Weights","Lo","Mid","Hi"),lty=c(1,2,1,1,1,1,1),lwd=2,col=c("black","black","black","gray","green","blue","red"))
## legend("bottomleft",bty="n",c("Genotyped","Not genotyped","Matched-Genotyped","Matched-Not Genotyped"),lty=c(1,2,1,2),lwd=2,col=c("black","black","gray","gray"),cex=.7)
## dev.off()

## diff<-list()
## fun<-function(mod) {
##     data.frame(geno=1,ragender="1.male")->z
##     survfit(mod,newdata=z)->mod2A
##     #
##     data.frame(geno=0,ragender="1.male")->z
##     survfit(mod,newdata=z)->mod2B
##     #
##     if (!all(mod2A$time==mod2B$time)) stop("problem")
##     mod2A$time>=50 -> index
##     sum(mod2A$surv[index]-mod2B$surv[index])/sum(index)
## }
## fun(mod.nowt)->diff$raw
## fun(mod.lo)->diff$baseline.lo
## fun(mod.mid)->diff$baseline.mid
## fun(mod.hi)->diff$baseline.hi

## #get mod from random forest
## library(randomForest)
## load(file="~/hrs/mortality/random_forest_model.Rdata")
## mod$votes->z
## df[z<quantile(z,.1),]->df.lo
## df[z>quantile(z,.45) & z<quantile(z,.55),]->df.mid
## df[z>quantile(z,.9),]->df.hi

## library(survey)
## svycoxph(Surv(time=age,event=dead)~geno+ragender,design = svydesign(~ 1, weights = ~ 1,data=df.mid))->mod.mid
## svycoxph(Surv(time=age,event=dead)~geno+ragender,design = svydesign(~ 1, weights = ~ 1,data=df.lo))->mod.lo
## svycoxph(Surv(time=age,event=dead)~geno+ragender,design = svydesign(~ 1, weights = ~ 1,data=df.hi))->mod.hi
## fun(mod.lo)->diff$rf.lo
## fun(mod.mid)->diff$rf.mid
## fun(mod.hi)->diff$rf.hi


