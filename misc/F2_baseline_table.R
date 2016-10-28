#this looked at the two-step stuff.


## load("~/hrs/mortality/all.Rdata")
## df[df$rabyear %in% 1910:1959,]->df
## ifelse(df$ragender=="1.male","Male","Female")->df$ragender
## ifelse(df$white==1,"White","Non-white")->df$white
## ifelse(df$radyear<2006,1,0)->df$early.death
## ifelse(is.na(df$early.death),0,df$early.death)->df$early.death
## #load("/tmp/geno_refusal.Rdata") #see B3_saliva_consent.R
## #gr[,c("hhidpn","geno.refusal")]->gr
## #merge(df,gr,all.x=TRUE)->df
## abs(1-df$early.death)->df$no.death
## df$rabyear-mean(df$rabyear,na.rm=TRUE)->df$rabyear

## df[df$early.death==0,]->tmp
## nrow(tmp)
## table(tmp$geno)/nrow(tmp)
## table(df$geno)/nrow(df)


## library(cvAUC)
## fun<-function(fm,df) {
##     split(df,list(df$ragender,df$white))->dfL
##     names(dfL)->nms
##     mod.wrapper<-function(nm,dfL) {
##         #
##         dfL[[nm]]->df.loc
##         glm(fm,df.loc,family="binomial")->mod
##         #
##         AUC(mod$fitted,mod$y)->auc
##         print(auc)
##         #
##         summary(mod)$coef
##     }
##     mods<-list()
##     for (i in 1:length(dfL)) mod.wrapper(names(dfL)[i],dfL=dfL)->mods[[i]]
##     lapply(mods,function(x) x[,c(1,3)])->mods
##     do.call("cbind",mods)->tab
##     colnames(tab)[c(1,3,5,7)]<-nms
##     tab
## }

## #fun(fm="geno~(raedyrs+bmi+height+smoke+cesd+diab+heart+alz+srh)*rabyear",df)->tab #this went in table



## out<-list()
## fun(fm="no.death~raedyrs+bmi+height+smoke+cesd+diab+heart+alz+srh+rabyear",df)->out$step1
## #fun(fm="geno~(raedyrs+bmi+height+smoke+cesd+diab+heart+alz+srh)*rabyear",df)->out$original
## #fun(fm="geno~(raedyrs+bmi+height+smoke+cesd+diab+heart+alz+srh)*rabyear",df[df$early.death==0,])->out$step2

## out$step1[,5]/out$original[,5]->t1
## out$step2[,5]/out$step1[,5]->t2
## cbind(t1,t2)
## out$step1[,5]/out$step2[,5]


## #pdf("/tmp/est.pdf",width=8,height=12)
## par(mfrow=c(2,2),mgp=c(2.5,1,0),mar=c(3,7,3,1))
## for (i in c(1,3,5,7)) {
##     #all
##     plot(out$original[,i+1],1:nrow(out$original),main=colnames(out$original)[i],type="n",yaxt="n",xlab="",ylab="")
##     axis(side=2,at=1:nrow(out$original),rownames(out$original),cex=.7,las=2)
##     points(out$original[,i+1],2/4+1:nrow(out$original),pch=19)
##     #step 1
##     text(out$step1[,i+1],1:nrow(out[[1]]),1)
##     #step 2
##     text(out$step2[,i+1],1/4+1:nrow(out[[1]]),2)
##     abline(v=0)
##     abline(v=-2,lty=2)
##     abline(v=2,lty=2)
##     for (j in 1:nrow(out[[1]])) arrows(out$step1[j,i+1],j,out$step2[j,i+1],1/4+j,length=.1)
## }
## #dev.off()
