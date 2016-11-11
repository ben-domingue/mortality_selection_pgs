#mod is a list of lm objects
table.lm<-function(mod) {
  lapply(mod,function(x) names(coef(x)))->nms
  unique(do.call("c",nms))->nms
  length(nms)->nr
  length(mod)->nc
  mat.est<-mat.tstat<-matrix(NA,nr,nc)
  for (j in 1:nc) {
    summary(mod[[j]])$coef->foo
    for (i in 1:nr) {
      match(nms[i],rownames(foo))->index
      if (length(index)>0) {
        foo[index,1]->mat.est[i,j]
        foo[index,2]->mat.tstat[i,j]
      }
    }
  }
  sapply(mod,function(x) length(residuals(x)))->N
  out<-list()
  new.nms<-list()
  for (i in 1:nr) {
    rbind(mat.est[i,],mat.tstat[i,])->out[[i]]
    new.nms[[i]]<-c(nms[i],paste(nms[i],".se",sep=""))
  }
  do.call("rbind",out)->out
  rbind(out,N)->out
  c(do.call("c",new.nms),"N")->rownames(out)
  out
}


                                        #this applies ipw


load(file="~/hrs/mortality/association.Rdata")
library(ipw)
library(survey)
library(gplots)

df$vN_bmi_nc.pgs -> df$bmi.pgs
df$vN_height_nc.pgs -> df$height.pgs
df$vN_education2_nc.pgs -> df$edu.pgs
df$vN_eversmoke_nc.pgs -> df$smoke.pgs


###################################################################################
#dynamic effects, no sex
dyn.bigf<-function(df,leg=TRUE,title=NULL) {
    assoc_fun<-function(pgs,outcome,df,fm.selection) {
        mods<-list()
                                        #raw
        formula(paste(outcome,".std~",pgs,"*rabyear.raw+ragender*rabyear.raw+ragender*",pgs,sep=""))->fm.pg
        lm(fm.pg,df[is.na(df$radyear),])->mods$still.alive
        lm(fm.pg,df)->mods$naive
        #print(length(mods$naive$resid))
        #print(summary(mods$naive))
                                        #reduced sample
        ## df$racohbyr!="1.ahead" -> test1
        ## df$racohbyr!="6.mid babyboomers" -> test2
        ## df[test1 & test2,]->df.reduced
        ## lm(fm.pg,df.reduced)->mods$reduced
                                        #ipw
        svyglm(fm.pg,design = svydesign(~ 1, weights = ~ wt.health,data=df))->mods$ipw1
        svyglm(fm.pg,design = svydesign(~ 1, weights = ~ wt.byear,data=df))->mods$ipw2
        print(table.lm(mods))
        mods
    }
    mod.dyn<-tab<-list()
    assoc_fun(outcome="bmi",pgs="bmi.pgs",df)->tab$all->mod.dyn$BMI
    assoc_fun(outcome="height",pgs="height.pgs",df)->tab$all->mod.dyn$Height
    assoc_fun(outcome="raedyrs",pgs="edu.pgs",df)->tab$all->mod.dyn$Education
    assoc_fun(outcome="smoke",pgs="smoke.pgs",df)->tab$all->mod.dyn$Smoke
    ## ##################
##     tab<-list()
##     for (nm in names(mod.dyn)) {
##         mod.dyn[[nm]]->x
##         lapply(x,function(x) summary(x)$coef)->tmp
##         foo<-list()
##         for (i in 1:length(tmp)) {
##             tmp[[i]]->z
##             grep("gender",rownames(z))->index
##             z[-c(1,index),1:2]->foo[[i]]
##                                         #tmp[[i]][-c(1,4),1:2]->foo[[i]]
##         }
##         do.call("cbind",foo)->tab[[nm]]
##                                         #cbind(t1,t2,t2[,1]/t1[,1])->tab[[nm]]
##     }
##     do.call("rbind",tab)->tabD
##     grep("pgs:rabyear",rownames(tabD))->index
##     tabD[index,]->tabD
## ##################
    ## f<-function(mod,col,lty,ci=FALSE) {
    ##     1919:1955 -> yrs
    ##                                     #data.frame(pgs=rep(1,length(yrs)),t=yrs)->df.hi
    ##                                     #data.frame(pgs=rep(-1,length(yrs)),t=yrs)->df.lo
    ##     data.frame(yrs,rep(-1,length(yrs)))->df.lo
    ##     data.frame(yrs,rep(+1,length(yrs)))->df.hi
    ##     c("rabyear.raw",names(coef(mod))[2])->names(df.lo)->names(df.hi)
    ##                                     #
    ##     if (!ci) {
    ##         predict(mod,newdata=df.hi)->hi
    ##         predict(mod,newdata=df.lo)->lo
    ##     } else {
    ##         predict(mod,df.lo,interval="confidence")->lo
    ##         predict(mod,df.hi,interval="confidence")->hi
    ##     }
    ##     if (class(lo)=="svystat") {
    ##         matrix(as.numeric(lo),ncol=1)->lo
    ##         matrix(as.numeric(hi),ncol=1)->hi
    ##     }
    ##     if (class(lo)=="numeric") {
    ##         matrix(lo,ncol=1)->lo
    ##         matrix(hi,ncol=1)->hi
    ##     }
    ##                                     #range(c(lo[,1],hi[,1]))->ran
    ##                                     #
    ##     lines(yrs,hi[,1]-lo[,1],col=col,lwd=2.5,lty=lty)
    ##                                     #polygon(c(yrs,rev(yrs)),c(lo[,2],rev(lo[,3])),col=rgb(0.93,0.93,0.93,alpha=.55))
    ##                                     #lines(yrs,hi[,1],col="darkgray",lwd=2.5)
    ##                                     #polygon(c(yrs,rev(yrs)),c(hi[,2],rev(hi[,3])),col=rgb(0.93,0.93,0.93,alpha=.55))
    ## }
    ## col=gray(seq(.6,.3,length.out=length(mod.dyn[[1]])))
    ##                                     #col<-c("black","black","gray","gray")
    ## lty<-c(2,2,1,1)
    ## ylim<-list(c(.28,.85),c(.3,.8),c(.3,.55),c(.1,.4))
    ##                                     #del<-.6
    ##                                     #ylim<-list(c(.3,.3+del),c(.48,.48+del),c(.4,.4+del),c(.1,.1+del))
    ## for (i in 1:length(mod.dyn)) {
    ##     plot(NULL,xlim=c(1919,1955),xlab="",ylab="Diff in std phenotype",ylim=ylim[[i]],lwd=2.5)
    ##     for (j in 1:length(mod.dyn[[i]])) {
    ##         f(mod.dyn[[i]][[j]],col[j],lty[j])
    ##     }
    ##     mtext(side=3,line=.2,names(mod.dyn)[i])
    ##     if (leg & i==4) legend("topleft",bty="n",rev(c("Enhanced mortality","Naive","Weighted, health only","Wt, health + birth year")),col=rev(col),lty=rev(lty),lwd=5,cex=.8)
    ##     if (!is.null(title)) mtext(side=1,line=2,title)
    ##                                     #boxplot
    ##     t(tabD[i,c(1,3,5,7)])->est
    ##     t(tabD[i,1+c(1,3,5,7)])->se
    ##     barplot2(est,beside=TRUE,space=c(0.1,0.1),xlab="",names.arg=rep("",length(est)),
    ##              col=col,
    ##              plot.ci=TRUE,
    ##              ci.u=est+1.96*se,
    ##              ci.l=est-1.96*se
    ##              )->out
    ##                                     #text(est+1.96*se,out,round(est,digits=3),cex=.8,pos=4)
    ## }
}

#par(mfrow=c(2,2),mar=c(3,3,1,1.2),mgp=c(2,1,0),oma=c(1,1,1,1))
#dyn.bigf(df)

dyn.bigf(df)
