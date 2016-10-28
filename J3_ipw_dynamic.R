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
        formula(paste(outcome,".std~",pgs,"*rabyear.raw",sep=""))->fm.pg
        lm(fm.pg,df[is.na(df$radyear),])->mods$still.alive
        lm(fm.pg,df)->mods$naive
        print(length(mods$naive$resid))
        print(summary(mods$naive))
                                        #reduced sample
        ## df$racohbyr!="1.ahead" -> test1
        ## df$racohbyr!="6.mid babyboomers" -> test2
        ## df[test1 & test2,]->df.reduced
        ## lm(fm.pg,df.reduced)->mods$reduced
                                        #ipw
        svyglm(fm.pg,design = svydesign(~ 1, weights = ~ wt.health,data=df))->mods$ipw1
        svyglm(fm.pg,design = svydesign(~ 1, weights = ~ wt.byear,data=df))->mods$ipw2
        mods
    }
    mod.dyn<-tab<-list()
    assoc_fun(outcome="bmi",pgs="bmi.pgs",df)->tab$all->mod.dyn$BMI
    assoc_fun(outcome="height",pgs="height.pgs",df)->tab$all->mod.dyn$Height
    assoc_fun(outcome="raedyrs",pgs="edu.pgs",df)->tab$all->mod.dyn$Education
    assoc_fun(outcome="smoke",pgs="smoke.pgs",df)->tab$all->mod.dyn$Smoke
##################
    tab<-list()
    for (nm in names(mod.dyn)) {
        mod.dyn[[nm]]->x
        lapply(x,function(x) summary(x)$coef)->tmp
        foo<-list()
        for (i in 1:length(tmp)) {
            tmp[[i]]->z
            grep("gender",rownames(z))->index
            z[-c(1,index),1:2]->foo[[i]]
                                        #tmp[[i]][-c(1,4),1:2]->foo[[i]]
        }
        do.call("cbind",foo)->tab[[nm]]
                                        #cbind(t1,t2,t2[,1]/t1[,1])->tab[[nm]]
    }
    do.call("rbind",tab)->tabD
    grep("pgs:rabyear",rownames(tabD))->index
    tabD[index,]->tabD
##################
    f<-function(mod,col,lty,ci=FALSE) {
        1919:1955 -> yrs
                                        #data.frame(pgs=rep(1,length(yrs)),t=yrs)->df.hi
                                        #data.frame(pgs=rep(-1,length(yrs)),t=yrs)->df.lo
        data.frame(yrs,rep(-1,length(yrs)))->df.lo
        data.frame(yrs,rep(+1,length(yrs)))->df.hi
        c("rabyear.raw",names(coef(mod))[2])->names(df.lo)->names(df.hi)
                                        #
        if (!ci) {
            predict(mod,newdata=df.hi)->hi
            predict(mod,newdata=df.lo)->lo
        } else {
            predict(mod,df.lo,interval="confidence")->lo
            predict(mod,df.hi,interval="confidence")->hi
        }
        if (class(lo)=="svystat") {
            matrix(as.numeric(lo),ncol=1)->lo
            matrix(as.numeric(hi),ncol=1)->hi
        }
        if (class(lo)=="numeric") {
            matrix(lo,ncol=1)->lo
            matrix(hi,ncol=1)->hi
        }
                                        #range(c(lo[,1],hi[,1]))->ran
                                        #
        lines(yrs,hi[,1]-lo[,1],col=col,lwd=2.5,lty=lty)
                                        #polygon(c(yrs,rev(yrs)),c(lo[,2],rev(lo[,3])),col=rgb(0.93,0.93,0.93,alpha=.55))
                                        #lines(yrs,hi[,1],col="darkgray",lwd=2.5)
                                        #polygon(c(yrs,rev(yrs)),c(hi[,2],rev(hi[,3])),col=rgb(0.93,0.93,0.93,alpha=.55))
    }
    col=gray(seq(.6,.3,length.out=length(mod.dyn[[1]])))
                                        #col<-c("black","black","gray","gray")
    lty<-c(2,2,1,1)
    ylim<-list(c(.28,.85),c(.3,.8),c(.3,.55),c(.1,.4))
                                        #del<-.6
                                        #ylim<-list(c(.3,.3+del),c(.48,.48+del),c(.4,.4+del),c(.1,.1+del))
    for (i in 1:length(mod.dyn)) {
        plot(NULL,xlim=c(1919,1955),xlab="",ylab="Diff in std phenotype",ylim=ylim[[i]],lwd=2.5)
        for (j in 1:length(mod.dyn[[i]])) {
            f(mod.dyn[[i]][[j]],col[j],lty[j])
        }
        mtext(side=3,line=.2,names(mod.dyn)[i])
        if (leg & i==4) legend("topleft",bty="n",rev(c("Enhanced mortality","Naive","Weighted, health only","Wt, health + birth year")),col=rev(col),lty=rev(lty),lwd=5,cex=.8)
        if (!is.null(title)) mtext(side=1,line=2,title)
                                        #boxplot
        t(tabD[i,c(1,3,5,7)])->est
        t(tabD[i,1+c(1,3,5,7)])->se
        barplot2(est,beside=TRUE,space=c(0.1,0.1),xlab="",names.arg=rep("",length(est)),
                 col=col,
                 plot.ci=TRUE,
                 ci.u=est+1.96*se,
                 ci.l=est-1.96*se
                 )->out
                                        #text(est+1.96*se,out,round(est,digits=3),cex=.8,pos=4)
    }
}

#par(mfrow=c(2,2),mar=c(3,3,1,1.2),mgp=c(2,1,0),oma=c(1,1,1,1))
#dyn.bigf(df)

pdf("/tmp/dyn.pdf")
matrix(c(1,1,2,3,3,4,5,5,6,7,7,8),4,3,byrow=TRUE)->m1
matrix(c(9,9,10,11,11,12,13,13,14,15,15,16),4,3,byrow=TRUE)->m2
layout(cbind(m1,m2))
par(mar=c(3,3,1,.5),mgp=c(2,1,0),oma=c(1,1,1,1))
dyn.bigf(df[df$gender=="female",])
mtext(side=1,line=3,"Females")
dyn.bigf(df[df$gender=="male",])
mtext(side=1,line=3,"Males")
dev.off()


###################################################################################
###############
##table


assoc_fun<-function(pgs,outcome,df,fm.selection) {
    mods<-list()
    #raw
    formula(paste(outcome,".std~",pgs,"*rabyear.raw+gender*rabyear.raw+gender*",pgs,sep=""))->fm.pg
    lm(fm.pg,df[is.na(df$radyear),])->mods$still.alive
    lm(fm.pg,df)->mods$naive
    print(length(mods$naive$resid))
    print(summary(mods$naive))
    #reduced sample
    ## df$racohbyr!="1.ahead" -> test1
    ## df$racohbyr!="6.mid babyboomers" -> test2
    ## df[test1 & test2,]->df.reduced
    ## lm(fm.pg,df.reduced)->mods$reduced
    #ipw
    svyglm(fm.pg,design = svydesign(~ 1, weights = ~ wt.health,data=df))->mods$ipw1
    svyglm(fm.pg,design = svydesign(~ 1, weights = ~ wt.byear,data=df))->mods$ipw2
    #lm(fm.pg,df[df$rabyear.raw.raw>1919,])->mods$no.old
    mods
}
## fun<-function(z) {
##     table.lm(z)->tab
##     apply(tab,2,unlist)->tab
##     do.call("cbind",tab)->tab
##     tab[-nrow(tab),]->tab
##     cbind(tab,tab[,2]/tab[,1],tab[,3]/tab[,1])->tab
##     tab[,c(1,4,5)]
## }

mod.dyn<-tab<-list()
assoc_fun(outcome="bmi",pgs="bmi.pgs",df)->tab$all->mod.dyn$BMI
assoc_fun(outcome="height",pgs="height.pgs",df)->tab$all->mod.dyn$Height
assoc_fun(outcome="raedyrs",pgs="edu.pgs",df)->tab$all->mod.dyn$Education
assoc_fun(outcome="smoke",pgs="smoke.pgs",df)->tab$all->mod.dyn$Smoke


lapply(mod.dyn,table.lm)































## pf<-function(nm,mod.dyn) {
##     mod.dyn[[nm]]->z
##     lapply(z,function(x) summary(x)$coef)->z
##     me<-inter<-list()
##     for (i in 1:length(z)) {
##         grep(".pgs",rownames(z[[i]]),fixed=TRUE)->index
##         z[[i]][index,]->tab
##         grep(":",rownames(tab))->i2
##         which(!(1:nrow(tab)==i2))->i1
##         tab[i1,]->me[[i]]
##         tab[i2,]->inter[[i]]
##     }
##     do.call("rbind",me)->me
##     do.call("rbind",inter)->inter
##     dotchart(me[,1],xlim=c(0,max(me[,1])))
##     if (nm=="Smoke") mtext(side=4,las=2,at=1:length(z),names(z),cex=.7)
##     mtext(side=3,line=0,nm)
##     if (nm=="BMI") mtext(side=2,line=0,"pgs main effect")
##     #for (i in 1:nrow(me)) segments(me[i,1]-1.96*me[i,2],i,me[i,1]+1.96*me[i,2],i)
##     ##
##     which.max(abs(inter[,1]))->M
##     inter[M,1]->m
##     m+.1*m -> m
##     if (sign(inter[M,1])<0) c(m,0)->ran else c(0,m)->ran
##     dotchart(rep(-100,4),xlim=ran,pch=0)
##     if (nm=="BMI") mtext(side=2,line=0,"pgs interaction")
##     for (i in 1:nrow(inter)) {
##         ifelse(inter[i,4]<.01,"+","o")->txt
##         ifelse(inter[i,4]<.05,"*",txt)->txt
##         ifelse(inter[i,4]<.01,"**",txt)->txt
##         ifelse(inter[i,4]<.001,"***",txt)->txt
##         text(inter[i,1],i,txt,cex=2)
##     }
##     if (nm=="Smoke") mtext(side=4,las=2,at=1:length(z),names(z),cex=.7)
##     #for (i in 1:nrow(inter)) segments(inter[i,1]-1.96*inter[i,2],i,inter[i,1]+1.96*inter[i,2],i)
## }
## png(paste("/tmp/effects.png",sep=""),units="in",height=10,width=8,res=150,pointsize=13)
## par(mfcol=c(2,4),mar=c(3,.5,1,0.2),mgp=c(1.5,.75,0),oma=c(.5,.5,.5,5))
## for (i in 1:length(mod.dyn)) pf(names(mod.dyn)[i],mod.dyn)
## dev.off()

        
        
    

#from penetrance paper.
## [1] "Education"
## Call:
## lm(formula = y ~ pgs * t)
## Residuals:
##    Min     1Q Median     3Q    Max 
## -5.403 -0.560 -0.145  0.804  2.146 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -34.95397    2.17374  -16.08   <2e-16 ***
## pgs           4.55045    2.19495    2.07    0.038 *  
## t             0.01804    0.00112   16.08   <2e-16 ***
## pgs:t        -0.00225    0.00113   -1.99    0.047 *  
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## Residual standard error: 0.969 on 8847 degrees of freedom
##   (14 observations deleted due to missingness)
## Multiple R-squared:  0.0609,	Adjusted R-squared:  0.0606 
## F-statistic:  191 on 3 and 8847 DF,  p-value: <2e-16
