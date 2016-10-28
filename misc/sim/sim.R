#trying to get some sense for what param estimates should be from empirical data
load("~/hrs/mortality/all.Rdata")
df[df$rabyear %in% 1910:1959,]->df
ifelse(df$white==1,"wh","not")->df$white
df[df$white=="wh",]->df #pgs won't be meaningful for non-whites

##b.pgs
##going to be 0.2, previous work

##b.year
lm(alz~rabyear,df) 
#-0.005

#year.mort
glm(geno~rabyear,df,family="binomial") #~0.02
glm(dead~rabyear,df,family="binomial") #~ -0.13
#y.mort
glm(geno~alz,df,family="binomial") #-0.7
glm(dead~alz,df,family="binomial") #1.6



df[,c("geno","dead","rabyear","alz","bmi","raedyrs","height","diab","heart","cesd","smoke")]->tmp
names(tmp)->nms
for (nm in nms[-(1:3)]) {
    tmp[[nm]]->z
    (z-mean(z,na.rm=TRUE))/sd(z,na.rm=TRUE)->tmp[[nm]]
}
cor(tmp,use='p')[,1:2]
cor(tmp[tmp$geno==0,],use='p')[,1:2]

#i don't think you want to use these since everything is standardized.
## coef(lm(alz~rabyear,tmp)) #b.byear
## coef(glm(geno~rabyear,tmp,family="binomial")) #byear.geno
## coef(glm(geno~alz,tmp,family="binomial")) #y.geno

df[,c("geno","rabyear","alz","raedyrs")]->tmp
names(tmp)->nms
for (nm in nms[-(1:2)]) {
    tmp[[nm]]->z
    (z-mean(z,na.rm=TRUE))/sd(z,na.rm=TRUE)->tmp[[nm]]
}
cor(tmp,use='p')[3:4,1:2]
cor(tmp[tmp$geno==0,],use='p')[,1:2]


## ###############################################################################################
## #simulation based on observed/complete data
## #don't change anything in here!
## fun<-function(b.pgs=0.2, #effect of pgs on trait (at middle birth year)
##               b.year=-0.005, #secular trend
##               inter, #dynamics of pgs
##               y.geno=-0.7, #effect of trait on genetic sample membership
##               year.geno=0.02, #effect of birth year on being in sample
##               N=24000 #the sample size
##               ) {
##     tab.est<-tab.t<-list()
##     for (i in 1:25) {
##         pgs<-rnorm(N)
##         byear<-round(rnorm(N,mean=0,sd=5))
##         y<-b.pgs*pgs+b.year*byear+inter*pgs*byear+rnorm(N)
##         pvg<-exp(-.41+y.geno*y+year.geno*byear)#aiming for ~40% of sample to be genotyped, to be similar to hrs whites.
##         pvg<-pvg/(1+pvg)
##         geno<-rbinom(N,1,pvg) 
##         data.frame(y=y,geno=geno,pgs=pgs,byear=byear)->df
##         lm(y~pgs,df)->m1
##         lm(y~pgs,df[df$geno==1,])->m2
##         lm(y~pgs*byear,df)->m3
##         lm(y~pgs*byear,df[df$geno==1,])->m4
##         true<-c(b.pgs,b.year,inter)
##         #estimates
##         c(summary(m1)$coef[2,1],NA,NA)->est.static
##         c(summary(m2)$coef[2,1],NA,NA)->naive.static
##         summary(m3)$coef[-1,1]->est.dyn
##         summary(m4)$coef[-1,1]->naive.dyn
##         cbind(true,est.static,naive.static,est.dyn,naive.dyn)->tab.est[[i]]
##         #t-stats
##         c(summary(m1)$coef[2,3],NA,NA)->est.static
##         c(summary(m2)$coef[2,3],NA,NA)->naive.static
##         summary(m3)$coef[-1,3]->est.dyn
##         summary(m4)$coef[-1,3]->naive.dyn
##         cbind(true,est.static,naive.static,est.dyn,naive.dyn)->tab.t[[i]]
##         list(tab.est,tab.t)
##     }
##     infun<-function(tab) {
##         Reduce("+",tab)->tmp
##         tmp/length(tab)->tmp
##         colnames(tmp)<-c("true","est.static","naive.static","est.dyn","naive.dyn")
##         rownames(tmp)<-c("pgs.main","year.main","inter")
##         tmp
##     }
##     list(est=tab.est,t=tab.t)->L
##     lapply(L,infun)
## }

## #but feel free to change these arguments
## fun(inter=0) #no dynamics
## #note that the naive models (which estimate only in the sample left alive) consistently underestimate the main genetic effect.
## #more interestingly, we also get an induced trend (year.main) and som evidence for a interaction.
## fun(inter=-.003) #decreasing genetic effect
## #again, reduced main effets. what's interesting is just how piss poor the estimates of interactions are.
## fun(inter=+.003) #increasing genetic effect
## #again, interactions are hard!


###############################################################################################
#results based on ipw



library(survey)
library(cvAUC)
fun<-function(b.pgs=0.2, #effect of pgs on trait (at middle birth year)
              b.year=-0.005, #secular trend
              inter, #dynamics of pgs
              y.geno=-0.7, #effect of trait on genetic sample membership
              year.geno=0.02, #effect of birth year on being in sample
              N=24000 #the sample size
              ) {
    tab.est<-tab.t<-list()
    auc<-list()
    for (i in 1:100 ) {
        pgs<-rnorm(N)
        byear<-round(rnorm(N,mean=0,sd=5))
        #
        y<-b.pgs*pgs+b.year*byear+inter*pgs*byear+rnorm(N)
        pvg<-exp(-.41+y.geno*y+year.geno*byear)#aiming for ~40% of sample to be genotyped, to be similar to hrs whites.
        pvg<-pvg/(1+pvg)
        pvg2<-exp(-.41+y.geno*y+year.geno*byear+rnorm(N))#aiming for ~40% of sample to be genotyped, to be similar to hrs whites.
        pvg2<-pvg2/(1+pvg2)
        geno<-rbinom(N,1,pvg) 
        AUC(pvg,geno)->auc[i]
        data.frame(y=y,geno=geno,pgs=pgs,byear=byear,wt=1/pvg,wt2=1/pvg2)->df
        lm(y~pgs,df)->m1
        lm(y~pgs,df[df$geno==1,])->m2a
        svyglm(y~pgs,design = svydesign(~ 1, weights = ~ wt,data=df[df$geno==1,]))->m2b
        lm(y~pgs*byear,df)->m3
        lm(y~pgs*byear,df[df$geno==1,])->m4a
        svyglm(y~pgs*byear,design = svydesign(~ 1, weights = ~ wt,data=df[df$geno==1,]))->m4b
        true<-c(b.pgs,b.year,inter)
        #estimates
        c(summary(m1)$coef[2,1],NA,NA)->est.static
        c(summary(m2a)$coef[2,1],NA,NA)->naive.static
        c(summary(m2b)$coef[2,1],NA,NA)->ipw.static
        summary(m3)$coef[-1,1]->est.dyn
        summary(m4a)$coef[-1,1]->naive.dyn
        summary(m4b)$coef[-1,1]->ipw.dyn
        cbind(true,est.static,naive.static,ipw.static,est.dyn,naive.dyn,ipw.dyn)->tab.est[[i]]
        #t-stats
        c(summary(m1)$coef[2,3],NA,NA)->est.static
        c(summary(m2a)$coef[2,3],NA,NA)->naive.static
        c(summary(m2b)$coef[2,3],NA,NA)->ipw.static
        summary(m3)$coef[-1,3]->est.dyn
        summary(m4a)$coef[-1,3]->naive.dyn
        summary(m4b)$coef[-1,3]->ipw.dyn
        cbind(true,est.static,naive.static,ipw.static,est.dyn,naive.dyn,ipw.dyn)->tab.t[[i]]
        list(tab.est,tab.t)
    }
    print(summary(unlist(auc)))
    ## infun<-function(tab) {
    ##     Reduce("+",tab)->tmp
    ##     tmp/length(tab)->tmp
    ##     colnames(tmp)<-c("true","est.static","naive.static","ipw.static","est.dyn","naive.dyn","ipw.dyn")
    ##     rownames(tmp)<-c("pgs.main","year.main","inter")
    ##     tmp
    ## }
    ## list(est=tab.est,t=tab.t)->L
    ## lapply(L,infun)
    infun<-function(tab) {
        tab[,1]->true
        tab[1,2:4]->static
        tab[1,5:7]->dyn.main
        tab[2,5:7]->dyn.year
        tab[3,5:7]->dyn.inter
        list(true=true,static=static,dyn.main=dyn.main,dyn.year=dyn.year,dyn.inter=dyn.inter)
    }
    lapply(tab.est,infun)->x
    bias<-list()
    for (i in 1:5) {
        lapply(x,"[[",i)->tmp
        do.call("rbind",tmp)->bias[[names(x[[1]])[i] ]]
    }    
    bias
}

set.seed(011181)
fun(inter=0)->f1 #no dynamics
fun(inter=-.003)->f2 #decreasing genetic effect
fun(inter=+.003)->f3 #increasing genetic effect
list(f1,f2,f3)->F

## fun<-function(f) {
##     tab<-list()
##     for (i in 1:3) {
##         rbind(f[[1]][i,],f[[2]][i,])->tab[[i]]
##     }
##     do.call("rbind",tab)->tab
##     tab
## }
## fun(f1)
## fun(f2)
## fun(f3)

png("/tmp/box.png",units="in",height=10,width=8,res=150,pointsize=13)
par(mfcol=c(4,3),mar=c(3,4,1.5,0.5),mgp=c(1.5,.75,0),oma=rep(.5,4))
true.vals<-c(NA,1,1,2,3)
txt<-c("No dynamics","Dynamically decreasing","Dynamically increasing")
xt<-c("Eqn 3, b1","Eqn 4, b1","Eqn 4, b2","Eqn 4, b3")
counter<-1
lims<-list(c(0.15,0.25),c(0.15,0.25),c(-.012,0.008),list(c(-.007,.007),c(-.009,.004),c(-.004,.009)))
for (i in 1:length(F)) {
    for (j in 2:length(F[[i]])) {
        unique(F[[i]]$true[,true.vals[j] ])->tv
        F[[i]][[j]]->vals
        if (j<5) lims[[j-1]]->yl else lims[[j-1]][[i]]->yl
        boxplot(vals,names=c("All data","Naive","IPW"),main=txt[i],xlab=xt[j-1],ylim=yl)
        abline(h=tv)
        mtext(side=3,adj=0,letters[counter])
        counter<-counter+1
    }
}
dev.off()



