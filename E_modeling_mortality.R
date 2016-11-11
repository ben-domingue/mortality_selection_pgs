#this creates  data for IPW

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


##                                         #these are the various mortality models
## load("~/hrs/mortality/all.Rdata")
## df[df$rabyear %in% 1910:1959,]->df

ifelse(df$ragender=="1.male","Male","Female")->df$ragender
ifelse(df$white==1,"White","Non-white")->df$white
split(df,list(df$ragender,df$white))->dfL
mod.wrapper<-function(nm,dfL) {
    #
    dfL[[nm]]->df.loc
    "no.death~(raedyrs+bmi+height+smoke+cesd+diab+heart+srh)*rabyear"->fm.baseline
    formula(fm.baseline)->fm
    df.loc[,all.vars(fm)]->tmp
    df.loc[rowSums(is.na(tmp))==0,]->df.loc
    mod<-list()
    #
    fm.list<-list()
    paste("no.death~rabyear")->fm.list[["byear"]]
    for (var in c("raedyrs","bmi","height","smoke","cesd","diab","heart","alz","srh")) {
        paste("no.death~",var,"*rabyear")->fm.list[[var]]
    }
    fm.list$baseline<-fm.baseline
    fm.list$no.interactions<-"no.death~raedyrs+bmi+height+smoke+cesd+diab+heart+srh+rabyear"
    fm.list$no.byear<-"no.death~raedyrs+bmi+height+smoke+cesd+diab+heart+srh"
    ##
    mod.list<-list()
    for (i in 1:length(fm.list)) {
        glm(fm.list[[i]],df.loc,family="binomial")->mod.list[[names(fm.list)[i] ]]
    }
    #
    #library(MASS)
    #stepAIC(mod.list$baseline,scope=list(lower=~1,upper=~(raedyrs+bmi+height+smoke+cesd+diab+heart+srh)*rabyear+I(raedyrs^2)+I(bmi^2)+I(height^2)+I(smoke^2)+I(cesd^2)+I(diab^2)+I(heart^2)+I(alz^2)+I(srh^2)),steps=1000)->mod.list$stepwise
    #
    library(randomForest)
    factor(ifelse(df.loc$no.death==1,"yes","no"))->df.loc$no.death
    randomForest(formula=formula(fm.baseline),data=df.loc)->mod.list$rfor
    mod.list
}
mods<-list()
for (i in 1:length(dfL)) mod.wrapper(names(dfL)[i],dfL=dfL)->mods[[i]]

tab1<-tab2<-list()
for (i in 1:length(mods)) {
    mods[[i]]->z
    print(length(z[[1]]$resid))
    z$no.interactions->m1
    z$no.byear->m2
    summary(m1)$coef[,1:2]->tab1[[i]]
    summary(m2)$coef[,1:2]->tab2[[i]]
}
do.call("cbind",tab1)->tab1
do.call("cbind",tab2)->tab2

    

fun<-function(df.loc,mod) {
    library(cvAUC)
    tab.fun<-function(M) {
        #library(pscl)
        #pR2(M)[4]->tr1
        length(M$fitted)->N
        summary(M)$aic->aic
        AUC(M$fitted,M$y)->auc
        #c(N,aic,tr1,auc)
        c(N,aic,auc)
    }
    rf.fun<-function(M) {
        length(M$votes[,2])->N
        M$votes[,2]->pr
        ifelse(M$y=="yes",1,0)->obs
        AUC(pr,obs)->auc
        c(N,NA,auc)
    }
    tab<-list()
    for (i in 1:length(mod)) {
        mod[[i]]->M
        if (any(class(M)=="glm")) {
            tab.fun(M)->tr
        } else {
            rf.fun(M)->tr
        }
        tr->tab[[i]]
    }
    do.call("rbind",tab)->tab
    rownames(tab)<-names(mod)
    tab
}    
tab<-list()
for (i in 1:length(mods)) fun(df.loc=dfL[[i]],mod=mods[[i]])->tab[[i]]
do.call("cbind",tab)->tab
