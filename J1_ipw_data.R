#this creates data for IPW

library(survival)
library(survey)
library(ipw)
load(file="~/hrs/mortality/df.Rdata")

#load("~/hrs/mortality/all.Rdata")
#df[df$rabyear %in% years,]->df

#special stuff
ifelse(df$radyear<2006,1,0)->df$early.death
ifelse(is.na(df$early.death),0,df$early.death)->df$early.death
abs(1-df$early.death)->df$no.death
#

df[df$white==1,]->df
df[df$rabyear %in% 1919:1955,]->df
    
####################################################################################################################################
#weighting
wt_fun<-function(df) {
                                        #formula("~raedyrs + bmi + height + smoke + cesd + diab + heart + alz + gender+rabyear")->fm.selection
    formula("~raedyrs + bmi + height + smoke + cesd + diab + heart +  gender+rabyear")->fm.selection #no alz
    df[,all.vars(fm.selection)]->tmp
    df[rowSums(is.na(tmp))==0,]->tmp
    ipwpoint(no.death,data=tmp,family="binomial",link="logit",numerator=~1,denominator=~raedyrs + bmi + height + smoke + cesd + diab + heart + gender+ rabyear,trunc=0.01)->wt
    print(table(wt$weights==wt$ipw.weights))
    wt$weights.trunc->tmp$wt.byear
    wt$tmp[,c("hhidpn","wt.byear")]->wt
    merge(df,tmp,all.x=TRUE)
}
wt_fun(df)->df

wt_fun<-function(df) {
    #formula("~raedyrs + bmi + height + smoke + cesd + diab + heart + alz + gender")->fm.selection
    formula("~raedyrs + bmi + height + smoke + cesd + diab + heart +  gender")->fm.selection #no alz
    df[,all.vars(fm.selection)]->tmp
    df[rowSums(is.na(tmp))==0,]->tmp
    ipwpoint(no.death,data=tmp,family="binomial",link="logit",numerator=~1,denominator=~raedyrs + bmi + height + smoke + cesd + diab + heart + gender,trunc=0.01)->wt
    wt$weights.trunc->tmp$wt.health
    print(table(wt$weights==wt$ipw.weights))
    tmp[,c("hhidpn","wt.health")]->wt
    merge(df,tmp,all.x=TRUE)
}
wt_fun(df)->df


## wt_fun<-function(df) {
##     #formula("~raedyrs + bmi + height + smoke + cesd + diab + heart + alz + gender")->fm.selection
##     formula("~rabyear")->fm.selection #no alz
##     df[,all.vars(fm.selection),drop=FALSE]->tmp
##     df[rowSums(is.na(tmp))==0,]->tmp
##     ipwpoint(no.death,data=tmp,family="binomial",link="logit",numerator=~1,denominator=~rabyear,trunc=0.01)->wt
##     wt$weights.trunc->tmp$wt.byearonly
##     print(table(wt$weights==wt$ipw.weights))
##     tmp[,c("hhidpn","wt.health")]->wt
##     merge(df,tmp,all.x=TRUE)
## }
## wt_fun(df)->df

#standardize phenotype
fun<-function(df,nm) {
    std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
    df[[nm]]->x
    std(x)->df[[paste(nm,".std",sep="")]]
    df
}
for (nm in c("bmi","height","raedyrs","smoke")) {
    split(df,df$ragender)->L
    lapply(L,fun,nm)->L
    data.frame(do.call("rbind",L))->df
}

df[!is.na(df$wt.byear),]->df

df$rabyear->hold
(df$rabyear-mean(df$rabyear,na.rm=TRUE))->df$rabyear
hold->df$rabyear.raw

NULL->rownames(df)
save(df,file="~/hrs/mortality/association.Rdata")


#genotyped & not genotyped
load(file="~/hrs/mortality/association.Rdata")

par(mfrow=c(2,1),mgp=c(2,1,0),mar=c(3.5,3.5,2,2))
boxplot(wt.byear~racohbyr,x[x$geno==0,],main="not genotyped",ylim=c(0,3))
boxplot(wt.byear~racohbyr,x[x$geno==1,],main="genotyped",ylim=c(0,3))

fun<-function(df,wt.nm,nm) {
    df[[wt.nm]]->df$wt
    plot(NULL,xlim=c(0.1,3.2),ylim=c(0,12),xlab="Weights",ylab="Density")
    mtext(side=3,line=1,nm)
    lines(density(df$wt[df$geno==0]),lty=1,col="black",lwd=3)
    lines(density(df$wt[df$geno==1]),lty=1,col="gray",lwd=3)
    legend("topright",bty="n",lwd=3,col=c("black","gray"),c("not genotyped","genotyped"))
}
par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(3.3,3.3,2.3,1))
split(df,df$ragender)->tmp
fun(tmp[["1.male"]],wt.nm="wt.health",nm=c("Males, health only"))
fun(tmp[["2.female"]],wt.nm="wt.health",nm=c("Females, health only"))
fun(tmp[["1.male"]],wt.nm="wt.byear",nm=c("Males, with birthyear"))
fun(tmp[["2.female"]],wt.nm="wt.byear",nm=c("Females, with birthyear"))

#3 types
load(file="~/hrs/mortality/association.Rdata")
fun2<-function(df,wt.nm,nm) {
    df[[wt.nm]]->df$wt
    plot(NULL,xlim=c(0.1,3.2),ylim=c(0,12),xlab="Weights",ylab="Density")
    mtext(side=3,line=1,nm)
    lines(density(df$wt[df$geno==0 & df$early.death==1] ),lty=1,col="black",lwd=3)
    lines(density(df$wt[df$geno==0 & df$early.death==0]),lty=2,col="black",lwd=3)
    lines(density(df$wt[df$geno==1]),col="gray",lwd=3)
    legend("topright",bty="n",lwd=3,col=c("black","black","gray"),c("not genotyped & dead","not genotyped & alive","genotyped"),lty=c(1,2,1))
}
par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(3.3,3.3,2.3,1))
split(df,df$ragender)->tmp
fun2(tmp[["1.male"]],wt.nm="wt.health",nm=c("Males, health only"))
fun2(tmp[["2.female"]],wt.nm="wt.health",nm=c("Females, health only"))
fun2(tmp[["1.male"]],wt.nm="wt.byear",nm=c("Males, with birthyear"))
fun2(tmp[["2.female"]],wt.nm="wt.byear",nm=c("Females, with birthyear"))

#
load(file="~/hrs/mortality/association.Rdata")
split(df,df$ragender)->tmp
pf<-function(x,y,g,d,nm) {
    range(y,na.rm=TRUE)->ry
    range(x,na.rm=TRUE)->rx
    plot(NULL,xlim=ry,xlab=nm,ylab="Weights",ylim=c(0,2.8))
    L<-list()
    cbind(y,x)->z
    z[d==1,]->L[[1]]
    z[d==0 & g==0,]->L[[2]]
    z[g==1,]->L[[3]]
    infun<-function(tmp) {
        loess(tmp[,2]~tmp[,1])->m1
        cbind(tmp[,1],m1$fitted)->z
        z[order(z[,1]),]->z
        z
    }
    lines(infun(L[[1]]),col="black",lty=1,lwd=2)
    lines(infun(L[[2]]),col="black",lty=2,lwd=2)
    lines(infun(L[[3]]),col="gray",lty=1,lwd=2)
}
png(paste("/tmp/weights.png",sep=""),units="in",height=10,width=8,res=150,pointsize=13)
par(mfrow=c(4,3),mgp=c(2,1,0),mar=c(3.3,3.3,1,.5),oma=c(.4,.4,1,.4))
for (i in 1:length(tmp)) {
    tmp[[i]]->x
    pf(x$wt.health,x$rabyear.raw,x$geno,x$early.death,nm="Birth year")
    pf(x$wt.health,x$raedyrs,x$geno,x$early.death,nm="Education")
    mtext(side=3,line=0.2,ifelse(i==1,"Males, Health only","Females, Health only"))
    pf(x$wt.health,x$srh,x$geno,x$early.death,nm="Self reported health")
}
for (i in 1:length(tmp)) {
    tmp[[i]]->x
    pf(x$wt.byear,x$rabyear.raw,x$geno,x$early.death,nm="Birth year")
    pf(x$wt.byear,x$raedyrs,x$geno,x$early.death,nm="Education")
    mtext(side=3,line=0.2,ifelse(i==1,"Males, w/ birth year","Females, w/ birth year"))
    pf(x$wt.byear,x$srh,x$geno,x$early.death,nm="Self reported health")
}
legend("topright",bty="n",lwd=3,col=c("black","black","gray"),c("not genotyped & dead","not genotyped & alive","genotyped"),lty=c(1,2,1))
dev.off()




    
























