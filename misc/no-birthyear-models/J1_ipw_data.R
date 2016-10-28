library(survival)
library(survey)
library(ipw)
load(file="~/hrs/mortality/df.Rdata")
df[df$rabyear %in% 1910:1959,]->df


##first get & standardize polygenic scores
grep("profile",names(df))->index
for (nm in names(df)[index]) {
    df[[nm]]->z
    (z-mean(z,na.rm=TRUE))/sd(z,na.rm=TRUE)->df[[paste(nm,".nopcs",sep="")]]
    lm(paste(nm,"~",paste(paste("pc",1:10,sep=""),collapse="+")),df)->mod
    mod$resid->z
    (z-mean(z,na.rm=TRUE))/sd(z,na.rm=TRUE)->df[[nm]]
}
names(df)->nms
gsub(".profile",".pgs",nms)->nms
nms->names(df)
grep(".pgs$",names(df))->index
df[,c("subjectID","hhidpn",names(df)[index]),]->scores

load("~/hrs/mortality/all.Rdata")
df[df$rabyear %in% 1910:1959,]->df

#special stuff
ifelse(df$radyear<2006,1,0)->df$early.death
ifelse(is.na(df$early.death),0,df$early.death)->df$early.death
abs(1-df$early.death)->df$no.death
#

ifelse(df$white==1,"wh","not")->df$white
df[df$white=="wh",]->df #pgs won't be meaningful for non-whites
dim(df)
merge(df,scores,all.x=TRUE)->df
dim(df)
    
####################################################################################################################################
#weighting
wt_fun<-function(df) {
    formula("~raedyrs + bmi + height + smoke + cesd + diab + heart + alz + gender")->fm.selection
    df[,all.vars(fm.selection)]->tmp
    df[rowSums(is.na(tmp))==0,]->tmp
    ipwpoint(no.death,data=tmp,family="binomial",link="logit",numerator=~1,denominator=~raedyrs + bmi + height + smoke + cesd + diab + heart + alz+gender,trunc=0.01)->wt
    wt$weights.trunc->tmp$wt
    tmp[,c("hhidpn","wt")]->wt
    merge(df,tmp,all.x=TRUE)
}
wt_fun(df)->df

## split(df,list(df$ragender))->dfL
## wt_fun<-function(df) {
##     formula("~(raedyrs + bmi + height + smoke + cesd + diab + heart + alz)*rabyear")->fm.selection
##     df[,all.vars(fm.selection)]->tmp
##     df[rowSums(is.na(tmp))==0,]->tmp
##     ipwpoint(no.death,data=tmp,family="binomial",link="logit",numerator=~1,denominator=~(raedyrs + bmi + height + smoke + cesd + diab + heart + alz) * rabyear,trunc=0.01)->wt
##     wt$weights.trunc->tmp$wt
##     tmp[,c("hhidpn","wt")]->wt
##     merge(df,tmp,all.x=TRUE)
## }
## lapply(dfL,wt_fun)->dfL
## data.frame(do.call("rbind",dfL))->df

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

df[!is.na(df$wt),]->df

df$rabyear->hold
(df$rabyear-mean(df$rabyear,na.rm=TRUE))->df$rabyear
hold->df$rabyear.raw
#df$rabyear-1919->df$rabyear

NULL->rownames(df)
## save(df,file="~/hrs/mortality/association.Rdata")



## load(file="~/hrs/mortality/association.Rdata")
## plot(NULL,xlim=c(0.2,3.2),ylim=c(0,12),xlab="Weights",ylab="Density")
## lines(density(df$wt[df$geno==0 & df$ragender=="2.female"]),lty=1,col="black",lwd=2)
## lines(density(df$wt[df$geno==1 & df$ragender=="2.female"]),lty=2,col="black",lwd=2)
## lines(density(df$wt[df$geno==0 & df$ragender=="1.male"]),lty=1,col="gray",lwd=2)
## lines(density(df$wt[df$geno==1 & df$ragender=="1.male"]),lty=2,col="gray",lwd=2)
## legend("topright",bty="n",lwd=2,lty=c(1,2,1,2),col=c("black","black","gray","gray"),c("Female, not genotyped","Female, genotyped","Male, not genotyped","Male, genotyped"))


    
























