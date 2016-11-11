
load(file="~/hrs/mortality/association.Rdata")
library(ipw)
library(survey)
library(gplots)


df$vN_bmi_cl.pgs -> df$bmi.pgs
df$vN_height_cl.pgs -> df$height.pgs
df$vN_education2_cl.pgs -> df$edu.pgs
df$vN_eversmoke_cl.pgs -> df$smoke.pgs

read.table("~/hrs/swb/hwe_eur_keep.txt",header=FALSE)->ids
dim(df)
df$subjectID %in% ids[,2] -> df$euro

L<-list(BMI=c("bmi.std","vN_bmi_nc.pgs","vN_bmi_nc0.1.pgs","vN_bmi_cl.pgs"),
        Height=c("height.std","vN_height_nc.pgs","vN_height_nc0.1.pgs","vN_height_cl.pgs"),
        Edu=c("raedyrs.std","vN_education2_nc.pgs","vN_education2_nc0.1.pgs","vN_education2_cl.pgs"),
        Smoke=c("smoke.std","vN_eversmoke_nc.pgs","vN_eversmoke_nc0.1.pgs","vN_eversmoke_cl.pgs")
        )
mod<-list()
for (i in 1:length(L)) {
    df[[L[[i]][1] ]] ->df$pheno
    df[[L[[i]][2] ]] ->df$pgs
    m<-list()
    lm(pheno~pgs+ragender+rabyear,df)->m[[1]]
    df[[L[[i]][3] ]] ->df$pgs
    lm(pheno~pgs+ragender+rabyear,df)->m[[2]]
    df[[L[[i]][4] ]] ->df$pgs
    lm(pheno~pgs+ragender+rabyear,df)->m[[3]]
    df[[L[[i]][2] ]] ->df$pgs
    lm(pheno~pgs+ragender+rabyear,df[df$euro,])->m[[4]]
    lapply(m,function(x) summary(x)$coef[2,])->tab
    do.call("rbind",tab)->mod[[names(L)[i] ]]
}

mod->mod.static
tab<-list()
for (nm in c("BMI","Height","Edu","Smoke")) {
    mod.static[[nm]]->x
    as.numeric(t(x[,1:2]))->tab[[nm]]
                                        #c(x[1,1],x[1,2],x[2,1],x[2,2],x[2,1]/x[1,1])->tab[[nm]]
}
do.call("rbind",tab)->tabS
t(tabS[,c(1,3,5,7)])->est
t(tabS[,1+c(1,3,5,7)])->se

png("/tmp/static.png",units="in",height=5,width=6,res=100)
par(mgp=c(2,1,0))
barplot2(est,horiz=TRUE,beside=TRUE,xlab="PGS Main Effect",
         col=gray(seq(.9,.3,length.out=ncol(est))),
         plot.ci=TRUE,xlim=c(0,.5),
         ci.u=est+1.96*se,
         ci.l=est-1.96*se,cex.axis=.8
         )->out
text(est+1.96*se,out,round(est,digits=3),cex=1,offset=1,pos=4)
legend("topright",bty="n",rev(c("No Clumping","p=0.5","Clumping","No clumping, European")),fill=rev(gray(seq(.9,.3,length.out=ncol(est)))),cex=.85)
dev.off()

    



#########################################################################3
#This looked at time-invariant analyes with all the scores
## png("/tmp/sense-static.png",units="in",height=11,width=7,res=100)
## par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(3.1,3.1,2,1))

## xl<-c(0,.55)
##                                         #load static.bigf from ../J2_...
## #########################################################################
## df$vN_bmi_nc0.1.pgs -> df$bmi.pgs
## df$vN_height_nc0.1.pgs -> df$height.pgs
## df$vN_education2_nc0.1.pgs -> df$edu.pgs
## df$vN_eversmoke_nc0.1.pgs -> df$smoke.pgs
## static.bigf(df,leg=FALSE,title="0.1 threshold",xl=xl)

## df$vN_bmi_nc0.5.pgs -> df$bmi.pgs
## df$vN_height_nc0.5.pgs -> df$height.pgs
## df$vN_education2_nc0.5.pgs -> df$edu.pgs
## df$vN_eversmoke_nc0.5.pgs -> df$smoke.pgs
## static.bigf(df,leg=FALSE,title="0.5 threshold",xl=xl)

## #########################################################################
## df$vN_bmi_cl.pgs -> df$bmi.pgs
## df$vN_height_cl.pgs -> df$height.pgs
## df$vN_education2_cl.pgs -> df$edu.pgs
## df$vN_eversmoke_cl.pgs -> df$smoke.pgs
## static.bigf(df,leg=FALSE,title="Clumped",xl=xl)
## #########################################################################

## read.table("~/hrs/swb/hwe_eur_keep.txt",header=FALSE)->ids
## dim(df)
## df[df$subjectID %in% ids[,2],]->tmp
## dim(tmp)
## tmp$vN_bmi_nc.pgs -> tmp$bmi.pgs
## tmp$vN_height_nc.pgs -> tmp$height.pgs
## tmp$vN_education2_nc.pgs -> tmp$edu.pgs
## tmp$vN_eversmoke_nc.pgs -> tmp$smoke.pgs
## static.bigf(tmp,leg=TRUE,title="Genetically identified Euro",xl=xl)

## dev.off()
