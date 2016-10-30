
load(file="~/hrs/mortality/association.Rdata")
library(ipw)
library(survey)
library(gplots)


pf<-function(df) {
    matrix(c(1,1,2,3,3,4,5,5,6,7,7,8),4,3,byrow=TRUE)->m1
    matrix(c(9,9,10,11,11,12,13,13,14,15,15,16),4,3,byrow=TRUE)->m2
    layout(cbind(m1,m2))
    par(mar=c(3,3,1,.5),mgp=c(2,1,0),oma=c(1,1,1,1))
    dyn.bigf(df[df$gender=="female",])
    mtext(side=1,line=3,"Females")
    dyn.bigf(df[df$gender=="male",])
    mtext(side=1,line=3,"Males")
}


#xl<-c(0,.55)
                                        #load static.bigf from ../J2_...
#########################################################################
df$vN_bmi_nc0.1.pgs -> df$bmi.pgs
df$vN_height_nc0.1.pgs -> df$height.pgs
df$vN_education2_nc0.1.pgs -> df$edu.pgs
df$vN_eversmoke_nc0.1.pgs -> df$smoke.pgs
png("/tmp/sense1.png",units="in",height=7,width=9,res=100)
pf(df)
dev.off()

df$vN_bmi_nc0.5.pgs -> df$bmi.pgs
df$vN_height_nc0.5.pgs -> df$height.pgs
df$vN_education2_nc0.5.pgs -> df$edu.pgs
df$vN_eversmoke_nc0.5.pgs -> df$smoke.pgs
png("/tmp/sense2.png",units="in",height=7,width=9,res=100)
pf(df)
dev.off()

#########################################################################
df$vN_bmi_cl.pgs -> df$bmi.pgs
df$vN_height_cl.pgs -> df$height.pgs
df$vN_education2_cl.pgs -> df$edu.pgs
df$vN_eversmoke_cl.pgs -> df$smoke.pgs
png("/tmp/sense3.png",units="in",height=7,width=9,res=100)
pf(df)
dev.off()

#########################################################################

read.table("~/hrs/swb/hwe_eur_keep.txt",header=FALSE)->ids
dim(df)
df[df$subjectID %in% ids[,2],]->tmp
dim(tmp)
tmp$vN_bmi_nc.pgs -> tmp$bmi.pgs
tmp$vN_height_nc.pgs -> tmp$height.pgs
tmp$vN_education2_nc.pgs -> tmp$edu.pgs
tmp$vN_eversmoke_nc.pgs -> tmp$smoke.pgs
png("/tmp/sense4.png",units="in",height=7,width=9,res=100)
pf(tmp)
dev.off()
