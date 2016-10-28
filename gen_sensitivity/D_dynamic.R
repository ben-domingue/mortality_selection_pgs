
load(file="~/hrs/mortality/association.Rdata")
library(ipw)
library(survey)
library(gplots)

pdf("/tmp/fig.pdf",width=6,height=6)
par(mfcol=c(4,4),mgp=c(2,1,0),mar=c(3.1,3.1,2,1))

xl<-c(0,.55)
                                        #load static.bigf from ../J2_...
#########################################################################
df$vN_bmi_nc0.1.pgs -> df$bmi.pgs
df$vN_height_nc0.1.pgs -> df$height.pgs
df$vN_education2_nc0.1.pgs -> df$edu.pgs
df$vN_eversmoke_nc0.1.pgs -> df$smoke.pgs
dyn.bigf(df,leg=FALSE,title="0.1 threshold")

df$vN_bmi_nc0.5.pgs -> df$bmi.pgs
df$vN_height_nc0.5.pgs -> df$height.pgs
df$vN_education2_nc0.5.pgs -> df$edu.pgs
df$vN_eversmoke_nc0.5.pgs -> df$smoke.pgs
dyn.bigf(df,leg=FALSE,title="0.5 threshold")

#########################################################################
df$vN_bmi_cl.pgs -> df$bmi.pgs
df$vN_height_cl.pgs -> df$height.pgs
df$vN_education2_cl.pgs -> df$edu.pgs
df$vN_eversmoke_cl.pgs -> df$smoke.pgs
dyn.bigf(df,leg=FALSE,title="Clumped")
#########################################################################

read.table("~/hrs/swb/hwe_eur_keep.txt",header=FALSE)->ids
dim(df)
df[df$subjectID %in% ids[,2],]->tmp
dim(tmp)
tmp$vN_bmi_nc.pgs -> tmp$bmi.pgs
tmp$vN_height_nc.pgs -> tmp$height.pgs
tmp$vN_education2_nc.pgs -> tmp$edu.pgs
tmp$vN_eversmoke_nc.pgs -> tmp$smoke.pgs
dyn.bigf(tmp,leg=FALSE,title="Genetically identified Euro")

dev.off()