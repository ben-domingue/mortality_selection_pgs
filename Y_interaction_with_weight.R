## But would we gain any new information by using the IPW (weights) shown in Figure A7 among the genotyped group in which you estimated models like those in table 5 but included an interaction between PGS*IPW. If the effect of the PGS on the phenotype is independent of selection then this coefficient should be zero. If we can show adequate power it would give us yet another piece of evidence that it might matter. Is that worth considering.

#this applies ipw


load(file="~/hrs/mortality/association.Rdata")

df$vN_bmi_nc.pgs -> df$bmi.pgs
df$vN_height_nc.pgs -> df$height.pgs
df$vN_education2_nc.pgs -> df$edu.pgs
df$vN_eversmoke_nc.pgs -> df$smoke.pgs

f<-function(y,x) {
    paste(y,"~",x,"",sep="")->fm1
    paste(y,"~",x,"*wt.byear",sep="")->fm2
    paste(y,"~",x,"*wt.health",sep="")->fm3
    lm(fm1,df)->m1
    lm(fm2,df)->m2
    lm(fm3,df)->m3
    rbind(summary(m1)$coef,summary(m2)$coef,summary(m3)$coef)
}
f("bmi","bmi.pgs")
f("height","height.pgs")
f("raedyrs","edu.pgs")
f("smoke","smoke.pgs")

    
