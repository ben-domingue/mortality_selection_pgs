#simple demonstration of bias
cor(df$sim.pgs,df$sim.y)
for (frac in c(.1,.25,.5,1,2)) {
    df[df[[paste("sim.geno.",frac,sep="")]]==1,]->tmp
    print(cor(tmp$sim.pgs,tmp$sim.y))
} #can compare this reduction to that which is seen in the LD score paper.

library(survey)
tab<-list()
for (frac in c(.1,.25,.5,1,2)) {
    df[df[[paste("sim.geno.",frac,sep="")]]==1,]->tmp
    paste("sim.y~sim.pgs",sep="")->fm
    lm(fm,tmp)->m1
    1/tmp[[paste("sim.pr.",frac,sep="")]]->tmp$wt
    quantile(tmp$wt,.99)->qu
    ifelse(tmp$wt>qu,qu,tmp$wt)->tmp$wt
    svyglm(fm,design = svydesign(~ 1, weights = ~ wt,data=tmp))->m2
    rbind(summary(m1)$coef[2,],summary(m2)$coef[2,])->tab[[as.character(frac)]]
}
