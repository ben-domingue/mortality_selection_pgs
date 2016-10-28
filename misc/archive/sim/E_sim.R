load("~/hrs/mortality/all.Rdata")
ifelse(df$ragender=="1.male","male","female")->df$gender
ifelse(df$geno,1,0)->df$geno
df[df$rabyear %in% 1910:1960,]->df

sum(df$geno)/sum(!is.na(df$geno))->rate
log(rate/(1-rate))->alpha
log(2*rate/(1-2*rate)) - alpha -> beta2 #a 1-SD increase in std normal polygenic score doubles chances of being in genetic sample for those beta2
rnorm(nrow(df))->df$sim.pgs
alpha->df$alpha
beta2->df$beta2
.3->df$betay

for (frac in c(.1,.25,.5,1,2)) {
    exp(alpha+frac*beta2*df$sim.pgs)->kern
    kern/(1+kern)->pv
    pv->df[[paste("sim.pr.",frac,sep="")]]
    rbinom(nrow(df),1,pv)->df[[paste("sim.geno.",frac,sep="")]]
}
10+df$betay*df$sim.pgs+rnorm(nrow(df))->df$sim.y



