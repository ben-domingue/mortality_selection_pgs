#load(file="~/hrs/mortality/association.Rdata")
library(ipw)
library(survey)

###################################################################################
#bmi education &E height
###################################################################################
#static effects
fn<-function(fm,df) {
    df[!is.na(df$wt),]->df
    lm(fm,df)->m1
    svyglm(fm,design = svydesign(~ 1, weights = ~ wt,data=df))->m2
    print(fm)
    print((coef(m2)[2]-coef(m1)[2])/coef(m1)[2])
    rbind(summary(m1)$coef[2,],summary(m2)$coef[2,])
}
mod<-list()
#fn("alz~vN_alz_nc.pgs+ragender",df)->mod$Alzheimers
fn("bmi.std~vN_bmi_nc.pgs+ragender",df)->mod$BMI
fn("height.std~vN_height_nc.pgs+ragender",df)->mod$Height
fn("raedyrs.std~vN_education_nc.pgs+ragender",df)->mod$Education
fn("smoke.std~vN_eversmoke_nc.pgs+ragender",df)->mod$Smoke
mod->mod.static


#dynamic effects
assoc_fun<-function(pgs,outcome,df,fm.selection) {
    df[!is.na(df$wt),]->df
    mods<-list()
    #raw
    formula(paste(outcome,".std~",pgs,"*rabyear+ragender",sep=""))->fm.pg
    lm(fm.pg,df)->mods$naive
    print(summary(mods$naive))
    #reduced sample
    df$racohbyr!="1.ahead" -> test1
    df$racohbyr!="6.mid babyboomers" -> test2
    df[test1 & test2,]->df.reduced
    lm(fm.pg,df.reduced)->mods$reduced
    #ipw
    svyglm(fm.pg,design = svydesign(~ 1, weights = ~ wt,data=df))->mods$ipw
    mods
}
fun<-function(z) {
    table.lm(z)->tab
    apply(tab,2,unlist)->tab
    do.call("cbind",tab)->tab
    tab[-nrow(tab),]->tab
    cbind(tab,tab[,2]/tab[,1],tab[,3]/tab[,1])->tab
    tab[,c(1,4,5)]
}
mod.dyn<-tab<-list()
assoc_fun(outcome="bmi",pgs="vN_bmi_nc.pgs",df)->tab$all->mod.dyn$BMI
assoc_fun(outcome="height",pgs="vN_height_nc.pgs",df)->tab$all->mod.dyn$Height
assoc_fun(outcome="raedyrs",pgs="vN_education_nc.pgs",df)->tab$all->mod.dyn$Education
assoc_fun(outcome="smoke",pgs="vN_eversmoke_nc.pgs",df)->tab$all->mod.dyn$Smoke


tab<-list()
for (nm in c("BMI","Height","Education","Smoke")) {
    mod.static[[nm]]->x
    c(x[1,1],x[1,2],x[2,1],x[2,2],x[2,1]/x[1,1])->tab[[nm]]
}
do.call("rbind",tab)->tabS

tab<-list()
for (nm in c("BMI","Height","Education","Smoke")) {
    mod.dyn[[nm]]->x
    lapply(x[c(1,3)],function(x) summary(x)$coef)->tmp
    tmp[[1]][-c(1,4),1:2]->t1
    tmp[[2]][-c(1,4),1:2]->t2
    cbind(t1,t2,t2[,1]/t1[,1])->tab[[nm]]
}
do.call("rbind",tab)->tabD




#from penetrance paper.
## [1] "Education"
## Call:
## lm(formula = y ~ pgs * t)
## Residuals:
##    Min     1Q Median     3Q    Max 
## -5.403 -0.560 -0.145  0.804  2.146 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -34.95397    2.17374  -16.08   <2e-16 ***
## pgs           4.55045    2.19495    2.07    0.038 *  
## t             0.01804    0.00112   16.08   <2e-16 ***
## pgs:t        -0.00225    0.00113   -1.99    0.047 *  
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## Residual standard error: 0.969 on 8847 degrees of freedom
##   (14 observations deleted due to missingness)
## Multiple R-squared:  0.0609,	Adjusted R-squared:  0.0606 
## F-statistic:  191 on 3 and 8847 DF,  p-value: <2e-16
