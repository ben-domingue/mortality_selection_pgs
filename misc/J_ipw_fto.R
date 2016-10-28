plink --bfile /hrsshare/cleaned-N/hrs_geno_final_translated --snp rs17817449 --recodeA --out /tmp/fto

read.table("/tmp/fto.raw")->fto
fto[,c(2,7)]->fto
names(fto)<-c("subjectID","fto")
as.numeric(fto$fto)->fto$fto

load(file="~/hrs/mortality/association.Rdata")
library(survey)
library(ipw)
df[!is.na(df$wt),]->df
merge(df,fto,all.x=TRUE)->df

cor(df$bmi,df$fto,use='p')
summary(lm(bmi~fto,df))

fn<-function(fm,df) {
    lm(fm,df)->m1
    svyglm(fm,design = svydesign(~ 1, weights = ~ wt,data=df),family="gaussian")->m2
    print(coef(m2)[2]/coef(m1)[2])
    list(summary(m1)$coef,summary(m2)$coef)
}
mod<-list()
fn("bmi~fto+ragender+rabyear",df)->mod$fto
