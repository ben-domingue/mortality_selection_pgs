load(file="~/hrs/mortality/association.Rdata")
library(ipw)
df[!is.na(df$wt),]->df


fn<-function(fm,df) {
    lm(fm,df)->m1
    svyglm(fm,design = svydesign(~ 1, weights = ~ wt,data=df),family="gaussian")->m2
    print(coef(m2)[2]/coef(m1)[2])
    list(summary(m1)$coef,summary(m2)$coef)
}
mod<-list()
fn("alz~vN_alz_nc.pgs+ragender+rabyear",df)->mod$alz
fn("srh~vN_alz_nc.pgs+ragender+rabyear",df)->mod$srh
fn("srh~vN_alz_nc.pgs+ragender+rabyear+raedyrs",df)->mod$srh.edu

library(foreign)
read.dta("/hrsshare/rand-N/rndhrs_n.dta")->x
grep("cogtot",names(x))->index
x[,index]->tmp
apply(tmp,1,min,na.rm=TRUE)->m
apply(tmp,1,max,na.rm=TRUE)->M
ifelse(is.finite(m),m,NA)->m
ifelse(is.finite(M),M,NA)->M
data.frame(hhidpn=x$hhidpn,cog.decline=m-M) -> tmp
merge(df,tmp)->df

fn("cog.decline~vN_alz_nc.pgs+ragender+rabyear",df)->mod$cog.decline

