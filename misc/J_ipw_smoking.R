load(file="~/hrs/mortality/association.Rdata")

library(ipw)

###################################################################################
#smoking
#from smoking BG paper
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)           0.668553   0.012826   52.12   <2e-16 ***
## grs                   0.010163   0.012810    0.79    0.428    
## rabyear               0.000502   0.000542    0.93    0.354    
## ragender2.female     -0.187241   0.010393  -18.02   <2e-16 ***
## grs:rabyear           0.001017   0.000543    1.87    0.061 .  
## grs:ragender2.female  0.025846   0.010394    2.49    0.013 *  
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## Residual standard error: 0.484 on 8898 degrees of freedom
## Multiple R-squared:  0.0436,	Adjusted R-squared:  0.0431 
## F-statistic: 81.2 on 5 and 8898 DF,  p-value: <2e-16


df$vN_eversmoke_nc.pgs->df$grs
#df$racohbyr!="1.ahead" -> test1
#df$racohbyr!="6.mid babyboomers" -> test2
#df[test1 & test2,]->df.reduced
df->df.reduced

#"smoke~grs+rabyear+ragender+grs*rabyear+grs*ragender"->fm
#"smoke~grs*rabyear*ragender"->fm
"smoke~grs*rabyear+ragender"->fm
all.vars(fm)->vars
df.reduced[,vars]->tmp
df.reduced[rowSums(is.na(tmp))==0,]->df.reduced
#glm(fm,df.reduced,family="binomial")->m1
#svyglm(fm,design = svydesign(~ 1, weights = ~ wt,data=df.reduced))->m2
lm(fm,df.reduced)->m1
svyglm(fm,design = svydesign(~ 1, weights = ~ wt,data=df.reduced),family="gaussian")->m2
cbind(summary(m1)$coef,summary(m2)$coef)->tab
cbind(tab,tab[,5]/tab[,1])->tab


fun1<-function(mod,txt="") {
    plot(NULL,ylim=c(.2,.8),xlim=range(df.reduced$rabyear),xlab="Birth year",ylab="Pr of Ever smoke",xaxt="n")
    for (gen in c("1.male","2.female")) {
        for (grs in c(-1,1)) {
            data.frame(ragender=gen,grs=grs,rabyear=unique(df.reduced$rabyear))->zz
            zz[order(zz$rabyear),]->zz
            predict(mod,newdata=zz)->hi
            data.frame(zz$rabyear,hi)->hi
            hi[order(hi[,1]),]->hi
            polygon(c(hi[,1],rev(hi[,1])),c(hi[,3],rev(hi[,4])),col=rgb(matrix(rep(ifelse(gen=="1.male",.65,.85),3),1,3),alpha=.55))
            lines(hi[,1],hi[,2],lwd=3,lty=ifelse(grs==1,1,3))
        }
    }
}
png("/tmp/grs2.png",units="in",height=4,width=7,res=100)
par(mfrow=c(1,2),mgp=c(2.5,1,0),mar=c(3.5,3.5,1.2,.5))
fun1<-function(mod,txt="") {
    plot(NULL,ylim=c(.2,.8),xlim=range(df.reduced$rabyear),xlab="Birth year",ylab="Pr of Ever smoke",xaxt="n")
    for (gen in c("1.male","2.female")) {
        for (grs in c(-1,1)) {
            data.frame(ragender=gen,grs=grs,rabyear=unique(df.reduced$rabyear))->zz
            zz[order(zz$rabyear),]->zz
            predict(mod,newdata=zz)->hi
            data.frame(zz$rabyear,hi)->hi
            hi[order(hi[,1]),]->hi
            polygon(c(hi[,1],rev(hi[,1])),c(hi[,3],rev(hi[,4])),col=rgb(matrix(rep(ifelse(gen=="1.male",.65,.85),3),1,3),alpha=.55))
            lines(hi[,1],hi[,2],lwd=3,lty=ifelse(grs==1,1,3))
        }
    }
    legend("bottomleft",c("PGS = 1","PGS = -1"),lty=c(1,3),lwd=3,bty="n")
}
fun(m1)
fun2<-function(mod,txt="") {
    plot(NULL,ylim=c(.2,.8),xlim=range(df.reduced$rabyear),xlab="Birth year",ylab="Pr of Ever smoke",xaxt="n")
    for (grs in c(-1,1)) {
        data.frame(ragender="1.male",grs=grs,rabyear=unique(df.reduced$rabyear))->zz
        data.frame(ragender="2.female",grs=grs,rabyear=unique(df.reduced$rabyear))->yy
        zz[order(zz$rabyear),]->zz
        yy[order(yy$rabyear),]->yy
        data.frame(rbind(zz,yy))->zz
        predict(mod,newdata=zz,design = svydesign(~ 1, weights = ~ wt,data=df.reduced))->out
        data.frame(out)->out
        out$link->zz$fit
        out$link-1.96*out$SE->zz$lo
        out$link+1.96*out$SE->zz$hi
        for (gen in c("1.male","2.female")) {
            zz[zz$ragender==gen,]->yy
            yy[order(yy$rabyear),]->hi
            hi[,c("rabyear","fit","lo","hi")]->hi
            polygon(c(hi[,1],rev(hi[,1])),c(hi[,3],rev(hi[,4])),col=rgb(matrix(rep(ifelse(gen=="1.male",.65,.85),3),1,3),alpha=.55))
            lines(hi[,1],hi[,2],lwd=3,lty=ifelse(grs==1,1,3))
        }
    }
}
fun2(m2)
dev.off()



## split(df.reduced,df.reduced$ragender)->dfL
## fun<-function(df.reduced) {
##     smoke~grs+rabyear+grs*rabyear->fm
##     lm(fm,df.reduced)->m1
##     svyglm(fm,design = svydesign(~ 1, weights = ~ wt,data=df.reduced))->m2
##     cbind(summary(m1)$coef,summary(m2)$coef)->tab
##     tab[,5]/tab[,1]->z
##     cbind(tab,z)
## }
## lapply(dfL,fun)
