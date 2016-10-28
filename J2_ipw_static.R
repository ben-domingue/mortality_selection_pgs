#this applies ipw


load(file="~/hrs/mortality/association.Rdata")
library(ipw)
library(survey)
library(gplots)

df$vN_bmi_nc.pgs -> df$bmi.pgs
df$vN_height_nc.pgs -> df$height.pgs
df$vN_education2_nc.pgs -> df$edu.pgs
df$vN_eversmoke_nc.pgs -> df$smoke.pgs

###################################################################################
###################################################################################
#static effects
static.bigf<-function(df,leg=TRUE,title=NULL,xl=c(0,0.425)) {
    fn<-function(fm,df) {
        m<-list()
        lm(fm,df[is.na(df$radyear),])->m$enh.mort
        lm(fm,df)->m$naive
        svyglm(fm,design = svydesign(~ 1, weights = ~ wt.health,data=df))->m$wt1
        svyglm(fm,design = svydesign(~ 1, weights = ~ wt.byear,data=df))->m$wt2
        print(length(m[[2]]$resid))
        lapply(m,function(x) summary(x)$coef[2,])->tab
        do.call("rbind",tab)
    }
    mod<-list()
    fn("bmi.std~bmi.pgs+ragender",df)->mod$BMI
    fn("height.std~height.pgs+ragender",df)->mod$Height
    fn("raedyrs.std~edu.pgs+ragender",df)->mod$Education
    fn("smoke.std~smoke.pgs+ragender",df)->mod$Smoke
    mod->mod.static
    tab<-list()
    for (nm in c("BMI","Height","Education","Smoke")) {
        mod.static[[nm]]->x
        as.numeric(t(x[,1:2]))->tab[[nm]]
                                        #c(x[1,1],x[1,2],x[2,1],x[2,2],x[2,1]/x[1,1])->tab[[nm]]
    }
    do.call("rbind",tab)->tabS
                                        #tabS[,c(1,3,5,7)]/tabS[,3]
    print((tabS[,c(1,3,5,7)]-tabS[,3])/tabS[,3])
    print(tabS[,c(1,3,5,7)]-tabS[,3])
    t(tabS[,c(1,3,5,7)])->est
    t(tabS[,1+c(1,3,5,7)])->se
    par(mgp=c(2,1,0))
    barplot2(est,horiz=TRUE,beside=TRUE,xlab="Time-invariant PGS effect",
             col=gray(seq(.9,.3,length.out=ncol(est))),
             density=c(-10,35,-10,-10),
             plot.ci=TRUE,xlim=xl,
             ci.u=est+1.96*se,
             ci.l=est-1.96*se
             )->out
    text(est+1.96*se,out,round(est,digits=3),cex=1,offset=1,pos=4)
    if (!is.null(title)) mtext(side=3,line=.2,title)
    if (leg) legend("topright",bty="n",cex=1,rev(c("Enh mortality","Naive","Wt, hlt only","Wt, hlt + byear")),fill=rev(gray(seq(.3,.9,length.out=ncol(est)))),density=rev(c(-10,35,-10,-10)))
    tabS
}

pdf("/tmp/fig.pdf")
static.bigf(df)
dev.off()

