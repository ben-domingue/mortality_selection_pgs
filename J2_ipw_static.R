#mod is a list of lm objects
table.lm<-function(mod) {
  lapply(mod,function(x) names(coef(x)))->nms
  unique(do.call("c",nms))->nms
  length(nms)->nr
  length(mod)->nc
  mat.est<-mat.tstat<-matrix(NA,nr,nc)
  for (j in 1:nc) {
    summary(mod[[j]])$coef->foo
    for (i in 1:nr) {
      match(nms[i],rownames(foo))->index
      if (length(index)>0) {
        foo[index,1]->mat.est[i,j]
        foo[index,2]->mat.tstat[i,j]
      }
    }
  }
  sapply(mod,function(x) length(residuals(x)))->N
  out<-list()
  new.nms<-list()
  for (i in 1:nr) {
    rbind(mat.est[i,],mat.tstat[i,])->out[[i]]
    new.nms[[i]]<-c(nms[i],paste(nms[i],".se",sep=""))
  }
  do.call("rbind",out)->out
  rbind(out,N)->out
  c(do.call("c",new.nms),"N")->rownames(out)
  out
}



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
        #print(length(m[[2]]$resid))
        print(table.lm(m))
        lapply(m,function(x) summary(x)$coef[2,])->tab
        do.call("rbind",tab)
    }
    mod<-list()
    fn("bmi.std~bmi.pgs+ragender+rabyear",df)->mod$BMI
    fn("height.std~height.pgs+ragender+rabyear",df)->mod$Height
    fn("raedyrs.std~edu.pgs+ragender+rabyear",df)->mod$Edu
    fn("smoke.std~smoke.pgs+ragender+rabyear",df)->mod$Smoke
    mod->mod.static
    tab<-list()
    for (nm in c("BMI","Height","Edu","Smoke")) {
        mod.static[[nm]]->x
        as.numeric(t(x[,1:2]))->tab[[nm]]
                                        #c(x[1,1],x[1,2],x[2,1],x[2,2],x[2,1]/x[1,1])->tab[[nm]]
    }
    do.call("rbind",tab)->tabS
                                        #tabS[,c(1,3,5,7)]/tabS[,3]
    #print((tabS[,c(1,3,5,7)]-tabS[,3])/tabS[,3])
    #print(tabS[,c(1,3,5,7)]-tabS[,3])
    t(tabS[,c(1,3,5,7)])->est
    t(tabS[,1+c(1,3,5,7)])->se
    par(mgp=c(2,1,0))
    barplot2(est,horiz=TRUE,beside=TRUE,xlab="Time-invariant PGS effect",
             col=gray(seq(.9,.3,length.out=ncol(est))),
             density=c(-10,35,-10,-10),
             plot.ci=TRUE,xlim=xl,
             ci.u=est+1.96*se,
             ci.l=est-1.96*se,cex.axis=.8
             )->out
    text(est+1.96*se,out,round(est,digits=3),cex=1,offset=1,pos=4)
    if (!is.null(title)) mtext(side=3,line=.2,title)
    if (leg) legend("topright",bty="n",cex=1,rev(c("Enh mortality","Naive","Wt, hlt only","Wt, hlt + byear")),fill=rev(gray(seq(.9,.3,length.out=ncol(est)))),density=rev(c(-10,35,-10,-10)))
    tabS[,c(1,3,5,7)]->est
    tabS[,1+c(1,3,5,7)]->se
    rbind(est[1,],se[1,],est[2,],se[2,],est[3,],se[3,],est[4,],se[4,])
}

png("/tmp/static.png",units="in",height=5,width=6,res=100)
static.bigf(df)
dev.off()


