load("~/hrs/mortality/all.Rdata")

df[!is.na(df$radyear),]->tmp
tmp[tmp$racohbyr=="3.hrs",]->tmp
tmp[tmp$radyear<2006,]->z1
tmp[tmp$radyear>=2006,]->z2
cor(z1$smoke,z1$srh,use='p')
cor(z2$smoke,z2$srh,use='p')

#special stuff
ifelse(df$radyear<2006,1,0)->df$early.death
ifelse(is.na(df$early.death),0,df$early.death)->df$early.death
abs(1-df$early.death)->df$no.death
#


##ifelse(df$geno==1,"gen-yes","gen-no")->df$geno
dim(df)
table(df$racohbyr)
table(df$racohbyr,df$geno,useNA="always")
paste(df$racohbyr,df$geno)->gr
aggregate(df$rabyear,list(gr),mean,na.rm=TRUE)
aggregate(df$rabyear,list(gr),sd,na.rm=TRUE)
by(df$rabyear,df$geno,mean,na.rm=TRUE)
by(df$rabyear,df$geno,sd,na.rm=TRUE)
by(df$white,df$racohbyr,mean,na.rm=TRUE)->M

c(list(all=df),split(df,df$racohbyr))->tmp

table(df$radyear,df$geno,useNA="ifany")->tab
dump("tab","")
nrow(tab)->N
#png("~/Downloads/deaths.png",units="in",height=10,width=8,res=150,pointsize=13)
#barplot(t(tab[-N,]),beside=TRUE,col=c("black","gray"),ylab="# Deaths",xlab="Death Year")
#legend("topright",bty="n",lty=1,lwd=4,col=c("black","gray"),c("Non-Genotyped","Genotyped"))
#dev.off()


fun<-function(x) {
    #
    nrow(x)->N
    100*sum(x$geno,na.rm=TRUE)/N->per.geno
    infun<-function(var,lev) {
        length(var)->N
        100*sum(var==lev,na.rm=TRUE)/N
    }
    infun(x$gender[x$geno==1],"female")->pf1
    infun(x$gender[x$geno==0],"female")->pf0
    infun(x$rahispan[x$geno==1],"1. hispanic")->ph1
    infun(x$rahispan[x$geno==0],"1. hispanic")->ph0
    infun(x$raracem[x$geno==1],"2.black/african american")->pb1
    infun(x$raracem[x$geno==0],"2.black/african american")->pb0
    infun(x$raracem[x$geno==1],"3.other")->po1
    infun(x$raracem[x$geno==0],"3.other")->po0
    c(N,per.geno,pf1,pf0,ph1,ph0,pb1,pb0,po1,po0)
}
lapply(tmp,fun)->tab
do.call("rbind",tab)->tab1


## fun<-function(x) {
##     vars<-c("raedyrs","bmi","height","smoke","cesd","diab","heart","alz","srh")
##     x[,vars]->tmp
##     colMeans(tmp,na.rm=TRUE)->cm
##     x[x$no.death==1,]->tmp
##     colMeans(tmp[,vars],na.rm=TRUE)->cm1
##     x[x$no.death==0,]->tmp
##     colMeans(tmp[,vars],na.rm=TRUE)->cm0
##     cm1/cm0->rat
##     paste(format(cm1,digits=2),format(rat,digits=2),sep="/")
## }
## lapply(tmp,fun)->tab
## do.call("rbind",tab)->tab2

vars<-c("raedyrs","bmi","height","smoke","cesd","diab","heart","alz","srh")
df[,vars]->tmp2
apply(tmp2,2,function(x) table(is.na(x)))

#same as above tab2, but this time separately
fun<-function(x) {
    vars<-c("raedyrs","bmi","height","smoke","cesd","diab","heart","alz","srh")
    x[,vars]->tmp
    colMeans(tmp,na.rm=TRUE)->cm
    x[x$no.death==1,]->tmp
    colMeans(tmp[,vars],na.rm=TRUE)->cm1
    x[x$no.death==0,]->tmp
    colMeans(tmp[,vars],na.rm=TRUE)->cm0
    cm1/cm0->rat
    list(formatC(cm,digits=2,format="f"),formatC(rat,digits=2,format="f"))
}
lapply(tmp,fun)->tab
do.call("rbind",sapply(tab,"[",1))->tab2a
do.call("rbind",sapply(tab,"[",2))->tab2b


#make a figure showing differences between those who died and those who did not.
library(gplots)
#split(df,df$racohbyr)->tmp
png("/tmp/mort.png",units="in",height=10,width=8,res=150,pointsize=13)
layout(matrix(c(1,1,2,3,4,5,6,7,8,9),5,2,byrow=TRUE))
par(mar=c(3,4,1.5,0.5),mgp=c(1.5,.75,0),oma=rep(.5,4))
for (nm in c("rabyear","raedyrs","bmi","height","smoke","cesd","diab","heart","alz")) {
    fun<-function(x,nm) {
        split(x,list(ifelse(x$no.death==0,"D<2006","D>=2006"),x$gender))->tmp
        infun<-function(x,nm) mean(x[[nm]],na.rm=TRUE)
        sapply(tmp,infun,nm=nm)->tr1
        #
        infun<-function(x,nm) sd(x[[nm]],na.rm=TRUE)/sqrt(sum(!is.na(x[[nm]])))
        sapply(tmp,infun,nm=nm)->z
        tr1-1.96*z->tr2
        tr1+1.96*z->tr3
        list(tr1,tr2,tr3)
    }
    #
    fun(df,nm=nm)->tab
    do.call("rbind",tab)->tab
    tab[2,]->cil
    tab[3,]->ciu
    tab[1,]->tab
    ##
    range(c(cil,ciu))->ran
    ran[1]-.1*ran[1]->ran[1]
    ran[2]+.1*ran[2]->ran[2]
    barplot2(tab,beside=TRUE,ylim=ran,xpd=FALSE,ylab=nm,col=gray(seq(.5,.9,length.out=4)),
             cex.names=.8,names.arg=names(tab),
             plot.ci=TRUE,ci.l=cil,ci.u=ciu
             )
}
dev.off()


## as.character(df$racohbyr)->df$racohbyr
## df[df$racohbyr!="0.not in any cohort",]->z
## ifelse(z$white==1,"wh","not")->z$white
## split(z,z$racohbyr)->tmp
## fun<-function(x) {
##     ftable(x$white,x$gender,x$geno)->tab
##     tab/nrow(x)
## }
## lapply(tmp,fun)->tmp
## #
## png("/tmp/sample-composition.png",units="in",height=10,width=8,res=150,pointsize=13)
## par(mfrow=c(2,1),mar=c(3,4,1.5,0.5),mgp=c(1.5,.75,0),oma=rep(.5,4))
## ##get gender #s
## fun<-function(x) {
##     x[1,1]+x[3,1]->ng.f
##     x[1,2]+x[3,2]->g.f
##     x[2,1]+x[4,1]->ng.m
##     x[2,2]+x[4,2]->g.m
##     list(ng.f=ng.f,g.f=g.f,ng.m=ng.m,g.m=g.m)->tab
##     unlist(tab)
## }
## lapply(tmp,fun)->tab
## do.call("rbind",tab)->tab
## plot(NULL,xlim=c(1,6),ylim=c(0,.8),xlab="Birth Cohort",ylab="% Within Cohort",main="Sex (m=male, f=female)")
## for (i in 1:4) {
##     lines(1:6,tab[,i],type="c")
##     text(1:6,tab[,i],colnames(tab)[i])
## }
## #
## #get race #s
## fun<-function(x) {
##     x[1,1]+x[2,1]->ng.nw
##     x[1,2]+x[2,2]->g.nw
##     x[3,1]+x[4,1]->ng.w
##     x[3,2]+x[4,2]->g.w
##     list(ng.nw=ng.nw,g.nw=g.nw,ng.w=ng.w,g.w=g.w)->tab
##     unlist(tab)
## }
## lapply(tmp,fun)->tab
## do.call("rbind",tab)->tab
## plot(NULL,xlim=c(1,6),ylim=c(0,.8),xlab="Birth Cohort",ylab="% Within Cohort",main="Race (w=white, nw=nonwhite)")
## for (i in 1:4) {
##     lines(1:6,tab[,i],type="c")
##     text(1:6,tab[,i],colnames(tab)[i])
## }
## dev.off()

## #now trying to redo with a barchart
## #
## png("/tmp/sample-composition.png",units="in",height=10,width=8,res=150,pointsize=13)
## par(mfrow=c(2,1),mar=c(3,4,1.5,0.5),mgp=c(1.5,.75,0),oma=rep(.5,4))
## ##get gender #s
## fun<-function(x) {
##     x[1,1]+x[3,1]->ng.f
##     x[1,2]+x[3,2]->g.f
##     x[2,1]+x[4,1]->ng.m
##     x[2,2]+x[4,2]->g.m
##     list("Non-genotyped female"=ng.f,"Genotyped female"=g.f,"Non-genotyped male"=ng.m,"Genotyped male"=g.m)->tab
##     unlist(tab)
## }
## lapply(tmp,fun)->tab
## do.call("rbind",tab)->tab
## gray(seq(.3,.9,length.out=nrow(tab)))->cols
## barplot(tab,beside=TRUE,cex.names=.7,col=cols)
## legend("topright",cex=.7,bty="n",rownames(tab),col=cols,lty=1,lwd=4)
## #
## #get race #s
## fun<-function(x) {
##     x[1,1]+x[2,1]->ng.nw
##     x[1,2]+x[2,2]->g.nw
##     x[3,1]+x[4,1]->ng.w
##     x[3,2]+x[4,2]->g.w
##     list("Non-genotyped NW"=ng.nw,"Genotyped NW"=g.nw,"Non-genotyped W"=ng.w,"Genotyped W"=g.w)->tab
##     unlist(tab)
## }
## lapply(tmp,fun)->tab
## do.call("rbind",tab)->tab
## gray(seq(.3,.9,length.out=nrow(tab)))->cols
## barplot(tab,beside=TRUE,cex.names=.7,col=cols)
## legend("topright",cex=.7,bty="n",rownames(tab),col=cols,lty=1,lwd=4)
## dev.off()


        

## library(gplots)
## levels(df$racohbyr)->levs
## list(all=df)->tmp
## for (lev in levs) df[df$racohbyr==lev,]->tmp[[lev]]
## #split(df,df$racohbyr)->tmp
## png("/tmp/mort.png",units="in",height=10,width=8,res=150,pointsize=13)
## par(mfrow=c(4,2),mar=c(3,4,1.5,0.5),mgp=c(1.5,.75,0),oma=rep(.5,4))
## for (nm in c("raedyrs","bmi","height","smoke","cesd","diab","heart","alz")) {
##     fun<-function(x,nm) {
##         split(x,list(x$geno,x$gender))->tmp
##         infun<-function(x,nm) mean(x[[nm]],na.rm=TRUE)
##         sapply(tmp,infun,nm=nm)->tr1
##         #
##         infun<-function(x,nm) sd(x[[nm]],na.rm=TRUE)/sqrt(sum(!is.na(x[[nm]])))
##         sapply(tmp,infun,nm=nm)->z
##         tr1-1.96*z->tr2
##         tr1+1.96*z->tr3
##         list(tr1,tr2,tr3)
##     }
##     #
##     lapply(tmp,fun,nm=nm)->tab
##     lapply(tab,"[[",1)->M
##     lapply(tab,"[[",2)->cil
##     lapply(tab,"[[",3)->ciu
##     do.call("rbind",M)->tab
##     do.call("rbind",cil)->cil
##     do.call("rbind",ciu)->ciu
##     t(tab)->tab
##     t(cil)->cil
##     t(ciu)->ciu
##     #
##     range(c(cil,ciu))->ran
##     ran[1]-.1*ran[1]->ran[1]
##     ran[2]+.1*ran[2]->ran[2]
##     barplot2(tab,beside=TRUE,ylim=ran,xpd=FALSE,ylab=nm,legend.text=nm=="alz",col=gray(seq(.5,.9,length.out=4)),names.arg=substr(colnames(tab),1,8),cex.names=.7,plot.ci=TRUE,ci.l=cil,ci.u=ciu)
## }
## dev.off()
