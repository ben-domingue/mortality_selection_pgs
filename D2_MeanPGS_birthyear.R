#these are the various mortality models
load(file="~/hrs/mortality/df.Rdata")
df[df$white==1,]->df
df[df$rabyear %in% 1910:1959,]->df
df[!is.na(df$vN_education2_nc.pgs),]->df
split(df,df$gender)->dfL


pf<-function(z,ylim=c(-.55,.55),txt,CI=TRUE,nms) {
    nice.nms<-list(BMI="vN_bmi_nc.pgs",Height="vN_height_nc.pgs",Education="vN_education2_nc.pgs",Smoke="vN_eversmoke_nc.pgs",Attainment="raedyrs")
    max(ylim)->M
    #par(mfrow=c(2,3),mgp=c(2.5,1,0),mar=c(0,0,0,0),oma=c(2,2,4,1))
    gray(seq(.1,.9,length.out=length(pairs)))->cols
    for (i in 1:length(z)) {
        plot(NULL,xlim=c(1910,1959),xlab="",ylab="",ylim=ylim,xaxt="n",yaxt="n",main="")
        if (txt=="female") mtext(side=2,line=2,"Mean PGS")
        if (i ==length(z)) axis(side=1)
        if (i==1) mtext(side=3,fancy.nms[[txt]],line=.3)
        if (i==length(z)) mtext(side=1,line=2.1,"Birth year")
                                        #if (i %in% c(3,6) ) axis(side=4)
        #if (i %in% c(1,4) ) axis(side=2)
        axis(side=2)
        z[[i]]->tmp
        lines(tmp,lwd=1,lty=2,col="black")
        names(z)[i]->nm
        #mtext(side=4,at=as.numeric(tmp[nrow(tmp),2]),names(z)[i],line=0.2,las=2)
        text(1938,.85*M,names(nice.nms)[which(unlist(nice.nms)==nm)],cex=1.3)
        if (CI) {
            tmp[tmp[,1] %in% 1919:1955,]->tmp
            lm(tmp[,2]~tmp[,1])->mod
            predict(mod,interval="confidence")->tab
            polygon(c(tmp[,1],rev(tmp[,1])),c(tab[,2],rev(tab[,3])),col=rgb(0.83,0.83,0.83,alpha=.55))
            lines(tmp[,1],tab[,1],lwd=2,col="black")
        }
    }
}


fancy.nms<-list(male=paste("Males, n=",nrow(dfL$male),sep=""),
                female=paste("Females, n=",nrow(dfL$female),sep="")
                )
                                        #png("/tmp/land_pgs.png",units="in",height=10,width=8,res=100)
par(mfcol=c(4,2),mgp=c(2.1,1,0),mar=c(0,1,0,1),oma=c(3.5,3.5,2,1))
for (i in 1:length(dfL)) {
    dfL[[i]]->df
    split(df,df$rabyear)->Y
    nms<-c("vN_bmi_nc.pgs","vN_height_nc.pgs","vN_education2_nc.pgs","vN_eversmoke_nc.pgs")
    ## mean pgs
    fun<-function(x,nm) c(unique(x$rabyear),mean(x[[nm]],na.rm=TRUE))
    z<-list()
    for (nm in nms) {
        lapply(Y,fun,nm=nm)->tmp
        do.call("rbind",tmp)->z[[nm]]
    }
    as.character(unique(df$ragender))->txt
    gsub("[12].","",txt)->txt
    pf(z,txt=txt,nms=names(z),ylim=c(-.5,.5))
}
#dev.off()


df[!is.na(df$vN_education2_nc.pgs),]->tmp
density(tmp$rabyear)->den
par(mgp=c(2,1,0))
plot(den,xlim=c(1910,1960),main="",xlab="Birth year")
which.min(abs(den$x-1919))->i1
which.min(abs(den$x-1955))->i2
segments(den$x[i1],0,den$x[i1],den$y[i1],lty=2)
segments(den$x[i2],0,den$x[i2],den$y[i2],lty=2)



## #####################################################
##                                         #no gender split

## pf<-function(z,ylim=c(-.55,.55),txt,CI=TRUE,nms) {
##     nice.nms<-list(BMI="vN_bmi_nc.pgs",Height="vN_height_nc.pgs",Education="vN_education2_nc.pgs",Smoke="vN_eversmoke_nc.pgs")
##     max(ylim)->M
##     #par(mfrow=c(2,3),mgp=c(2.5,1,0),mar=c(0,0,0,0),oma=c(2,2,4,1))
##     gray(seq(.1,.9,length.out=length(pairs)))->cols
##     for (i in 1:length(z)) {
##         plot(NULL,xlim=c(1919,1955),xlab="",ylab="",ylim=ylim,xaxt="n",yaxt="n",main="")
##         axis(side=1)
##         if (i==1) mtext(side=3,txt,line=.3)
##         #if (i %in% c(3,6) ) axis(side=4)
##         #if (i %in% c(1,4) ) axis(side=2)
##         axis(side=2)
##         z[[i]]->tmp
##         lines(tmp,lwd=2)
##         names(z)[i]->nm
##         #mtext(side=4,at=as.numeric(tmp[nrow(tmp),2]),names(z)[i],line=0.2,las=2)
##         text(1938,.85*M,names(nice.nms)[which(unlist(nice.nms)==nm)],cex=1.3)
##         if (CI) {
##             lm(tmp[,2]~tmp[,1])->mod
##             predict(mod,interval="confidence")->tab
##             lines(tmp[,1],tab[,1],lwd=1)
##             polygon(c(tmp[,1],rev(tmp[,1])),c(tab[,2],rev(tab[,3])),col=rgb(0.83,0.83,0.83,alpha=.55))
##         }
##     }
## }
## png("/tmp/land_pgs_all.png",units="in",height=10,width=8,res=100)
## par(mfcol=c(2,2),mgp=c(2.1,1,0),mar=c(3,3,0,1),oma=c(2,2,2,1))
## df.orig->df
## split(df,df$rabyear.raw)->Y
## nms<-c("vN_bmi_nc.pgs","vN_height_nc.pgs","vN_education2_nc.pgs","vN_eversmoke_nc.pgs")
## ## mean pgs
## fun<-function(x,nm) c(unique(x$rabyear.raw),mean(x[[nm]],na.rm=TRUE))
## z<-list()
## for (nm in nms) {
##     lapply(Y,fun,nm=nm)->tmp
##     do.call("rbind",tmp)->z[[nm]]
## }
## pf(z,txt="",nms=names(z),ylim=c(-.25,.25))
## dev.off()
