load("~/hrs/mortality/all.Rdata")


#special stuff
ifelse(df$radyear<2006,1,0)->df$early.death
ifelse(is.na(df$early.death),0,df$early.death)->df$early.death
abs(1-df$early.death)->df$no.death
#

ifelse(is.na(df$radyear),2,df$no.death)->df$death
fun<-function(x,var) {
    nms<-list(raedyrs="Education",
              bmi="BMI",
              height="Height",
              smoke="Ever smoker",
              cesd="CESD",
              diab="Diabetes",
              heart="Heart Disease",
              srh="Self-reported health (5-poor)"
              )
    x[df$rabyear %in% 1910:1959,]->x
    L<-list()
    #x[x$death==0,]->L[[1]] #died early
    #x[x$death>0,]->L[[2]] #lived through 2006
                                        #x[x$death==2,]->L[[3]] #still around
    x[x$no.death==0,]->L[[1]]
    x[x$no.death==1,]->L[[2]]
    infun<-function(x,var) {
        split(x,x$rabyear)->x
        f<-function(x,var) c(unique(x$rabyear),mean(x[[var]],na.rm=TRUE))
        lapply(x,f,var=var)->tmp
        do.call("rbind",tmp)
    }
    tab<-lapply(L,infun,var=var)
    ##plot(NULL,xlim=c(1910,1959),ylim=range(c(tab[[1]][,2],tab[[2]][,2],tab[[3]][,2])))
    plot(NULL,xlim=c(1910,1959),ylim=range(c(tab[[1]][,2],tab[[2]][,2])),ylab=nms[[var]],xlab="Birth year")
    #mtext(side=3,line=0,var)
    lines(tab[[1]],col="black",lwd=1,lty=2)
    lines(tab[[2]],col="darkgray",lwd=1,lty=2)
    f2<-function(x) {
        x[x[,1] %in% 1919:1955,]->x
        loess(x[,2]~x[,1])->mod
        predict(mod,se=TRUE)->pred
        cbind(names(pred$fit),pred$fit,pred$se.fit)->foo
        apply(foo,2,as.numeric)
    }
    lapply(tab,f2)->tab
    tab[[1]]->z
    lines(z[,1],z[,2],col="black",lwd=2)
    col2rgb("black")[,1]->col
    polygon(c(z[,1],rev(z[,1])),c(z[,2]-1.96*z[,3],rev(z[,2]+1.96*z[,3])),col=rgb(col[1],col[2],col[3],alpha=.15))
    tab[[2]]->z
    lines(z[,1],z[,2],col="darkgray",lwd=2)
    col2rgb("darkgray")[,1]->col
    polygon(c(z[,1],rev(z[,1])),c(z[,2]-1.96*z[,3],rev(z[,2]+1.96*z[,3])),col=rgb(col[1],col[2],col[3],m=255,alpha=.15))
                                        #lines(tab[[3]],col="blue")
}
vars<-c("raedyrs","bmi","height","smoke","cesd","diab","heart","srh")

png(paste("/tmp/fig3.png",sep=""),units="in",height=8,width=6,res=150,pointsize=13)
par(mfrow=c(4,2),mar=c(3,4,0.5,0.5),mgp=c(1.5,.75,0),oma=rep(.5,4))
for (i in 1:length(vars)) fun(df,vars[i])
legend("bottomleft",bty="n",c("Not genotyped","Genotyped"),lwd=2,lty=1,col=c("black","darkgray"))
dev.off()

       
