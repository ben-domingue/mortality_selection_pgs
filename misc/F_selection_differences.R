## load("~/hrs/mortality/all.Rdata")
## df[df$rabyear %in% 1910:1959,]->df


## #special stuff
## ifelse(df$radyear<2006,1,0)->df$early.death
## ifelse(is.na(df$early.death),0,df$early.death)->df$early.death
## abs(1-df$early.death)->df$no.death
## #

## ifelse(df$ragender=="1.male","Male","Female")->df$ragender
## ifelse(df$white==1,"White","Non-white")->df$white
## split(df,list(df$ragender,df$white))->dfL

## pic.wrapper<-function(nm,dfL) {
##     #"geno~(raedyrs+bmi+height+smoke+cesd+diab+heart+alz+srh)*rabyear"->fm.baseline
##     "no.death~raedyrs+bmi+height+smoke+cesd+diab+heart+alz+srh+rabyear"->fm.baseline
##     #
##     dfL[[nm]]->df
##     formula(fm.baseline)->fm
##     df[,all.vars(fm)]->tmp
##     df[rowSums(is.na(tmp))==0,]->df
##     glm(fm,df,family="binomial")->mod
##     print(mod$aic)
##     fitted(mod)->df$fitted
##     pf<-function(tmp,nm) {
##         density(tmp$fitted[tmp$no.death==1],from=0,to=1,n=10000)->d1
##         density(tmp$fitted[tmp$no.death==0],from=0,to=1,n=10000)->d0
##         #integrate difference
##         mean(diff(d1$x))->dx
##         sum(abs(d1$y-d0$y)*dx)->int
##         #
##         plot(d1,type="l",col="red",xlim=c(0,1),main=nm,xlab="Pr of Selection into Genetic Sample",sub=format(int,digits=4))
##         lines(d0,type="l",col="black")
##         int
##     }
##     par(mar=c(4,3,1.5,0.5),mgp=c(1.5,.6,0),oma=c(.5,.5,1.5,.5))
##     layout(matrix(c(1,1,2,3,4,5,6,7),4,2,byrow=TRUE))
##     df[df$racohbyr!="0.not in any cohort",]->df
##     as.character(df$racohbyr)->df$racohbyr
##     pf(df,nm="All")
##     split(df,df$racohbyr)->df
##     int<-numeric()
##     for (i in 1:length(df)) pf(df[[i]],nm=names(df)[i])->int[i]
##     names(df)->names(int)
##     mtext(side=3,nm,cex=1.2,outer=TRUE,line=0)
##     #print(summary(mod))
##     int
## }

## int<-list()
## for (i in 1:length(dfL)) {
##     png(paste("/tmp/pr-select-",i,".png",sep=""),units="in",height=10,width=8,res=150,pointsize=13)
##     pic.wrapper(names(dfL)[i],dfL)->int[[names(dfL)[i] ]]
##     dev.off()
## }

## ## col<-c("black","red","blue","green")
## ## do.call("rbind",int)->tab
## ## plot(NULL,xlim=c(1,6),ylim=c(0,.8),xlab="Birth Cohort",ylab="Area between curves")
## ## for (i in 1:4) {
## ##     nrow(tab)->n
## ##     lines(1:n,tab[i,],type="c",col=col[i])
## ##     text(1:n,tab[i,],names(dfL)[i],cex=.7,col=col[i])
## ## }

## png("/tmp/pr-select-changes.png",units="in",height=5,width=6,res=150,pointsize=13)
## par(mar=c(3,4,1.5,0.5),mgp=c(1.5,.75,0),oma=rep(.5,4))
## do.call("cbind",int)->tab
## gray(seq(.3,.9,length.out=nrow(tab)))->cols
## barplot(tab,beside=TRUE,cex.names=.7,col=cols)
## legend("topleft",cex=.7,bty="n",rownames(tab),col=cols,lty=1,lwd=4)
## dev.off()


