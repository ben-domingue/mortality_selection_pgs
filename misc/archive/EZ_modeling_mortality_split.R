## #split by race and gender
## load("~/hrs/mortality/all.Rdata")
## df[df$rabyear %in% 1910:1960,]->df
## ifelse(df$white==1,"wh","not")->df$white
## split(df,df$ragender)->tmp
## for (i in 1:length(tmp)) {
##     tmp[[i]]->z
##     (z$height-mean(z$height,na.rm=TRUE))/sd(z$height,na.rm=TRUE)->z$height
##     z->tmp[[i]]
## }
## data.frame(do.call("rbind",tmp))->df
## #as.character(df$racohbyr)->df$racohbyr
## #df[df$racohbyr!="0.not in any cohort",]->df
## #df[df$racohbyr!="1.ahead",]->df
## #df[df$racohbyr!="6.mid babyboomers",]->df
## split(df,list(df$ragender,df$white))->dfL


## ############################################################
## #First make the main picture
## mod.fun<-function(mod.nm,fm,dfL) {
##     dfL[[mod.nm]]->df
##     formula(fm)->fm
##     df[,all.vars(fm)]->tmp
##     df[rowSums(is.na(tmp))==0,]->df
##     glm(fm,df,family="binomial")->mod
##     fitted(mod)->df$fitted
##     library(pscl)
##     pR2(mod)[4]->mcfad.r2
##     pf<-function(tmp,nm) {
##         density(tmp$fitted[tmp$geno==1],from=0,to=1,n=10000)->d1
##         density(tmp$fitted[tmp$geno==0],from=0,to=1,n=10000)->d0
##         #integrate difference
##         mean(diff(d1$x))->dx
##         sum(abs(d1$y-d0$y)*dx)->int
##         #
##         plot(d1,type="l",col="red",xlim=c(0,1),main=nm,xlab="Pr of Selection into Genetic Sample",sub=format(int,digits=4))
##         lines(d0,type="l",col="black")
##         int
##     }
##     par(mar=c(4,3,1.5,0.5),mgp=c(1.5,.6,0),oma=c(.5,.5,1.5,.5))
##     if (length(unique(df$racohbyr))>4) {
##         layout(matrix(c(1,1,2,3,4,5,6,7),4,2,byrow=TRUE))
##     } else {
##         layout(matrix(c(1,1,2,3,4,5),3,2,byrow=TRUE))
##     }    
##     pf(df,nm="All")
##     split(df,df$racohbyr)->df
##     int<-numeric()
##     for (i in 1:length(df)) pf(df[[i]],nm=names(df)[i])->int[i]
##     mtext(side=3,mod.nm,cex=1.2,outer=TRUE,line=0)
##     print(summary(mod))
##     int
## }

## #png("/tmp/pr-select.png",units="in",height=10,width=8,res=150,pointsize=13)
## pdf("/tmp/pr-select.pdf")
## "geno~(raedyrs+bmi+height+smoke+cesd+diab+heart+alz+srh)*rabyear"->fm.baseline
## lapply(names(dfL),mod.fun,dfL=dfL,fm=fm.baseline)->int
## dev.off()

## col<-c("black","red","blue","green")
## do.call("rbind",int)->tab
## plot(NULL,xlim=c(1,6),ylim=c(0,.8),xlab="Birth Cohort",ylab="Area between curves")
## for (i in 1:4) {
##     nrow(tab)->n
##     lines(1:n,tab[i,],type="c",col=col[i])
##     text(1:n,tab[i,],names(dfL)[i],cex=.7,col=col[i])
## }
