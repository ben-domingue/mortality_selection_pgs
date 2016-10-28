## load("~/hrs/mortality/all.Rdata")

## library(foreign)
## read.dta("/hrsshare/rand-N/rndhrs_n.dta")->x
## data.frame(hhidpn=x$hhidpn)->y
## for (i in 1:11) {
##     paste("r",i,"wtresp",sep="")->nm
##     x[,nm]->z
##     ifelse(z==0,1,0)->y[[paste("nh_",i,sep="")]]
##     paste("r",i,"proxy",sep="")->nm
##     x[,nm]->z
##     ifelse(z=="0.not proxy",1,0)->y[[paste("proxy_",i,sep="")]]
## }
## merge(df[,c("hhidpn","geno")],y)->tmp

## out1<-out2<-list()
## for (i in 1:11) {
##     table(tmp$geno,tmp[[paste("nh_",i,sep="")]],useNA="ifany")->tab
##     tab[,2]/nrow(tmp)->out1[[i]]
##     table(tmp$geno,tmp[[paste("nh_",i,sep="")]],useNA="no")->tab
##     tab[,2]/sum(tab)->out2[[i]]
## }
## do.call("rbind",out1)->tab1
## do.call("rbind",out2)->tab2

## png("/tmp/inst.png",units="in",height=5,width=6,res=150,pointsize=13)
## plot(NULL,xlim=c(1,11),ylim=c(0,.15),xlab="Wave",ylab="")
## lines(tab1[,1],col="black")
## lines(tab1[,2],col="red")
## lines(tab2[,1],col="black",lty=2)
## lines(tab2[,2],col="red",lty=2)
## legend("topright",bty="n",lty=1,col=c("black","red"),c("Non-geno","Geno"))
## abline(v=8,)
## dev.off()
