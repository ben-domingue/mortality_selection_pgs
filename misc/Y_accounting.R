## library(foreign)
## read.dta("/hrsshare/rand-N/rndhrs_n.dta")->x
## x[,c("hhidpn","radyear",paste("r",1:11,"iwendy",sep=""))]->df

## ##first interview
## grep("r[[:digit:]]+iwendy",names(x))->int.years
## x[,int.years]->tmp
## fun<-function(x) {
##     !is.na(x)->x2
##     x[which(x2)[1]]
## }
## apply(tmp,1,fun)->df$year.first.interview

## ##inst
## ##proxy
## for (i in 1:11) {
##     paste("r",i,"wtresp",sep="")->nm
##     x[,nm]->z
##     ifelse(z==0,1,0)->df[[paste("nh_",i,sep="")]]
##     paste("r",i,"proxy",sep="")->nm
##     x[,nm]->z
##     ifelse(z=="0.not proxy",1,0)->df[[paste("proxy_",i,sep="")]]
## }

## out<-list()
## for (i in 1:11) {
##     df[,c("radyear",paste("r",i,"iwendy",sep=""),paste("nh_",i,sep=""),paste("proxy_",i,sep=""))]->tmp
##     tmp[!is.na(tmp[,2]),]->tmp
##     nrow(tmp)->n1
##     table(tmp[,3:4])->tab
##     tab[2,2]->n.nh
##     tab[1,1]->n.pr
##     tab[2,1]->n.both
##     c(n1,n.nh,n.pr,n.both)->out[[i]]
## }
## do.call("rbind",out)->tab
