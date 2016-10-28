load("~/hrs/mortality/all.Rdata")

table(df$geno,df$radyear)


table(df$radyear>=2006 | is.na(df$radyear))
table(df$radyear>=2006 | is.na(df$radyear),df$geno,useNA="always")

ifelse(df$radyear>=2006 | is.na(df$radyear),1,0)->df$type
ifelse(df$geno==1,2,df$type)->df$type
table(df$type)

split(df,df$type)->dfL

library(survival)
fun<-function(df) {
    cols<-c("black","darkgray","darkgray")
    lty<-c(1,1,2)
    survfit(Surv(time=age,event=dead)~1,df,conf.int=FALSE)->mod
    lines(mod$time,mod$surv,lty=lty[unique(df$type)+1],lwd=3,col=cols[unique(df$type)+1])
    c(mod$surv+1.96*mod$std.err,rev(mod$surv-1.96*mod$std.err))->y.ci
    ifelse(!is.finite(y.ci),2,y.ci)->y.ci
    polygon(c(mod$time,rev(mod$time)),y.ci,col=rgb(211,211,211,max=255,alpha=90))
}

                                        #png("/tmp/km.png",units="in",height=6,width=8,res=150,pointsize=13)
pdf("/tmp/fig1.pdf",width=7,height=10)
par(mar=c(3,3,1.5,0.5),mgp=c(1.5,.6,0),oma=rep(.5,4))
plot(NULL,ylim=c(0,1),xlim=c(50,100),xlab="Age",ylab="",main="")
lapply(dfL,fun)
legend("topright",paste(c("Type 1","Type 2","Type 3"),as.numeric(table(df$type)),sep=", "),col=c("black","darkgray","darkgray"),lty=c(1,1,2),bty="n")
dev.off()


df[df$type>0,]->tmp
table(tmp$type,tmp$raracem)/nrow(tmp)




###################################################################################
##looking at those who lived until at least 2006 but were not genotyped.
df[df$geno==0 & (df$radyear>=2006 | is.na(df$radyear)),]->df

##sample refresh
table(df$year.first.interview>2008,df$geno)
ifelse(df$year.first.interview>2008,1,0)->df$sample.refresh

##nursing home
library(foreign)
read.dta("/hrsshare/rand-N/rndhrs_n.dta")->x
data.frame(hhidpn=x$hhidpn)->y
for (i in 1:11) {
    paste("r",i,"wtresp",sep="")->nm
    x[,nm]->z
    ifelse(z==0,1,0)->y[[paste("nh_",i,sep="")]]
    paste("r",i,"proxy",sep="")->nm
    x[,nm]->z
    ifelse(z=="1.proxy",1,0)->y[[paste("proxy_",i,sep="")]]
}
#merge(df[,c("hhidpn","geno")],y)->tmp
y[,c("hhidpn","proxy_8","proxy_9","nh_8","nh_9")]->y
rowSums(y[,c("nh_8","nh_9")],na.rm=TRUE)->y$nh
y[,c("proxy_8","proxy_9")]->tmp
ifelse(tmp[,1]==1 | tmp[,2]==1,1,0)->y$proxy
ifelse(is.na(tmp$proxy_8) & is.na(tmp$proxy_9),NA,y$proxy)->y$proxy
ifelse(is.na(y$proxy),0,y$proxy)->y$proxy
merge(df,y)->df


table(df$sample.refresh,df$nh)




df[df$sample.refresh==0 & df$nh==0,]->df1
table(df1$proxy,useNA="always")
df1[df1$proxy!=1,]->df2



##refusers
load("/tmp/geno_refusal.Rdata") #see B3_saliva_consent.R
merge(df2,gr,all.x=TRUE)->df3
ifelse(df3$KI913==5 | df3$LI913==5,1,0)->df3$refuse
ifelse(is.na(df3$refuse),0,df3$refuse)->df3$refuse
df3[df3$refuse!=1,]->df4

#not in wave 8
x[,c("hhidpn","r8wtresp","r9wtresp"),]->tmp
merge(df4,tmp)->df4
table(is.na(df4$r8wtresp),is.na(df4$r9wtresp))


