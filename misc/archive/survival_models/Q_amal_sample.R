load("~/hrs/mortality/all.Rdata")
df[df$rabyear %in% 1910:1959,]->df
#ifelse(df$white==1,"wh","not")->df$white


ifelse(df$ragender=="1.male","Male","Female")->df$ragender
ifelse(df$white==1,"White","Non-white")->df$white
split(df,list(df$ragender,df$white))->dfL

dfL$Female.White->df
df[,c("rabyear","radyear","year.first.interview","year.last.interview","geno","dead","age.first.interview")]->df
df$rabyear+sample(-3:3,nrow(df),replace=TRUE)->df$rabyear
df$year.last.interview+sample(-2:2,nrow(df),replace=TRUE)->df$duration.time
rbinom(nrow(df),1,ifelse(df$geno==1,.9,.1))->df$geno
rbinom(nrow(df),1,ifelse(df$dead==1,.9,.1))->df$dead
df$year.last.interview-df$year.first.interview -> df$duration.time
df[sample(1:nrow(df),1000),]->df
save(df,file="/tmp/scratch_data.Rdata")

load(file="/tmp/scratch_data.Rdata")
library(survival)
library(survey)

coxph(Surv(time=duration.time,event=dead)~geno+age.first.interview,df,control=coxph.control(iter=5000))->mod
print(mod)
plot(NULL,xlab="Age",ylab="",xlim=c(0,25),ylim=c(.5,1))    
byear<-c(1928,1938,1950)
fi<-c(55,65)
lwd<-c(2,4)
for (i in 1:length(byear)) {
    for (j in 1:length(fi)) {
        col<-rgb(c(0,1,0),c(0,0,0),c(0,0,1),alpha=1-(j-1)/2) #
        data.frame(geno=1,rabyear=byear[i],age.first.interview=fi[j])->z
        survfit(mod,newdata=z)->mod2
        lines(mod2$time,mod2$surv,lwd=lwd[j],col=col[i])
        data.frame(geno=0,rabyear=byear[i],age.first.interview=fi[j])->z
        survfit(mod,newdata=z)->mod2
        lines(mod2$time,mod2$surv,lty=2,lwd=lwd[j],col=col[i])
    }
}
for (i in seq(50,110,by=10)) abline(v=i,lty=3,lwd=.4,col="gray")
for (i in seq(0,1,by=.1)) abline(h=i,lty=3,lwd=.4,col="gray")

