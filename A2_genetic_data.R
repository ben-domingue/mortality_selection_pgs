setwd("~/hrs/polygenic_score/scores")
list.files(pattern="vN.+profile")->lf
L<-list()
for (fn in lf) {
    read.table(fn,header=TRUE)->pr
    pr[,c(2,6)]->pr
    names(pr)<-c("subjectID",fn)
    pr->L[[fn]]
}
L[[1]]->df
for (i in 2:length(L)) merge(df,L[[i]],all=TRUE)->df

##get scores based on different p-values
setwd("~/hrs/mortality/sense/")
list.files(pattern="vN.+profile")->lf
L<-list()
for (fn in lf) {
    read.table(fn,header=TRUE)->pr
    pr[,c(2,6)]->pr
    names(pr)<-c("subjectID",fn)
    pr->L[[fn]]
}
for (i in 1:length(L)) merge(df,L[[i]],all=TRUE)->df

load("/hrsshare/hrs_linked_N.Rdata")
x[x$rahispan=="0. not hispanic" & x$raracem=="1.white/caucasian",]->x
x[,c("hhidpn","subjectID","rabyear")]->x
merge(df,x)->df
#read.table("/hrsshare/cleaned-N/hrs_le_WH.eigenvec",header=FALSE)->eig
read.table("/hrsshare/cleaned-N/hrs_pcs.eigenvec",header=FALSE)->eig
names(eig)<-c("family","subjectID",paste("pc",1:20,sep=""))
merge(df,eig)->df

##first get & standardize polygenic scores
grep("profile",names(df))->index
for (nm in names(df)[index]) {
    df[[nm]]->z
    (z-mean(z,na.rm=TRUE))/sd(z,na.rm=TRUE)->df[[paste(nm,".nopcs",sep="")]]
    lm(paste(nm,"~",paste(paste("pc",1:10,sep=""),collapse="+")),df)->mod
    mod$resid->z
    (z-mean(z,na.rm=TRUE))/sd(z,na.rm=TRUE)->df[[nm]]
}
names(df)->nms
gsub(".profile",".pgs",nms)->nms
nms->names(df)
grep(".pgs$",names(df))->index
df[,c("subjectID","hhidpn",names(df)[index]),]->scores

load(file="~/hrs/mortality/all.Rdata")
merge(df,scores,all.x=TRUE)->df
save(df,file="~/hrs/mortality/df.Rdata")
