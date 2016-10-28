setwd("/hrsshare/herit")
library(foreign)
#read.table("/hrsshare/cleaned-N/hrs_geno_final.fam")->geno
read.table("/hrsshare/HRS/GenotypeFiles/phg000207.v1.CIDR_HRS.genotype-calls-matrixfmt.c1/subject_level_filtered_PLINK_set/CIDR_HRS_Top_subject_level_filtered.fam")->geno
read.dta("/hrsshare/HRS/files_link/HRS_2006_2008_phs000428_Xref.dta")->link2
read.table("/hrsshare/HRS/subjectID2scanID.txt",header=TRUE)->link1
as.numeric(link2$LOCAL_ID)->link2$local.subjectID
merge(link1,link2,by="local.subjectID",all=TRUE)->link
as.character(as.numeric(link$HHID)*1000+as.numeric(link$PN))->link$hhidpn
#merge link and geno
NULL->geno[,6]
names(geno)<-c("family","subjectID","paternalID","maternalID","sex.plink")
merge(geno,link)->ids

read.dta("/hrsshare/rand-N/rndhrs_n.dta")->x

##institutionalized
grep("r[[:digit:]]+wtresp",names(x))->index
x[,index]->tmp
apply(tmp,2,function(x) sum(x==0,na.rm=TRUE))


#
x[,c("hhidpn","rabyear","r11iwendy","r1iwendy","radyear","racohbyr","ragender","rahispan","raracem","raestrat")]->df
df$hhidpn %in% link$hhidpn -> df$geno
df$radyear-df$rabyear -> df$age.death
ifelse(!is.na(df$radyear),1,0)->df$dead
ifelse(df$dead==1,df$age.death,df$r11iwendy-df$rabyear)->df$age
df$r1iwendy-df$rabyear->df$age.w1

grep("r[[:digit:]]+iwendy",names(x))->int.years
x[,int.years]->tmp
fun<-function(x) {
    !is.na(x)->x2
    x[which(x2)[1]]
}
apply(tmp,1,fun)->df$year.first.interview
df$year.first.interview-df$rabyear -> df$age.first.interview
fun<-function(x) {
    !is.na(x)->x2
    which(x2)->y
    x[[max(y)]]
}
apply(tmp,1,fun)->df$year.last.interview

df->hold
rm("df")

fun<-function(x) {
    options(warnings=-10)
    as.numeric(substr(x,1,1))->x
    ifelse(x>1,NA,x)->x
    ifelse(!is.finite(x),NA,x)->x
    x
}
#education
x[,c("hhidpn","raedyrs")]->df
#bmi
grep("r[1234567]bmi$",names(x))->index
x[,index]->tmp
rowMeans(tmp,na.rm=TRUE)->df$bmi
apply(tmp,1,max,na.rm=TRUE)->z
ifelse(!is.finite(z),NA,z)->df$max.bmi
##height
grep("r[1234567]height$",names(x))->index
x[,index]->tmp
apply(tmp,1,max,na.rm=TRUE)->tmp
tmp->df$height
#eversmoke
grep("r[1234567]smokev$",names(x))->index
x[,index]->tmp
apply(tmp,2,fun)->tmp
apply(tmp,1,max,na.rm=TRUE)->df$smoke
#diabetes
grep("r[1234567]diab$",names(x))->index
x[,index]->tmp
apply(tmp,2,fun)->tmp
apply(tmp,1,max,na.rm=TRUE)->df$diab
#cvd
grep("r[1234567]heart$",names(x))->index
x[,index]->tmp
apply(tmp,2,fun)->tmp
apply(tmp,1,max,na.rm=TRUE)->df$heart
#psychiatric disorder
grep("r[1234567]psyche$",names(x))->index
x[,index]->tmp
apply(tmp,2,fun)->tmp
apply(tmp,1,max,na.rm=TRUE)->df$psych
#cesd
grep("r[1234567]cesd$",names(x))->index
x[,index]->tmp
rowMeans(tmp,na.rm=TRUE)->df$cesd
#alz
grep("r[1234567]memry$",names(x))->index
#grep("r10alzhe$",names(x))->i2
#grep("r11alzhe$",names(x))->i3
#c(i1,i2,i3)->index
x[,index]->tmp
apply(tmp,2,fun)->tmp
apply(tmp,1,max,na.rm=TRUE)->df$alz

                                        #N children
grep("h[1234567]child$",names(x))->index
x[,index]->tmp
apply(tmp,1,max,na.rm=TRUE)->tmp
tmp->df$n.kids
#
grep("r[1234567]shlt$",names(x))->index
x[,index]->tmp
apply(tmp,2,function(x) substr(x,1,1))->tmp
apply(tmp,2,as.numeric)->tmp
rowMeans(tmp,na.rm=TRUE)->tmp
tmp->df$srh

for (i in 1:ncol(df)) ifelse(!is.finite(df[,i]),NA,df[,i])->df[,i]
merge(hold,df)->df
ifelse(df$ragender=="1.male","male","female")->df$gender
ifelse(df$geno,1,0)->df$geno
ifelse(df$rahispan=="0. not hispanic" & df$raracem=="1.white/caucasian",1,0)->df$white
df[df$year.first.interview<2006,]->df

save(df,file="~/hrs/mortality/all.Rdata")




