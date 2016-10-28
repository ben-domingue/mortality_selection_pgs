read.table("~/gwas_results/IGAP_stage_1.txt",header=TRUE)->gwas
gwas[,c("MarkerName","Pvalue")]->hits
hits[hits$Pvalue<1e-8,]->th

write.table(th$MarkerName,file="/tmp/alz.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)

plink --bfile /hrsshare/cleaned-N/hrs_geno_final_translated --extract /tmp/alz.txt --recodeA --out /tmp/alz

read.table("/tmp/alz.raw",header=TRUE)->alz
alz[,-c(1,3,4,5,6)]->alz

load(file="~/hrs/mortality/association.Rdata")
library(survey)
library(ipw)
df[!is.na(df$wt),]->df

out<-list()
for (i in 2:ncol(alz)) {
    alz[,c(1,i)]->tmp
    names(tmp)<-c("subjectID","snp")
    merge(df,tmp,all.x=TRUE)->tmp
    lm(alz~snp,tmp)->m1
    svyglm(alz~snp,design = svydesign(~ 1, weights = ~ wt,data=tmp),family="gaussian")->m2
    c(summary(m1)$coef[2,c(1,4)],summary(m2)$coef[2,c(1,4)])->out[[i-1]]
}
do.call("rbind",out)->out
table(out[,2]<.05,out[,4]<.05)

pdf("/tmp/gwas.pdf")
par(mfrow=c(2,1))
plot(out[,1],out[,3]); abline(0,1)
plot(-log10(out[,2]),-log10(out[,4])); abline(0,1)
dev.off()
