library(HeritHelper)
setwd("/home/domingue/hrs/mortality/sense")

pfun<-function(p) {
    read.table("/tmp/GWAS2.result",header=TRUE)->x
    print(nrow(x))
    x[x[,4]<p,]->x
    print(nrow(x))
    write.table(x,file="/tmp/GWAS2.result",quote=FALSE,row.names=FALSE)
}

for (pv in c(.1,.5)) {
    ##get things in right order (snp, alleles, pvalue, effect
                                        #
    system("awk '{print $1, $2, $3, $7, $5}' ~/gwas_results/SNP_gwas_mc_merge_nogc.tbl.uniq > /tmp/GWAS2.result")
    pfun(pv)
    make_pgs(out.name=paste("vN_bmi_nc",pv,sep=""),clump=FALSE,gwas.file="/tmp/GWAS2.result",wd="/tmp/bd/",plink.file="/hrsshare/cleaned-N/hrs_geno_final_translated")
                                        #
    system("awk '{print $2, $4, $5, $11, $9}' ~/gwas_results/tag.evrsmk.tbl > /tmp/GWAS2.result")
    pfun(pv)
    make_pgs(out.name=paste("vN_eversmoke_nc",pv,sep=""),clump=FALSE,gwas.file="/tmp/GWAS2.result",wd="/tmp/bd/",plink.file="/hrsshare/cleaned-N/hrs_geno_final_translated")
                                        #
    system("awk '{print $1, $4, $5, $9, $7}' ~/gwas_results/EduYears_excl_HRS_23andMe_sumstats.txt > /tmp/GWAS2.result")
    pfun(pv)
    make_pgs(out.name=paste("vN_education2_nc",pv,sep=""),clump=FALSE,gwas.file="/tmp/GWAS2.result",wd="/tmp/bd/",plink.file="/hrsshare/cleaned-N/hrs_geno_final_translated")
                                        #
    system("awk '{print $1, $2, $3, $7, $5}' ~/gwas_results/GIANT_HEIGHT_Wood_et_al_2014_publicrelease_HapMapCeuFreq.txt > /tmp/GWAS2.result")
    pfun(pv)
    make_pgs(out.name=paste("vN_height_nc",pv,sep=""),clump=FALSE,gwas.file="/tmp/GWAS2.result",wd="/tmp/bd/",plink.file="/hrsshare/cleaned-N/hrs_geno_final_translated")
}
