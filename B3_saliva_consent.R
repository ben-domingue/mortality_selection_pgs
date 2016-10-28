read.dat.dct <- function(dat, dct, labels.included = "no") {
  #from http://stackoverflow.com/questions/14224321/reading-dat-and-dct-directly-from-r
  temp <- readLines(dct)
    temp <- temp[grepl("_column", temp)]
    switch(labels.included,
           yes = {
               pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+)[a-z]\\s+(.*)"
               classes <- c("numeric", "character", "character", "numeric", "character")
               N <- 5
               NAMES <- c("StartPos", "Str", "ColName", "ColWidth", "ColLabel")
           },
           no = {
               pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+).*"
               classes <- c("numeric", "character", "character", "numeric")
               N <- 4
               NAMES <- c("StartPos", "Str", "ColName", "ColWidth")
           })
    metadata <- setNames(lapply(1:N, function(x) {
        out <- gsub(pattern, paste("\\", x, sep = ""), temp)
        out <- gsub("^\\s+|\\s+$", "", out)
        out <- gsub('\"', "", out, fixed = TRUE)
        class(out) <- classes[x] ; out }), NAMES)
    metadata[["ColName"]] <- make.names(gsub("\\s", "", metadata[["ColName"]]))
    myDF <- read.fwf(dat, widths = metadata[["ColWidth"]], 
             col.names = metadata[["ColName"]])
    if (labels.included == "yes") {
        attr(myDF, "col.label") <- metadata[["ColLabel"]]
    }
    myDF
}

#2006
setwd("/hrsshare/public_release_data/2006/")
read.dat.dct(dat="H06I_R.da",dct="H06I_R.dct")->df
table(df$KI913)
df[,c("HHID","PN","KI913")]->df2006

#2008
setwd("/hrsshare/public_release_data/2008/")
read.dat.dct(dat="H08I_R.da",dct="H08I_R.dct")->df
table(df$LI913)
df[,c("HHID","PN","LI913")]->df2008

merge(df2006,df2008,all=TRUE)->df
as.character(as.numeric(df$HHID)*1000+as.numeric(df$PN))->df$hhidpn
df->gr
save(df,file="~/hrs/mortality/geno_refusal.Rdata")

## readLines("~/hrs/mortality/phs000428.v1.pht002614.v1.p1.c1.phenotype_HRS_2006_2008.NPR.txt")->coll
## grep("^dbGaP",coll)->index
## coll[-(1:(index-1))]->coll
## strsplit(coll,"\t")->coll
## do.call("rbind",coll)->coll
## coll[1,]->nms
## data.frame(coll[-1,])->coll
## nms->names(coll)
## coll[,c("local_id","colyear")]->coll
## as.numeric(coll$local_id)->coll$local_id

## load("/hrsshare/hrs_linked_N.Rdata")
## x[,c("hhidpn","local_id")]->ids
## merge(df,ids,all=TRUE)->df
## merge(df,coll,all=TRUE)->df

## ifelse(is.na(df$KI913),-1,df$KI913)->df$KI913
## ifelse(is.na(df$LI913),-1,df$LI913)->df$LI913
## ifelse(df$KI913==5 | df$LI913==5,1,NA)->df$geno.refusal
## ifelse(df$KI913==1 | df$LI913==1,0,df$geno.refusal)->df$geno.refusal


## df[,c("hhidpn","geno.refusal","KI913","LI913","colyear")]->gr

## ########################################################################################
## load("~/hrs/mortality/all.Rdata")
## merge(df,gr,all=TRUE)->df

## df[!is.na(df$geno.refusal),]->df

## table(df$KI913)
## table(df$LI913)

## tab<-function(x) table(x)/length(x)
## by(df$gender,df$geno.refusal,tab)
## by(df$raracem,df$geno.refusal,tab)
## by(df$rahispan,df$geno.refusal,tab)

## df[df$geno.refusal==1,]->x1
## df[df$geno.refusal!=1,]->x0
## png("/tmp/spitters.png",units="in",height=5,width=5,res=150,pointsize=13)
## plot(NULL,xlim=c(1900,1980),ylim=c(0,0.04),xlab="Birth year",ylab="Density")
## lines(density(x0$rabyear,na.rm=TRUE),col="black")
## lines(density(x1$rabyear,na.rm=TRUE),col="red")
## legend("topright",bty="n",c("Non-refusers","Refusers"),lty=1,col=c("black","red"))
## dev.off()

