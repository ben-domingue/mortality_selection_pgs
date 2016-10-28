#not really emphasized at present.

load("~/hrs/mortality/all.Rdata")
c("raedyrs","bmi","height","smoke","cesd","diab","heart","alz")->vars
df[,vars]->tmp
cor(tmp[df$geno==1,],use='p')->C1
cor(tmp[df$geno==0,],use='p')->C0

split(df,df$racohbyr)->tmp
fun<-function(df) {
    df[,vars]->tmp
    cor(tmp[df$geno==1,],use='p')->C1
    cor(tmp[df$geno==0,],use='p')->C0
    C1-C0
}
lapply(tmp,fun)
