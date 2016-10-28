## load("~/hrs/mortality/all.Rdata")
## df[df$rabyear %in% 1910:1960,]->df
## split(df,df$ragender)->tmp
## for (i in 1:length(tmp)) {
##     tmp[[i]]->z
##     (z$height-mean(z$height,na.rm=TRUE))/sd(z$height,na.rm=TRUE)->z$height
##     z->tmp[[i]]
## }
## data.frame(do.call("rbind",tmp))->df


## mod.fun<-function(fm,df) {
##     formula(fm)->fm
##     df[,all.vars(fm)]->tmp
##     df[rowSums(is.na(tmp))==0,]->df
##     glm(fm,df,family="binomial")->mod
##     mod
## }

## tab<-list()
## "geno~(raedyrs+bmi+height+smoke+cesd+diab+heart+alz+srh+gender)*rabyear"->fm
## mod.fun(fm,df)->tab$all
## for (var in c("raedyrs","bmi","height","smoke","cesd","diab","heart","alz","srh")) {
##     paste("geno~(",var,"+gender)*rabyear")->fm
##     mod.fun(fm,df)->tab[[var]]
## }

## table.lm<-function(mod) {
##   lapply(mod,function(x) names(coef(x)))->nms
##   unique(do.call("c",nms))->nms
##   length(nms)->nr
##   length(mod)->nc
##   mat.est<-mat.tstat<-matrix(NA,nr,nc)
##   for (j in 1:nc) {
##     summary(mod[[j]])$coef->foo
##     for (i in 1:nr) {
##       match(nms[i],rownames(foo))->index
##       if (length(index)>0) {
##         foo[index,1]->mat.est[i,j]
##         foo[index,3]->mat.tstat[i,j]
##       }
##     }
##   }
##   sapply(mod,function(x) length(residuals(x)))->N
##   out<-list()
##   new.nms<-list()
##   for (i in 1:nr) {
##     rbind(mat.est[i,],mat.tstat[i,])->out[[i]]
##     new.nms[[i]]<-c(nms[i],paste(nms[i],".tstat",sep=""))
##   }
##   do.call("rbind",out)->out
##   rbind(out,N)->out
##   c(do.call("c",new.nms),"N")->rownames(out)
##   out
## }
## table.lm(tab)->tab
## write.csv(tab,"")

