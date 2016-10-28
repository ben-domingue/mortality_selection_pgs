## "geno~(raedyrs+bmi+height+smoke+cesd+diab+heart+alz+srh+gender)*rabyear+rahispan+raracem"->fm.baseline

## #stepwise component
## load("~/hrs/mortality/all.Rdata")
## df[df$rabyear %in% 1910:1960,]->df
## split(df,df$ragender)->tmp
## for (i in 1:length(tmp)) {
##     tmp[[i]]->z
##     (z$height-mean(z$height,na.rm=TRUE))/sd(z$height,na.rm=TRUE)->z$height
##     z->tmp[[i]]
## }
## data.frame(do.call("rbind",tmp))->df
## formula(fm.baseline)->fm.baseline
## df[,all.vars(fm.baseline)]->tmp
## df[rowSums(is.na(tmp))==0,]->df

## library(randomForest)
## #
## factor(ifelse(df$geno==1,"yes","no"))->df$geno
## factor(df$gender)->df$gender
## factor(df$rahispan)->df$rahispan
## factor(df$raracem)->df$raracem
## randomForest(formula=fm.baseline,data=df)->mod
## save(mod,file="~/hrs/mortality/random_forest_model.Rdata")

## library(cvAUC)
## mod$votes[,2]->pr
## df$geno->obs
## auc.boot<-function(pr,obs) {
##     AUC(pr,obs)
## }
## auc.boot(pr,obs)->auc
## boot<-numeric()
## length(pr)->n
## for (i in 1:100) {
##     sample(1:n,n,replace=TRUE)->index
##     auc.boot(pr[index],obs[index])->boot[i]
## }
## quantile(boot,c(.025,.975))*1000->auc.ci
