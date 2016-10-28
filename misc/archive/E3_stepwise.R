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
## glm(fm.baseline,df,family="binomial")->mod
## library(MASS)
## stepAIC(mod,scope=list(lower=~1,upper=~(raedyrs+bmi+height+smoke+cesd+diab+heart+alz+srh)*rabyear*gender+I(raedyrs^2)+I(bmi^2)+I(height^2)+I(smoke^2)+I(cesd^2)+I(diab^2)+I(heart^2)+I(alz^2)+I(srh^2)+(raracem+rahispan)*(raedyrs+bmi+height+smoke+cesd+diab+heart+alz+srh)),steps=1000)->mod.update
## save(mod.update,file="~/tmp/mod-updated.Rdata")
