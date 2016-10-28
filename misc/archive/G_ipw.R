## load("~/hrs/mortality/all.Rdata")
## df[df$rabyear %in% 1910:1960,]->df

## #glm(geno~raedyrs+bmi+height+smoke+cesd+diab+heart+alz+gender+rabyear,df,family="binomial")->mod.base
## formula("geno~(raedyrs+bmi+height+smoke+cesd+diab+heart+alz+gender)*rabyear")->fm
## df[,all.vars(fm)]->tmp
## df[rowSums(is.na(tmp))==0,]->tmp
## #glm(fm,tmp,family="binomial")->mod.inter
## #fitted(mod.inter)->tmp$fitted
## library(ipw)
## ipwpoint(geno,data=tmp,family="binomial",link="logit",numerator=~1,denominator=~(raedyrs+bmi+height+smoke+cesd+diab+heart+alz+gender)*rabyear,trunc=0.01)->wt
## wt$weights.trunc->tmp$wt

## load(file="~/hrs/mortality/df.Rdata")
## #now standardize polygenic scores
## grep("profile",names(df))->index
## for (nm in names(df)[index]) {
##     df[[nm]]->z
##     (z-mean(z,na.rm=TRUE))/sd(z,na.rm=TRUE)->df[[paste(nm,".nopcs",sep="")]]
##     lm(paste(nm,"~",paste(paste("pc",1:10,sep=""),collapse="+")),df)->mod
##     mod$resid->z
##     (z-mean(z,na.rm=TRUE))/sd(z,na.rm=TRUE)->df[[nm]]
## }
## names(df)->nms
## gsub(".profile",".pgs",nms)->nms
## nms->names(df)

## load("/hrsshare/hrs_linked_N.Rdata")
## x[x$rahispan=="0. not hispanic" & x$raracem=="1.white/caucasian",]->x
## x[,c("hhidpn","subjectID")]->x
## merge(df,x)->df
## merge(df,tmp)->df

## reg_fun<-function(xv,yv,df) {
##     paste(yv,"~",xv,"*rabyear")->fm
##     lm(fm,df)->m1
##     print(length(m1$resid))
##     library(survey)
##     svyglm(fm,design = svydesign(~ 1, weights = ~ wt,data=df))->m2
##     print(summary(m2))
##     print(length(m2$resid))
##     cbind(summary(m1)$coef[,c(1,4)],summary(m2)$coef[,c(1,4)])->mat
##     mat[,3]/mat[,1]->z
##     cbind(mat,z)
## }
## reg_fun(yv="raedyrs",xv="vN_education_nc.pgs",df)

## reg_fun(yv="bmi",xv="vN_bmi_cl.profile",df)
## reg_fun(yv="alz",xv="vN_alz_nc.profile",df)
## reg_fun(yv="height",xv="vN_height_nc.profile",df)
## reg_fun(yv="heart",xv="vN_cardio_nc.profile",df)


    
