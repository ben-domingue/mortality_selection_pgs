load(file="~/hrs/mortality/association.Rdata")
df[!is.na(df$vN_education2_nc.pgs),]->df
split(df,df$gender)->dfL
library(survey)

par(mfcol=c(4,2))
for (i in 1:length(dfL)) {
    dfL[[i]]->df
    nms<-c("vN_bmi_nc.pgs","vN_height_nc.pgs","vN_education2_nc.pgs","vN_eversmoke_nc.pgs")
    tab<-list()
    for (nm in nms) {
        paste(nm,"~rabyear")->fm
        lm(fm,df)->m1
        svyglm(fm,design = svydesign(~ 1, weights = ~ wt.health,data=df))->m2
        c(coef(m1),coef(m2))->tab[[nm]]
        fitted(m1)->y1
        fitted(m2)->y2
        data.frame(x=df$rabyear,y1=y1,y2=y2)->tmp
        plot(NULL,xlim=c(range(df$rabyear)),ylim=c(range(c(y1,y2))))
        lines(tmp$x,tmp$y1,col="black")
        lines(tmp$x,tmp$y2,col="red")
    }
    print(tab)
}

