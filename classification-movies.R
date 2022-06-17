library(rminer)
library(rpart.plot)
library(dplyr)

# classification
w <- df_complete #complete.csv
View(w)

#remove duplicates
w = w[order(w[,'tconst'],-w[,'worldwideSalesDollars']),]
w = w[!duplicated(w$tconst),]
print(summary(w))

#replace nas with 0
w[is.na(w)] = 0

# add binary attribute
w$outcome=cut(w$worldwideSalesDollars,c(-0.5,0.5,3e+09),c("unpopular","popular"))

print(summary(w))
w$tconst <- as.factor(w$tconst)
w$primaryTitle <- as.factor(w$primaryTitle)
w$genres <- as.factor(w$genres)
w$directors <- as.factor(w$directors)
w$writers <- as.factor(w$writers)
w$mainCast <- as.factor(w$mainCast)

#remove maincast bc it doesnt work
w = subset(w, select = -c(mainCast) )

#remove 'useless'
w = subset(w, select = -c(worldwideSalesDollars) )
w = subset(w, select = -c(primaryTitle) )
w = subset(w, select = -c(runtimeMinutes) )
w = subset(w, select = -c(isAdult) )

#mpause("random selection of 500 examples:")
# select random sample of 500 examples (to speedup execution)
s=sample(1:nrow(w),500)
ws=w[s,]
#mpause("transform quality into (bad,medium,good):")
print(summary(ws))
View(ws)
plot(ws$outcome)

#mpause("save transformed data into a new csv: wq3.csv (bad,medium,good):")
write.table(file="moviesuccess500.csv",ws,row.names=FALSE,col.names=TRUE,sep=",") # , is for weka


#mpause("fit of a decision tree:")
dt=fit(outcome~.,ws,model="dt")
#mpause("show decision tree:")
plot(dt@object);text(dt@object)
#mpause("show same decision tree in a nicer way:")
rpart.plot(dt@object)

#mpause("fit a SVM:")
svm=fit(outcome~.,ws,model="ksvm",search="heuristic5")
#mpause("get predictions:")
pdt=predict(dt,w[-s,])
psvm=predict(svm,w[-s,])
Y=w[-s,]$outcome
#mpause("show some dt metrics:")
print(mmetric(Y,pdt,metric=c("ACC","AUC","ACCLASS","AUCCLASS")))
print(mmetric(Y,pdt,metric="CONF"))
#mpause("show some svm metrics:")
print(mmetric(Y,psvm,metric=c("ACC","AUC","ACCLASS","AUCCLASS")))
print(mmetric(Y,psvm,metric="CONF"))
#mpause("show ROC for svm:")
mgraph(Y,psvm,graph="ROC",TC=1,baseline=TRUE,leg="good",Grid=10) #TC era 3 e meti 1
#mpause("show ROC for svm and dt:")
L=vector("list",2)
testl=vector("list",1);testl[[1]]=Y
p1=vector("list",1);p1[[1]]=psvm
p2=vector("list",1);p2[[1]]=pdt
L[[1]]=list(pred=p1,test=testl,runs=1)
L[[2]]=list(pred=p2,test=testl,runs=1)
mgraph(L,graph="ROC",TC=1,baseline=TRUE,leg=c("svm","dt"),main="good",Grid=10) #TC era 3 e meti 1