install.packages("caret")
install.packages("Boruta")
install.packages("dplyr")

library(caret)
library(Boruta)
library(dplyr)

##get that sweet sweet data##
train = read.csv(file = "train.csv",stringsAsFactors = FALSE)

##Creating random set, will be used as basis for other stuff later##
set.seed(189)
DataPart = createDataPartition(train$target,p=0.01,list=FALSE)
Base = train[DataPart,]

##Separating variables into types##
Attri = setdiff(names(Base),c("ID","target"))
Classification = sapply(Attri,function(x){class(Base[,x])})
UnqClass = unique(Classification)
TypeAttri = lapply(UnqClass,function(x){names(Classification[Classification==x])})
names(TypeAttri) = UnqClass

# Filling in missing values and combining numeric + character data##
preproc = preProcess(Base[c(TypeAttri$numeric,TypeAttri$integer)],
                     method=c("medianImpute"))
preprocBase = predict(preproc,Base[c(TypeAttri$numeric,TypeAttri$integer)])
Data = cbind(preprocBase,Base[TypeAttri$character])

##Analysis##
set.seed(2016)
Results = Boruta(Data,factor(Base$target),
                 maxRuns=101,
                 doTrace=0)
Stats = attStats(Results)
borPlot = plot(main = "Importance of Variables",normHits~meanImp,col=Stats$decision,data=Stats)