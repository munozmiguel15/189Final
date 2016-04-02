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

##
##
##
##
##

traindata <-read.csv("train.csv", header=T)
cleantrain <- traindata[complete.cases(traindata),]
sample <- cleantrain[sample(nrow(cleantrain), 15000, replace =  FALSE), ]##create random sample

###finding the best order for predictors
stepAIC(glm(target ~ ( v56 + v125 + v113 + v100+ v58 + v112+ v119 + v117 ), data=sample), scale = 0, trace=FALSE,
        direction = "both", steps = 1000)
step(glm(target ~ ( v22 + v56 + v125 + v113 + v100+ v58 + v112+ v119 + v117+ v79+ v82+ v52+ v23+ v40+ v63+ v25+ v46+ v109+ v87 + v2 + v8 + v69 + v115 + v30 + v47 ), data=sample), scale = 0,
     direction = "forward", steps = 1000)

##logistic regression

##glm model using our most important predictors
model <- glm( target ~ v56 + v113 + v119 + v79 + v109 + v30, family=binomial(link='logit'), data=sample)
summary(model)##interpretation
exp(coef(model)) ##exponentiated coefficients

confint(model)##CIs


##testing the significance of the model 
with(model, null.deviance - deviance)
with(model, df.null - df.residual)                            
with(model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))


##
##
##
##
##

test = read.csv("test.csv", header = TRUE)
train = read.csv("train.csv", header = TRUE)
cleantrain = train[complete.cases(train),]
cleantest = test[complete.cases(test),]
v3<-levels(cleantrain$v3) <- c(1:4)
v22<-levels(cleantrain$v22) <- c(1:118211)
v24<-levels(cleantrain$v24) <- c(1:5)
v30<-levels(cleantrain$v30) <- c(1:8)
v31<-levels(cleantrain$v31) <- c(1:4)
v47<-levels(cleantrain$v47) <- c(1:10)
v52<-levels(cleantrain$v52) <- c(1:13)
v56<-levels(cleantrain$v56) <- c(1:123)
v66<-levels(cleantrain$v66) <- c(1:3)
v71<-levels(cleantrain$v71) <- c(1:9)
v74<-levels(cleantrain$v74) <- c(1:3)
v75<-levels(cleantrain$v75) <- c(1:4)
v79<-levels(cleantrain$v79) <- c(1:18)
v91<-levels(cleantrain$v91) <- c(1:8)
v107<-levels(cleantrain$v107) <- c(1:8)
v110<-levels(cleantrain$v110) <- c(1:3)
v112<-levels(cleantrain$v112) <- c(1:23)
v113<-levels(cleantrain$v113) <- c(1:37)
v125<-levels(cleantrain$v125) <- c(1:92)
sampletrain = cleantrain[sample(nrow(cleantrain), 500), ]
sampletrain = sampletrain[-1]
sampletest = cleantrain[sample(nrow(cleantrain), 500), ]
sampletest = sampletest[-1]
classifiertest = sampletest[-2:-length(sampletest)]
classifier = sampletrain[-2:-length(sampletrain)]
sampletrain = sampletrain[-1]
sampletest = sampletest[-1]
classifier = unlist(classifier)
classifiertest = unlist(classifiertest)
samplematrix <- matrix(as.matrix(sampletrain), ncol = ncol(sampletrain), dimnames = NULL)
sampletestmatrix <- matrix(as.matrix(sampletest), ncol = ncol(sampletest), dimnames = NULL)
write.csv(samplematrix, file = "samplematrix.csv")
write.csv(sampletestmatrix, file = "sampletest.csv")
write.csv(classifier, file = "classifier.csv")
write.csv(classifier, file = "classifiertest.csv")


##
##
##
##
##


