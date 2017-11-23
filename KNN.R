library(rgl)
library(scatterplot3d)
library(class)
library(caret)

traindataset <- read.csv("Training_Dataset.csv")
head (traindataset)

testdataset <- read.csv("Test_Dataset.csv")
head (testdataset)

traindataset.bkup <- traindataset
testdataset.bkup<-testdataset

## Convert the dependent var to factor. Normalize the numeric variables  
traindataset$Class <- factor(traindataset$Class)
num.vars <- sapply(traindataset, is.numeric)
traindataset[num.vars] <- lapply(traindataset[num.vars], scale)
par(mar = rep(2, 4))

## Convert the dependent var to factor. Normalize the numeric variables  
testdataset$Class <- factor(testdataset$Class)
num.vars1 <- sapply(testdataset, is.numeric)
testdataset[num.vars1] <- lapply(testdataset[num.vars1], scale)
par(mar = rep(2, 4))


## Selecting only 3 features 
myvars <- c("Retweets", "Favorites", "New_Feature")
traindataset.subset <- traindataset[myvars]
testdataset.subset<-testdataset[myvars]

summary(traindataset.subset)
summary(testdataset.subset)

#test <- 1:400
train.traindataset <- traindataset.subset
test.testdataset <- testdataset.subset

train.def <- traindataset$Class
test.def <- testdataset$Class

#Fit method
knn.1 <-  knn(train.traindataset, test.testdataset, train.def, k=1)
knn.2 <-  knn(train.traindataset, test.testdataset, train.def, k=2)
knn.100 <- knn(train.traindataset, test.testdataset, train.def, k=100)

#s<-scatterplot3d(train.traindataset[,c("Retweets", "Favorites", "New_Feature")],color=c('black','red')[as.numeric(train.def)],pch=c(21,24)[as.numeric(train.def)])
#s$points3d(test.testdataset[,c("Retweets", "Favorites", "New_Feature")],col=c('green','yellow')[as.numeric(knn.1)],pch=c(21,24)[as.numeric(knn.1)])

scatterplot3d(test.testdataset[,c("Retweets", "Favorites", "New_Feature")],color=c('green','yellow')[as.numeric(test.def)],pch=c(21,24)[as.numeric(test.def)],xlim=c(-1,5),ylim=c(-1,5),zlim=c(-1,5))
legend("topleft",col=c('black','red','yellow','green'),pch=c(21,24),bg=c(1,1),
       legend=c("TRAINED AS SPAM","TRAINED AS NON-SPAM","PREDICTED AS SPAM","PREDICTED AS NON-SPAM"),
       title="Symbols",bty="n",cex=.8)

pred_data<-data.frame(test.testdataset,pred=knn.100)
#print(pred_data[,4])
print(test.def)

#knn.reg(train.traindataset,test.testdataset,k=3)

Accuracy<-function(){
  actual<-test.def
  print(actual)
  prediction<-unlist(pred_data[,4])
  print(length(prediction))
  table(actual,prediction)
  confusionMatrix(actual,prediction)
}