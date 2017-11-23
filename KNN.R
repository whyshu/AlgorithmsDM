library(rgl)
library(scatterplot3d)
library(class)


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


## Selecting only 3 features for this demostration, just to keep things simple

myvars <- c("Retweets", "Favorites", "New_Feature")
traindataset.subset <- traindataset[myvars]
testdataset.subset<-testdataset[myvars]

summary(traindataset.subset)
summary(testdataset.subset)

#test <- 1:400
train.traindataset <- traindataset
test.testdataset <- testdataset

train.def <- traindataset$Class
test.def <- testdataset$Class


knn.1 <-  knn(train.traindataset, test.testdataset, train.def, k=1)
knn.5 <-  knn(train.traindataset, test.testdataset, train.def, k=5)
knn.20 <- knn(train.traindataset, test.testdataset, train.def, k=20)


s<-scatterplot3d(train.traindataset[,c("Retweets", "Favorites", "New_Feature")],color=c('black','red')[as.numeric(train.def)],pch=c(21,24)[as.numeric(train.def)])
s$points3d(test.testdataset[,c("Retweets", "Favorites", "New_Feature")],col=c('green','yellow')[as.numeric(knn.1)],pch=c(21,24)[as.numeric(knn.1)])

legend("topleft",col=c('black','red','yellow','green'),pch=c(21,24),bg=c(1,1),
       legend=c("TRAINED AS SPAM","TRAINED AS NON-SPAM","PREDICTED AS SPAM","PREDICTED AS NON-SPAM"),
       title="Symbols",bty="n",cex=.8)


