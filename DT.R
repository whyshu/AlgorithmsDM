library(rattle)
library(rpart.plot)
library(RColorBrewer)
train <- data.frame(ClaimID = c(1,2,3,4,5,6,7,8,9,10),
                    RearEnd = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE),
                    Whiplash = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE),
                    Activity = factor(c("active", "very active", "very active", "inactive", "very inactive", "inactive", "very inactive", "active", "active", "very active"),
                                      levels=c("very inactive", "inactive", "active", "very active"),
                                      ordered=TRUE),
                    Fraud = c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE))
library(rpart) #load the rpart package

mytree <- rpart(Fraud ~ RearEnd + Whiplash + Activity, data = train, method = "class", minsplit = 2, minbucket = 1,cp=-1)
fancyRpartPlot(mytree)