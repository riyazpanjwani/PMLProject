require(ggplot2)
require(lattice)
require(caret)
require(xgboost)
require(stats)
require(corrplot)

# URL of the training and testing data
train.url ="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test.url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

#train <- read.csv(url(train.url))
#test <-  read.csv(url(test.url))
train <- read.csv("G:/pml-training.csv")
test <- read.csv("G:/pml-testing.csv")
dim(train)
dim(test)
names(train)

##Data Cleaning

out <- train[,"classe"]
levels(out)
levels(out) <- 1 : length(levels(out))
#head(out)
train$classe <- NULL
filter <- grep("belt|arm|dumbell",names(train))
train <- train[,filter]
test <- test[,filter]

cols.without.na <- colSums(is.na(test)) == 0
train <- train[,cols.without.na]
test <- test[,cols.without.na]

zero.var <- nearZeroVar(train,saveMetrics = TRUE)

corrplot.mixed(cor(train), lower="circle", upper="color", 
               tl.pos="lt", diag="n", order="hclust", hclust.method="complete")

# convert data to matrix
train.matrix <- as.matrix(train)
mode(train.matrix) <- "numeric"
test.matrix <- as.matrix(test)
mode(test.matrix) <- "numeric"
# convert outcome from factor to numeric matrix 
#   xgboost takes multi-labels in [0, numOfClass)
y <- as.matrix(as.integer(out)-1)

param <- list("objective" = "multi:softprob",    # multiclass classification 
              "num_class" = length(levels(out)),    # number of classes 
              "eval_metric" = "merror",    # evaluation metric 
              "nthread" = 8,   # number of threads to be used 
              "max_depth" = 16,    # maximum depth of tree 
              "eta" = 0.3,    # step size shrinkage 
              "gamma" = 0,    # minimum loss reduction 
              "subsample" = 1,    # part of data instances to grow tree 
              "colsample_bytree" = 1,  # subsample ratio of columns when constructing each tree 
              "min_child_weight" = 12  # minimum sum of instance weight needed in a child 
)

# set random seed, for reproducibility 
set.seed(1234)
# k-fold cross validation, with timing
nround.cv <- 50
system.time( bst.cv <- xgb.cv(param=param, data=train.matrix, label=y, 
                              nfold=4, nrounds=nround.cv, prediction=TRUE, verbose=FALSE) )

# index of minimum merror
min.merror.idx <- which.min(bst.cv$dt[, test.merror.mean]) 
min.merror.idx 
# get CV's prediction decoding
pred.cv <- matrix(bst.cv$pred, nrow=length(bst.cv$pred)/length(levels(out)), ncol=length(levels(out)))
pred.cv <- max.col(pred.cv, "last")
# confusion matrix
confusionMatrix(factor(y+1), factor(pred.cv))

