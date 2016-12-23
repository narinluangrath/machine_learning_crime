##------------------------------
## Part 1 - Data Prep
##------------------------------

# Import data and add column names
setwd("C:\Users\Khamphet Luangrath\Documents\mach_learn\crime")
crime <- read.table("Community.txt", sep=",", na.strings="?")
names <- read.table("names.txt",
                    sep="\t",
                    stringsAsFactors=F,
                    strip.white=T,
                    header=F)
names <- names[[1]]
names <- gsub(",", "", names)
colnames(crime) <- names

# Include predictors 6-129 and outcome variable 131 ('murdPerPop')
# Furthermore, remove columns with more than 1000 NA values
crime <- crime[c(6:129, 131)]
good_col <- character(0)
for (col_name in colnames(crime)) {
  if (sum(is.na(crime[col_name])) < 1000) {
    good_col <- c(good_col, col_name)
  }
}
crime <- crime[good_col]
crime <- na.omit(crime)

# Set seed so others can replicate results
# Randomly select 70% of data as training and 30% as test
set.seed(1234)
n <- nrow(crime)
index <- sample(n, round(0.7 * n))
train <- crime[index, ]
test <- crime[-index, ]

# A function to evaluate performance
perform <- function(y, y_hat){
  r2 <- cor(y, y_hat)^2
  rmse <- sqrt(mean((y - y_hat)^2))
  results <- list(Rsquare=r2, RMSE=rmse)
  return(results)
}

##------------------------------
## Part 2 - Stepwise Regression
##------------------------------

# Make model and print summary
null <- lm(murdPerPop ~ 1, data=train)
full <- lm(murdPerPop ~ ., data=train)
model_sw <- step(null, 
                 scope=list(upper=full),
                 data=train,
                 direction="both")
summary(model_sw)

# Resubstitution (test on training data)
pred_train <- predict(model_sw, newdata=train)
perform(train$murdPerPop, pred_train)

# Test on testing data
pred_test <- predict(model_sw, newdata=test)
perform(test$murdPerPop, pred_test)

##--------------------------------------
## Part 3 - Penalized (Lasso) Regression
##--------------------------------------

library(glmnet)

x <- model.matrix(murdPerPop ~ ., train)[,-1]
y <- train$murdPerPop

# Plot how predictor variables shrink as lambda increases
grid <- 10^seq(8, -4, length=250)
model_lasso <- glmnet(x, y, alpha=1, lambda=grid)
plot(model_lasso, xvar="lambda", main="Lasso")

# 10-fold cross validation of lambda values
set.seed(1234)
cv_lambda <- cv_glmnet(x, y, lambda=grid, alpha=1)
plot(cv_lambda)

# The chosen lambda value
bestlam <- cv_lambda$lambda.min
bestlam

# The variables selected for our model
coef(model_lasso, s=bestlam)

# Resubstitution
lasso_pred <- predict(model_lasso, s=bestlam, newx=x)
perform(train$murdPerPop, lasso_pred)

# Test with New Data
x <- model.matrix(murdPerPop ~ ., test)[,-1]
lasso_pred <- predict(model_lasso, s=bestlam, newx=x)
perform(test$murdPerPop, lasso_pred)


##------------------------------
## Part 3 - Regression Tree
##------------------------------

library(rpart)
library(rpart.plot)

set.seed(1234)
rtree <- rpart(murdPerPop ~ ., data=train)

# Plot unpruned tree
rpart.plot(rtree)

# 10-fold cross validation on complexity parameter CP
rtree$cptable
plotcp(rtree)

# Prune tree (using left most point below line)
rtree_pruned <- prune(rtree, cp=0.027)
rpart.plot(rtree.pruned)

# Resubstitution
yhat <- predict(rtree_pruned, train)
perform(train$murdPerPop, yhat)

# Test data
yhat <- predict(rtree_pruned, test)
perform(test$murdPerPop, yhat)
