# machine_learning_crime
Predict crime using 3 machine learning techniques

From the following website you can download data from over 2000 communities about crime statistics:
http://archive.ics.uci.edu/ml/datasets/Communities+and+Crime+Unnormalized#

The communities.txt file is a CSV file with the actual data. The names.txt file contains the row names.

PRED_MURDER.R is code I used to predict the murder rate from a subset of the predictors. The three prediction techniques I use are Stepwise Regression, Penalized (Lasso) Regression, and Regression Tree. For the latter two, I use 10-fold cross validation on the mean squared error to determine the best coefficients to use. I examine the effectiveness of these three tools by looking at R-squared and root mean square error.
