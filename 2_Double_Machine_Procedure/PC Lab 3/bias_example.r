##############################################################################
########################  Load Packages and the Data  ########################
##############################################################################

### Load the packages  
library(fBasics)     # use for descriptive statistics
library(tidyverse)   # use for handling data
library(caret)       # use for handling data
library(lmtest)      # use for heteroscedasticity robust standard errors
library(sandwich)    # use for heteroscedasticity robust standard errors
library(hdm)         # use for Lasso and Post-Double-Selection
library(glmnet)      # use for lasso and Elastic Net regularized Generalized Linear Models
options(warn=-1)     # supress warnings

print('All packages successfully installed and loaded.')

### Load the Data
set.seed(12345678) 
df <- read.csv("job_corps.csv",header=TRUE, sep=",") # load data from csv-file
df <- df[sample(c(1:nrow(df)), size=3000, replace =F),] # Select a random subsample of 3000 observations
print('Data successfully loaded.')

##############################################################################


#########################################################################
########################  Univariate OLS Regression #####################
#########################################################################

## Univariate OLS
ols1 <- lm(EARNY4 ~ assignment, data = df)
summary(ols1)
ols <- summary(ols1)$assignment

set.seed(1000000001)
bias <- X <- rbinom(nrow(df), 1, .8)
df$assignment[bias == 1] <- df$female[bias == 1]
cor(df$assignment,df$female)

## Univariate OLS
ols3 <- lm(EARNY4 ~ assignment + female, data = df)
summary(ols3)

## Univariate OLS
ols2 <- lm(EARNY4 ~ assignment, data = df)
summary(ols2)

##############################################################################
####################################################################
################# Cross-Validated Lasso ############################
####################################################################

set.seed(123456789) # Starting value

# Cross-validated Lasso in earnings equation
lasso <- cv.glmnet(as.matrix(df[,c(-1,-3)]), as.matrix(df$EARNY4), 
                        alpha=1, nfolds = 10, type.measure = 'mse', standardize = TRUE,
                        penalty.factor=c(0, rep(1, ncol(df) - 3)))
# alpha =1 is Lasso, alpha = 0 is Ridgde
# nfolds - number of cross-validation folds
# type.measure - measure for model accuracy
coef(lasso,s = lasso$lambda.1se)




# Select covariates with non-zero coefficients
coef <- predict(lasso,s = lasso$lambda.1se, type = "nonzero") #
colnames <- colnames(df[,c(-1,-3)])
n1 <- colnames[unlist(coef)]
print(paste0("Number of Selected Variables Earnings Equation: ",length(n1)))
print("Selected Variables:")
print(n1)



####################################################################

###############################################################################
# Post-Lasso Model
###############################################################################

# Take union of selected covariates
selected_covariates <- c(unique(c("assignment",n1)))

# Setup the formula of the linear regression model
sumx <- paste(selected_covariates, collapse = " + ")  
linear <- paste("EARNY4",paste(sumx, sep=" + "), sep=" ~ ")
linear <- as.formula(linear)

# Post-Lasso OLS regression
ols <- lm(linear, data = df)
summary(ols)

