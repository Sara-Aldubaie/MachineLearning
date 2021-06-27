# set directory 
setwd("C:/Users/SarOonh/DataScience/23")

# load the dataset 
dataset = read.csv('./boston.csv')


# Splitting the data-set into the Training set and Test set
#install.packages('caTools')
library(caTools)
set.seed(42)
split = sample.split(dataset$target, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#View(training_set)
#View(test_set)

# NOTE: 
# *** = NOX + RM + DIS +  RAD + PTRATIO + B + LSTAT
# ** = CRIM + ZN + CHAS , TAX + B


# ------------------------------------ 1. All Columns ----------------------------#

# Fitting Multiple Linear Regression to the Training set
regressor_1 = lm(formula = target ~ .,
               data = training_set)

summary(regressor_1)



# To get the coefficients 
regressor_1$coefficients

# Predicting the Test set results
y_pred_1 <- predict(regressor_1, newdata = test_set)
y_pred_1

y_actual <- test_set$target

error_1 <- y_pred_1 - test_set$target 
percent_error_1 <- abs(error_1)/y_pred_1

percent_error_1 <- round(percent_error_1,2)

df_multi_1 <- data.frame(y_pred_1, y_actual, error_1, percent_error_1)
#View(df_multi_1)

#----------------------------------------------------------------------------------------------#


# NOTE: 
# *** = NOX + RM + DIS +  RAD + PTRATIO + LSTAT
# ** = CRIM + ZN + CHAS + TAX + B


# ------------------------------------ 2. columns with (** & ***)-----------------------------#


# Fitting Multiple Linear Regression to the Training set
regressor_2 = lm(formula = target ~ NOX + RM + DIS +  RAD + PTRATIO + LSTAT + CRIM + ZN + CHAS + TAX + B,
               data = training_set)

summary(regressor_2)

# To get the coefficients 
regressor_2$coefficients

# Predicting the Test set results
y_pred_2 <- predict(regressor_2, newdata = test_set)
y_pred_2

y_actual <- test_set$target

error_2 <- y_pred_2 - test_set$target 
percent_error_2 <- abs(error_2)/y_pred_2

percent_error_2 <- round(percent_error_2,2)

df_multi_2 <- data.frame(y_pred_2, y_actual, error_2, percent_error_2)
#View(df_multi_2)


#----------------------------------------------------------------------------------------------#


# NOTE: 
# *** = NOX + RM + DIS +  RAD + PTRATIO + LSTAT

# ------------------------------------ 3. columns with (***)-----------------------------#

# Fitting Multiple Linear Regression to the Training set
regressor_3 = lm(formula = target ~ NOX + RM + DIS + PTRATIO + LSTAT,
                 data = training_set)

summary(regressor_3)

# To get the coefficients 
regressor_3$coefficients

# Predicting the Test set results
y_pred_3 <- predict(regressor_3, newdata = test_set)
y_pred_3

y_actual <- test_set$target

error_3 <- y_pred_3 - test_set$target 
percent_error_3 <- abs(error_3)/y_pred_3

percent_error_3 <- round(percent_error_3,2)

df_multi_3 <- data.frame(y_pred_3, y_actual, error_3, percent_error_3)
#View(df_multi_3)

#----------------------------------------------------------------------------------------------#


df_compare <- data.frame(y_actual, y_pred_1, error_1, percent_error_1, y_pred_2, error_2, percent_error_2, y_pred_3, error_3, percent_error_3)
View(df_compare)

#-----------------------------------------------------------------------------------------------#


#------------------------------------- calculating R2 ------------------------------------------#

# R2 square function to calculate the R2 cost
rsq <- function (x, y) cor(x, y) ^ 2


rsq_1 <- rsq(y_actual, y_pred_1)
rsq_2 <- rsq(y_actual, y_pred_2)
rsq_3 <- rsq(y_actual, y_pred_3)

rsq_1
rsq_2
rsq_3


rsq_score <- data.frame(rsq_1, rsq_2, rsq_3)
View(rsq_score)

# the second model when we used these columns (NOX + RM + DIS +  RAD + PTRATIO + LSTAT + CRIM + ZN + CHAS + TAX + B) (** & ***)

# report which features were deemed most important by R
#The following are the most important features (NOX + RM + DIS + PTRATIO + LSTAT) with ***
