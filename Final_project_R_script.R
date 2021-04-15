# Reading in the data
stroke <- read.csv("healthcare-dataset-stroke-data.csv")

# Transforming bmi from character to numeric
stroke$bmi <- as.numeric(stroke$bmi)

# Checking the balance of the dataset's binary variables 

barplot(table(stroke$gender),
        ylim = c(0, 3000),
        col = c("pink", "blue", "gray"),
        main ="Barplot of Gender")
# Roughly 2000 males and 3000 females and very few labled as "other"

barplot(table(stroke$hypertension),
        ylim = c(0, 5000),
        col = c("green","red"),
        names = c("no hypertension", "hypertension"),
        main ="Barplot of Hypertension")
# A lot more people without hypertension that with hypertension, as would be expected in a normal population.

barplot(table(stroke$heart_disease),
        ylim = c(0, 5000),
        col = c("green","red"),
        names = c("no heart disease", "heart disease"),
        main ="Barplot of Heart Disease")
# Again, a lot more people without heart disease than with heart disease, which is a reasonable result.

barplot(table(stroke$ever_married),
        ylim = c(0, 4000),
        col = c("red", "green"),
        main = "Barplot of Ever Married")
# We have more married people than not married, but the importance of this variable is unclear.


barplot(table(stroke$Residence_type),
        ylim = c(0, 3000),
        col = c("green", "gray"),
        main = "Barplot of Residence Type")
# The residence type is pretty even, half urban and half rural.

barplot(table(stroke$smoking_status),
        ylim = c(0, 2000),
        cex.names = 0.71,
        col = c("orange", "green", "red", "gray"),
        main = "Barplot of Smoking Status")
# Most people in this dataset either never smoked or their status is unknown. 
# Less than 2000 people smoke or formerly smoked.

# Finally, our main variable of interest:
barplot(table(stroke$stroke),
        ylim = c(0, 5000),
        col = c("green", "red"),
        names= c("no stroke", "stroke"),
        main = "Barplot of Stroke")
# This dataset seems to be a bit unbalanced in terms of how many people have had a stroke or not.
# A lot more people have not ever suffered from a stroke than those who have. This is consistent with reality.



# Dataset is unbalanced. Cosnder bootstrapping, bagging. 
#Ask prof whether you need to worry about this issue. 
#DON'T DO DECISION TREES - MORE SUSCEPTIBLE TO UNBALANCED DATASET
# use random forest instead of decision tree because dataset is unbalanced? ASK PROF FIRST; 
# YOU NEEED BOOTSTRAPPING FIRST, OTHERWIESE DO NOT RUN THE DECISION TREE


sum(is.na(stroke$bmi))
# Since we only have 201 observations for bmi that are NA 
# (which is just 201/5110 * 100 = 3.93% of the total data entries),
# we can then safely omit these rows when cleaning the data,

# Data cleaning
stroke <- na.omit(stroke)
# we have 4909 valid observations in the stroke dataset

shuffled_index <- sample(1:4909, 4909, replace = F)
stroke <- stroke[shuffled_index,]

# Partitiong the data into the training and testing set

# We will take 20% of the data as the testing set and 80% as the training set.
set.seed(1)
# for keeping dataset partitioning consistent across different runs.
seq <- sample(1:4909, 981, replace = F)
testing <- stroke[seq, ]
training <- stroke[-seq, ]


library(glmnet)

# Generating model matrix
X <- model.matrix(stroke ~ gender + age + hypertension + heart_disease + ever_married + work_type + Residence_type + avg_glucose_level + bmi + smoking_status, data = training)[,-1]
# `[,-1]` removes the first column: The first column corresponds to an intercept.
# `glmnet` also adds an intercept, therefore we remove it here.

# Finding the best value for lambda

set.seed(1)
cv_result <- cv.glmnet(x = X, y = training$stroke, family = "binomial")
plot(cv_result)

cat("The value of lambda the yielded the lowest mean-squared error:", cv_result$lambda.min)
# The value of lambda the yielded the lowest mean-squared error: 0.002288081

# Doing the actual logistic regression



lasso_fit <- glmnet(x = X, y = training$stroke, lambda = 0.002288081, family = "binomial")
coef(lasso_fit)
#consider the issue with dummy variables for work type (delete rows with never worked — too unbalanced? can also keep. )
# Fit another model with different model selcetion algorithm (stpewise, forward, backward, p value) 

# Results: in the classification models, we can use:
#age, 
#hypertension, 
#heart_disease, 
#work_type, -
#avg_glucose_level and 
#smoking_status - 

# BUT, work_type is uncertain (same for smoking status), 
# because some options are set to have slope 0 (Govt_job, Never_worked), 
# while others options have a slope different than 0.
# Thus, we need to do other kind of model selection to see whether to include this variable or not.

# STEPWISE REGRESSION: FORWARD, BASED ON P-VALUE

summary(glm(stroke ~ gender, data = training, family = binomial(link = "logit")))$coefficients

summary(glm(stroke ~ age, data = training, family = binomial(link = "logit")))$coefficients

summary(glm(stroke ~ hypertension, data = training, family = binomial(link = "logit")))$coefficients

summary(glm(stroke ~ heart_disease, data = training, family = binomial(link = "logit")))$coefficients

summary(glm(stroke ~ ever_married, data = training, family = binomial(link = "logit")))$coefficients

summary(glm(stroke ~ work_type, data = training, family = binomial(link = "logit")))$coefficients

summary(glm(stroke ~ Residence_type, data = training, family = binomial(link = "logit")))$coefficients

summary(glm(stroke ~ avg_glucose_level, data = training, family = binomial(link = "logit")))$coefficients

summary(glm(stroke ~ bmi, data = training, family = binomial(link = "logit")))$coefficients
summary(glm(stroke ~ smoking_status, data = training, family = binomial(link = "logit")))$coefficients

# AGE HAS THE LOWEST P-VALUE, SO WE WILL CHOOSE THAT

summary(glm(stroke ~ age + gender, data = training, family = binomial(link = "logit")))$coefficients
summary(glm(stroke ~ age + hypertension, data = training, family = binomial(link = "logit")))$coefficients
summary(glm(stroke ~ age + heart_disease, data = training, family = binomial(link = "logit")))$coefficients
summary(glm(stroke ~ age + ever_married, data = training, family = binomial(link = "logit")))$coefficients
summary(glm(stroke ~ age + work_type, data = training, family = binomial(link = "logit")))$coefficients
summary(glm(stroke ~ age + Residence_type, data = training, family = binomial(link = "logit")))$coefficients
summary(glm(stroke ~ age + avg_glucose_level, data = training, family = binomial(link = "logit")))$coefficients
summary(glm(stroke ~ age + bmi, data = training, family = binomial(link = "logit")))$coefficients
summary(glm(stroke ~ age + smoking_status, data = training, family = binomial(link = "logit")))$coefficients

# AVG_GLUCOSE_LEVEL HAS THE LOWEST P-VALUE, SO WE WILL CHOOSE THAT

summary(glm(stroke ~ age + avg_glucose_level + gender, data = training, family = binomial(link = "logit")))$coefficients
summary(glm(stroke ~ age + avg_glucose_level + hypertension, data = training, family = binomial(link = "logit")))$coefficients
summary(glm(stroke ~ age + avg_glucose_level + heart_disease, data = training, family = binomial(link = "logit")))$coefficients
summary(glm(stroke ~ age + avg_glucose_level + ever_married, data = training, family = binomial(link = "logit")))$coefficients
summary(glm(stroke ~ age + avg_glucose_level + work_type, data = training, family = binomial(link = "logit")))$coefficients
summary(glm(stroke ~ age + avg_glucose_level + Residence_type, data = training, family = binomial(link = "logit")))$coefficients
summary(glm(stroke ~ age + avg_glucose_level + bmi, data = training, family = binomial(link = "logit")))$coefficients
summary(glm(stroke ~ age + avg_glucose_level + smoking_status, data = training, family = binomial(link = "logit")))$coefficients

# HYPERTENSION HAS THE LOWEST P-VALUE, SO WE WILL CHOOSE THAT

summary(glm(stroke ~ age + hypertension + avg_glucose_level + gender, data = training, family = binomial(link = "logit")))$coefficients
summary(glm(stroke ~ age + hypertension + avg_glucose_level + heart_disease, data = training, family = binomial(link = "logit")))$coefficients
summary(glm(stroke ~ age + hypertension + avg_glucose_level + ever_married, data = training, family = binomial(link = "logit")))$coefficients
summary(glm(stroke ~ age + hypertension + avg_glucose_level + work_type, data = training, family = binomial(link = "logit")))$coefficients
summary(glm(stroke ~ age + hypertension + avg_glucose_level + Residence_type, data = training, family = binomial(link = "logit")))$coefficients
summary(glm(stroke ~ age + hypertension + avg_glucose_level + bmi, data = training, family = binomial(link = "logit")))$coefficients
summary(glm(stroke ~ age + hypertension + avg_glucose_level + smoking_status, data = training, family = binomial(link = "logit")))$coefficients

# NO OTHER PARAMETERS HAVE A P-VALUE SMALLER THAN 0.05, SO WE WILL STOP HERE
# FINAL MODEL BY STEPWISE REGRESSION (FORWARDS) CONTAINS:
#age
#hypertension
# avg_glucose_level


# LOGISTIC REGRESSION 1: BASED ON PARAMETERS DISCOVERED BY LASSO
#age, 
#hypertension, 
#heart_disease, 
#work_type, -
#avg_glucose_level and 
#smoking_status
log_reg1 <- glm(stroke ~ age + hypertension + heart_disease + work_type + avg_glucose_level + smoking_status,
                data = training,
                family = binomial(link = "logit"))

summary(log_reg1)

library(ROCR)
pred = predict(log_reg1, testing, type="response")

predObj = prediction(pred, testing$stroke)
rocObj = performance(predObj, measure="tpr", x.measure="fpr") 
aucObj = performance(predObj, measure="auc")
plot(rocObj, main = paste("Area under the curve:", round(aucObj@y.values[[1]] ,4)))

library(caret)

pred = predict(log_reg1, testing, type = "response")
for (i in 1:981){
        if(pred[i] > 0.5){
                pred[i] = 1
        }
        else{
                pred[i] = 0
        }
}

pred <- factor(pred)

library(caret)
confusionMatrix(pred,factor(testing$stroke))



# LOGISTIC REGRESSION 2: BASED ON PARAMETERS DISCOVERED BY STEPWISE REGRESSION: FORWARD

log_reg2 <- glm(stroke ~ age + hypertension + avg_glucose_level,
                data = training,
                family = binomial(link = "logit"))

summary(log_reg2)

library(ROCR)
pred = predict(log_reg2, testing, type="response")

predObj = prediction(pred, testing$stroke)
rocObj = performance(predObj, measure="tpr", x.measure="fpr") 
aucObj = performance(predObj, measure="auc")
plot(rocObj, main = paste("Area under the curve:", round(aucObj@y.values[[1]] ,4)))

pred2 = predict(log_reg2, testing, type = "response")
for (i in 1:981){
        if(pred2[i] > 0.5){
                pred2[i] = 1
        }
        else{
                pred2[i] = 0
        }
}
pred2 <- factor(pred2)

library(caret)
confusionMatrix(pred2,factor(testing$stroke))

## DECISION TREE 1: USING THE PARAMETERS DISCOVERED BY LASSO
library(rpart)
library(rpart.plot)
fit <- rpart(stroke ~ age + hypertension + heart_disease + work_type  + avg_glucose_level + smoking_status,
             method ="class",
             data = stroke, 
             control=rpart.control(maxdepth=6,cp=0.001),
             parms=list(split='information'))
rpart.plot(fit, type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen=0)
pred <-predict(object = fit, stroke, type = "class")
t <- table(pred, stroke$stroke)
confusionMatrix(t)



## DECISION TREE 2: USING THE PARAMETERS DISCOVERED BY FORWARD SELECTION BASED ON P-VALUE
library(rpart)
library(rpart.plot)
fit2 <- rpart(stroke ~ age + hypertension + heart_disease+ avg_glucose_level,
              method ="class",
              data = stroke, 
              control=rpart.control(maxdepth=5,cp=0.001),
              parms=list(split='information'))
rpart.plot(fit2, type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen=0)
pred <-predict(object = fit2, stroke, type = "class")
t <- table(pred, stroke$stroke)
confusionMatrix(t)



