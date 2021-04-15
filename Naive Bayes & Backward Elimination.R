
stroke <- healthcare.dataset.stroke.data
stroke$bmi <- as.numeric(stroke$bmi)
stroke <- na.omit(stroke)
shuffled_index <- sample(1:4909, 4909, replace = F)
stroke <- stroke[shuffled_index,]
set.seed(1)
seq <- sample(1:4909, 981, replace = F)
testing <- stroke[seq, ]
training <- stroke[-seq, ]

back <- glm(stroke ~ 
              gender + 
              age + 
              hypertension + 
              heart_disease + 
              work_type  + 
              avg_glucose_level + 
              smoking_status + 
              ever_married + 
              Residence_type + 
              bmi, data = training)
summary(back)

back <- glm(stroke ~ 
              age + 
              hypertension + 
              heart_disease + 
              work_type  + 
              avg_glucose_level + 
              smoking_status + 
              ever_married + 
              Residence_type + 
              bmi, data = training)
summary(back)

back <- glm(stroke ~ 
              age + 
              hypertension + 
              heart_disease + 
              work_type  + 
              avg_glucose_level + 
              ever_married + 
              Residence_type + 
              bmi, data = training)
summary(back)

back <- glm(stroke ~ 
              age + 
              hypertension + 
              heart_disease + 
              work_type  + 
              avg_glucose_level + 
              ever_married + 
              bmi, data = training)
summary(back)

back <- glm(stroke ~ 
              age + 
              hypertension + 
              heart_disease + 
              work_type  + 
              avg_glucose_level + 
              ever_married,
              data = training)
summary(back)

library(e1071)
naive1 <- naiveBayes(stroke ~ age + hypertension + heart_disease + work_type  + avg_glucose_level + smoking_status  , 
           data = training , 
           laplace = 0.01, 
           na.action = na.pass)
library(caTools)
pred <-predict(object = naive1, testing, type = "class")
t <- table(testing$stroke,pred)
library(caret,quietly = TRUE)
confusionMatrix(t)

naive2 <- naiveBayes(stroke ~ age + hypertension + avg_glucose_level  , 
                     data = training , 
                     laplace = 0.01, 
                     na.action = na.pass)
pred <-predict(object = naive2, testing, type = "class")
t <- table(testing$stroke,pred)
confusionMatrix(t)
