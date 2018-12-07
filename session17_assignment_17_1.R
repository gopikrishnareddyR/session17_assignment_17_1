#session17_assisgnment_17.1

#1. Use the below given data set
#Data Set
library(data.table)
getwd()
wle_ex<-fread("C:/Users/DELL/Documents/weight_lifting_exercises.csv")
wle_ex<-wle_ex[,-c(2:6,11:35,49:58,68:82,86:100,102:111,124:138,140:149)] 
library(psych) 
t<-View(describe(wle_ex)) 
sum(is.na(wle_ex)) 
summary(wle_ex) 
dim(wle_ex) 
pairs(wle_ex[,1:10])



library(ISLR)

smp_siz<-floor(0.75*nrow(wle_ex)) # creates a value for dividing the data into train and test. In this case the value is defined as 75% of the number of rows in the dataset smp_siz 
set.seed(123) # set seed to ensure you always have same random numbers generated 
wlet_ind= sample(seq_len(nrow(wle_ex)),size = smp_siz) # Randomly identifies the rows equal to sample size ( defined in previous instruction) from all the rows of Smarket dataset and stores the row number in train_ind 
wlet_train =wle_ex[wlet_ind,] #creates the training dataset with row numbers stored in train_ind 
View(wlet_train)
wlet_test=wle_ex[-wlet_ind,]

#2. Perform the below given activities:

### Ordinal logistic regression model


library(MASS)
wlet_train$classe<-as.ordered(wlet_train$classe)
wlet_test$classe<-as.ordered(wlet_test$classe)

wlet_test$classe<-as.factor(wlet_test$classe)
wlet_train$classe<-as.factor(wlet_train$classe)

mod<-polr(classe~gyros_belt_x+gyros_belt_y+gyros_belt_z,wlet_train, Hess = TRUE)
summary(mod)
ctable<-coef(summary(mod))
p<-pnorm(abs(ctable[,"t value"]), lower.tail = FALSE)*2
ctable<-cbind(ctable,"p value"=p)

predd<-predict(mod, wlet_train[1:5,-1], type = "prob")
print(predd, digits=3)

mod1<-polr(classe~roll_belt+pitch_belt+yaw_belt+total_accel_belt+
             roll_dumbbell+pitch_dumbbell+yaw_dumbbell+total_accel_dumbbell,wlet_train, Hess = TRUE)
summary(mod1)
colnames(wle_ex)
ctable<-coef(summary(mod1))
p<-pnorm(abs(ctable[,"t value"]), lower.tail = FALSE)*2
ctable<-cbind(ctable,"p value"=p)

predd<-predict(mod1, wlet_train[,-1], type = "prob")
print(predd, digits=3)


##################

# a. Create classification model using logistic regression model
library(nnet)
fit_lr<-multinom(classe~., data = wlet_train)

summary(fit_lr)

library(MASS)
step_fit<-stepAIC(fit_lr, method='backward')
summary(step_fit)


#b. verify model goodness of fit
table(wlet_train$classe)
chisq.test(table(wlet_train$classe))

library(ResourceSelection)
hoslem.test(mod1)
anova(step_fit, test = 'Chisq')



#c. Report the accuracy measures
control<-trainControl(method = 'repeatedcv', number = 10, repeats = 3)
set.seed(123)
metric<-'Accuracy'

fit_default<-train(classe~.,data = wlet_train,preProcess="scale", 
                   method='lvq',metric=metric,trControl=control)
fit_default

pred_link<-predict(fit_lr, newdata =wlet_test, type = 'class')

pred<-predict(fit_lr, newdata =wlet_test, type = 'probs')


#d. Report the variable importance
library(caret)
varImp(fit_lr, scale=false)


#e. Report the unimportant variables

library(car)
vif(fit_lr)
vif(step_fit)

#f. Interpret the results
confint(step_fit)



#g. Visualize the results

plot(fit_lr$fitted.values)
plot(fit_lr$residuals)

plot(step_fit$fitted.values)
plot(step_fit$residuals)



library(pROC)
g<-roc(classe~pred, data = wlet_test)
