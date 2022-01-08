load("C:/Users/1flor/Downloads/ECOM151-Assignment-Data.rda")
library(corrplot)
library(MASS)
library(class)
library(dplyr)
library(leaps)
library(tree) 
library(ISLR) 
library(glmnet)
library(randomForest)

names(testData)
head(testData)
summary(testData)

names(trainData)
head(trainData)
summary(trainData)


# Create a new variable in trainData called 'y' which takes the value = 1 if loan_status is 'Charged off' and 0 otherwise

y=ifelse(trainData$loan_status=="Charged Off",1,0)
trainData<-cbind.data.frame(trainData,as.data.frame(y))

y=ifelse(testData$loan_status=="Charged Off",1,0)
testData<-cbind.data.frame(testData,as.data.frame(y))

# All variables provided to you other than loan_status are referred to as 'predictors'
predictors = trainData[-c(1)] 

predictorsDnum = mutate_all(predictors, function(x) as.numeric(as.character(x)))
View(predictorsDnum)

gradeNum = as.numeric(recode_factor(predictors$grade, A="1", B="2", C="3", D="4", E="5", F="6", G="7")) # Altered the A-G scale to a numeric scale of 1-7, so that correlation can be seen. A=1 etc.
class(gradeNum)
apptypeNum = as.numeric(recode(predictors$application_type, Individual="1", "Joint App" = "2"))
class(apptypeNum)
emplengthNum = as.numeric(paste(recode(trainData$emp_length, "< 1 year"= "0", "1 year" = "1", "2 years" = "2", "3 years"= "3", "4 years"= "4", "5 years"= "5", "6 years"="6", "7 years"="7", "8 years"="8", "9 years"= "9", "10 years" = "10", "10+ years"= "11"))) 
class(emplengthNum)
homeNum = as.numeric(recode_factor(trainData$home_ownership, RENT = "1", MORTGAGE = "2", OWN= "3" ))
class(homeNum)

predictorsDnum<-cbind.data.frame(predictorsDnum,as.data.frame(gradeNum))
predictorsDnum<-cbind.data.frame(predictorsDnum,as.data.frame(apptypeNum))
predictorsDnum<-cbind.data.frame(predictorsDnum,as.data.frame(emplengthNum))
predictorsDnum<-cbind.data.frame(predictorsDnum,as.data.frame(homeNum))
predictorsDnum = subset(predictorsDnum, select = -c(grade, emp_length, application_type, home_ownership)) # getting rid of the non-numerical vectors
predictorsDnum=predictorsDnum[-c(which(is.na(predictorsDnum$emplengthNum))),] #getting rid of the n/a rows for the data set


testDnum = mutate_all(testData, function(x) as.numeric(as.character(x)))
gradeTestNum = as.numeric(recode_factor(testData$grade, A="1", B="2", C="3", D="4", E="5", F="6", G="7")) # Altered the A-G scale to a numeric scale of 1-7, so that correlation can be seen. A=1 etc.
class(gradeTestNum)
apptypeTestNum = as.numeric(recode(testData$application_type, "Individual"="1", "Joint App" = "2"))
class(apptypeTestNum)
emplengthTestNum = as.numeric(paste(recode(testData$emp_length, "< 1 year"= "0", "1 year" = "1", "2 years" = "2", "3 years"= "3", "4 years"= "4", "5 years"= "5", "6 years"="6", "7 years"="7", "8 years"="8", "9 years"= "9", "10 years" = "10", "10+ years"= "11"))) 
class(emplengthTestNum)
hometestNum = as.numeric(recode_factor(testData$home_ownership, RENT = "1", MORTGAGE = "2", OWN= "3" ))
class(hometestNum)

testDnum<-cbind.data.frame(testDnum,as.data.frame(gradeTestNum))
testDnum<-cbind.data.frame(testDnum,as.data.frame(apptypeTestNum))
testDnum<-cbind.data.frame(testDnum,as.data.frame(emplengthTestNum))
testDnum<-cbind.data.frame(testDnum,as.data.frame(hometestNum))
testDnum = subset(testDnum, select = -c(grade, emp_length, application_type, home_ownership, loan_status)) # getting rid of the non-numerical vectors
testDnum=testDnum[-c(which(is.na(testDnum$emplengthTestNum))),]


# Find the top 10 positively correlated variables with y and store it as a4
corgraph = cor(predictorsDnum$y, predictorsDnum)
corrplot(corgraph, method= "number", type="upper", col = "black") #zelf de 10 grootste toewijzen aan a4

a4=predictorsDnum[c(1:3, 5, 7, 17:20, 28)] #gradeNum has a corr of 0.25 kicking out, revol_util with a corr of 0.4


# Find the top 10 negatively correlated variables with y and store it as a5
a5 = predictorsDnum[c(4, 11, 14:16, 21, 25, 26, 30, 31)]


#LINEAR REGRESSION

LRM = lm(predictorsDnum$y~predictorsDnum$homeNum+predictorsDnum$loan_amnt+predictorsDnum$int_rate+predictorsDnum$installment+predictorsDnum$gradeNum+predictorsDnum$emplengthNum+predictorsDnum$annual_inc+predictorsDnum$dti+predictorsDnum$delinq_2yrs+predictorsDnum$mths_since_last_delinq+predictorsDnum$open_acc+predictorsDnum$pub_rec+predictorsDnum$revol_bal+predictorsDnum$revol_util+predictorsDnum$total_acc+predictorsDnum$total_pymnt+predictorsDnum$total_pymnt_inv+predictorsDnum$total_rec_prncp+predictorsDnum$total_rec_int+predictorsDnum$total_rec_late_fee+predictorsDnum$recoveries+predictorsDnum$collection_recovery_fee+predictorsDnum$last_pymnt_amnt+predictorsDnum$collections_12_mths_ex_med+predictorsDnum$acc_now_delinq+predictorsDnum$tot_coll_amt+predictorsDnum$tot_cur_bal+predictorsDnum$total_rev_hi_lim+predictorsDnum$apptypeNum+predictorsDnum$inq_last_6mths, data = predictorsDnum)
summary(LRM)
plot(LRM)
yhat=predict(LRM, predictorsDnum)
yhatBin=ifelse(yhat>0.5,1,0)
Htest = table(yhatBin, predictorsDnum$y)
print(Htest)
m1.1=mean((yhatBin-predictorsDnum$y)^2)
class(m1.1)
View(m1.1)
class(LRMhat)
class(predictorsDnum)
class(trainData$y)

LRMtest = lm((testDnum$y~testDnum$hometestNum+testDnum$loan_amnt+testDnum$int_rate+testDnum$installment+testDnum$annual_inc+testDnum$dti+testDnum$delinq_2yrs+testDnum$inq_last_6mths+testDnum$mths_since_last_delinq+testDnum$open_acc+testDnum$pub_rec+testDnum$revol_bal+testDnum$revol_util+testDnum$total_acc+testDnum$total_pymnt+testDnum$total_pymnt_inv+testDnum$total_rec_prncp+testDnum$total_rec_int+testDnum$total_rec_late_fee+testDnum$recoveries+testDnum$collection_recovery_fee+testDnum$last_pymnt_amnt+testDnum$collections_12_mths_ex_med+testDnum$acc_now_delinq+testDnum$tot_coll_amt+testDnum$tot_cur_bal+testDnum$total_rev_hi_lim+testDnum$gradeTestNum+testDnum$apptypeTestNum+testDnum$emplengthTestNum))
summary(LRM)
plot(LRM)
testHat=predict(LRMtest, testDnum)
testhatBin=ifelse(testHat>0.5,1,0)
Htest = table(testhatBin, testDnum$y)
print(Htest)
m1.2=mean((testhatBin-testDnum$y)^2)


# BEST SUBSET MODEL
unconstrained= regsubsets(predictorsDnum$y~.,predictorsDnum$loan_amnt+predictorsDnum$int_rate+predictorsDnum$installment+predictorsDnum$gradeNum+predictorsDnum$emplengthNum+predictorsDnum$annual_inc+predictorsDnum$dti+predictorsDnum$delinq_2yrs+predictorsDnum$mths_since_last_delinq+predictorsDnum$open_acc+predictorsDnum$pub_rec+predictorsDnum$revol_bal+predictorsDnum$revol_util+predictorsDnum$total_acc+predictorsDnum$total_pymnt+predictorsDnum$total_pymnt_inv+predictorsDnum$total_rec_prncp+predictorsDnum$total_rec_int+predictorsDnum$total_rec_late_fee+predictorsDnum$recoveries+predictorsDnum$collection_recovery_fee+predictorsDnum$last_pymnt_amnt+predictorsDnum$collections_12_mths_ex_med+predictorsDnum$acc_now_delinq+predictorsDnum$tot_coll_amt+predictorsDnum$tot_cur_bal+predictorsDnum$total_rev_hi_lim+predictorsDnum$apptypeNum+predictorsDnum$inq_last_6mths+predictorsDnum$homeNum ,data=predictorsDnum, nvmax=30)
unconSum= summary(unconstrained)
unconSum$adjr2

forward= regsubsets(predictorsDnum$y~.,predictorsDnum$loan_amnt+predictorsDnum$int_rate+predictorsDnum$installment+predictorsDnum$gradeNum+predictorsDnum$emplengthNum+predictorsDnum$annual_inc+predictorsDnum$dti+predictorsDnum$delinq_2yrs+predictorsDnum$mths_since_last_delinq+predictorsDnum$open_acc+predictorsDnum$pub_rec+predictorsDnum$revol_bal+predictorsDnum$revol_util+predictorsDnum$total_acc+predictorsDnum$total_pymnt+predictorsDnum$total_pymnt_inv+predictorsDnum$total_rec_prncp+predictorsDnum$total_rec_int+predictorsDnum$total_rec_late_fee+predictorsDnum$recoveries+predictorsDnum$collection_recovery_fee+predictorsDnum$last_pymnt_amnt+predictorsDnum$collections_12_mths_ex_med+predictorsDnum$acc_now_delinq+predictorsDnum$tot_coll_amt+predictorsDnum$tot_cur_bal+predictorsDnum$total_rev_hi_lim+predictorsDnum$apptypeNum+predictorsDnum$inq_last_6mths+predictorsDnum$homeNum ,data=predictorsDnum, nvmax=30, method="forward")
forwardSum= summary(forward)
forwardSum$adjr2

backward= regsubsets(predictorsDnum$y~.,predictorsDnum$loan_amnt+predictorsDnum$int_rate+predictorsDnum$installment+predictorsDnum$gradeNum+predictorsDnum$emplengthNum+predictorsDnum$annual_inc+predictorsDnum$dti+predictorsDnum$delinq_2yrs+predictorsDnum$mths_since_last_delinq+predictorsDnum$open_acc+predictorsDnum$pub_rec+predictorsDnum$revol_bal+predictorsDnum$revol_util+predictorsDnum$total_acc+predictorsDnum$total_pymnt+predictorsDnum$total_pymnt_inv+predictorsDnum$total_rec_prncp+predictorsDnum$total_rec_int+predictorsDnum$total_rec_late_fee+predictorsDnum$recoveries+predictorsDnum$collection_recovery_fee+predictorsDnum$last_pymnt_amnt+predictorsDnum$collections_12_mths_ex_med+predictorsDnum$acc_now_delinq+predictorsDnum$tot_coll_amt+predictorsDnum$tot_cur_bal+predictorsDnum$total_rev_hi_lim+predictorsDnum$apptypeNum+predictorsDnum$inq_last_6mths+predictorsDnum$homeNum, data = predictorsDnum, nvmax=30, method="backward")
backwardSum= summary(backward)
backwardSum$adjr2

which.max(unconSum$adjr2)
which.max(forwardSum$adjr2)
which.max(backwardSum$adjr2)

coef(unconstrained,28)
coef(forward,28)
coef(backward,28)

unconstrainedTest= regsubsets(testDnum$y~.,testDnum$loan_amnt+testDnum$int_rate+testDnum$installment+testDnum$gradeTestNum+testDnum$emplengthTestNum+testDnum$annual_inc+testDnum$dti+testDnum$delinq_2yrs+testDnum$mths_since_last_delinq+testDnum$open_acc+testDnum$pub_rec+testDnum$revol_bal+testDnum$revol_util+testDnum$total_acc+testDnum$total_pymnt+testDnum$total_pymnt_inv+testDnum$total_rec_prncp+testDnum$total_rec_int+testDnum$total_rec_late_fee+testDnum$recoveries+testDnum$collection_recovery_fee+testDnum$last_pymnt_amnt+testDnum$collections_12_mths_ex_med+testDnum$acc_now_delinq+testDnum$tot_coll_amt+testDnum$tot_cur_bal+testDnum$total_rev_hi_lim+testDnum$apptypeTestNum+testDnum$inq_last_6mths+testDnum$hometestNum ,data=testDnum, nvmax=31)
untestSum= summary(unconstrainedTest)
untestSum$adjr2

forwardTest= regsubsets(testDnum$y~.,testDnum$hometestNum+testDnum$loan_amnt+testDnum$int_rate+testDnum$installment+testDnum$annual_inc+testDnum$dti+testDnum$delinq_2yrs+testDnum$inq_last_6mths+testDnum$mths_since_last_delinq+testDnum$open_acc+testDnum$pub_rec+testDnum$revol_bal+testDnum$revol_util+testDnum$total_acc+testDnum$total_pymnt+testDnum$total_pymnt_inv+testDnum$total_rec_prncp+testDnum$total_rec_int+testDnum$total_rec_late_fee+testDnum$recoveries+testDnum$collection_recovery_fee+testDnum$last_pymnt_amnt+testDnum$collections_12_mths_ex_med+testDnum$acc_now_delinq+testDnum$tot_coll_amt+testDnum$tot_cur_bal+testDnum$total_rev_hi_lim+testDnum$gradeTestNum+testDnum$apptypeTestNum+testDnum$emplengthTestNum, data = testDnum, nvmax = 31, method= "forward")
fortestSum= summary(forwardTest)
fortestSum$adjr2

backwardTest= regsubsets(testDnum$y~.,testDnum$hometestNum+testDnum$loan_amnt+testDnum$int_rate+testDnum$installment+testDnum$annual_inc+testDnum$dti+testDnum$delinq_2yrs+testDnum$inq_last_6mths+testDnum$mths_since_last_delinq+testDnum$open_acc+testDnum$pub_rec+testDnum$revol_bal+testDnum$revol_util+testDnum$total_acc+testDnum$total_pymnt+testDnum$total_pymnt_inv+testDnum$total_rec_prncp+testDnum$total_rec_int+testDnum$total_rec_late_fee+testDnum$recoveries+testDnum$collection_recovery_fee+testDnum$last_pymnt_amnt+testDnum$collections_12_mths_ex_med+testDnum$acc_now_delinq+testDnum$tot_coll_amt+testDnum$tot_cur_bal+testDnum$total_rev_hi_lim+testDnum$gradeTestNum+testDnum$apptypeTestNum+testDnum$emplengthTestNum, data = testDnum, nvmax = 31, method= "backward")
backtestSum= summary(backwardTest)
backtestSum$adjr2

which.max(untestSum$adjr2)
which.max(fortestSum$adjr2)
which.max(backtestSum$adjr2)

coef(unconstrainedTest,27)
coef(forwardTest,27)
coef(backwardTest,27)


# What are the variables in the "best" model of this class? Store this in object named m2.3.


# RIDGE REGRESSION MODEL
ridgeTrain=model.matrix(predictorsDnum$y~.,predictorsDnum)[,-1]
trainy=predictorsDnum$y

grid=10^seq(10,-2,length=100)
ridge=glmnet(ridgeTrain, trainy,alpha=0,lambda=grid, thresh=1e-12) 
dim(ridgeTrain)

set.seed(1)
cvRidge = cv.glmnet(ridgeTrain, trainy, alpha = 0,lambda=grid, thresh=1e-12)

bestlam = cvRidge$lambda.min  # Select lamda that minimizes training MSE
View(bestlam)
predTrain=predict(ridge,s=bestlam,newx=ridgeTrain)
m3.1= mean((predTrain-trainy)^2)

ridgeTest=model.matrix(testDnum$y~., testDnum)[,-1]
testy=testDnum$y
predTest=predict(ridge, s=bestlam, newx=ridgeTest)
m3.2= mean((predTest-testy)^2)

ridgeCoef=predict(ridge,type="coefficients",s=bestlam)
m3.3=order(as.numeric(ridgeCoef),decreasing=TRUE)[1:10]
View(m3.3)
ridgeCoef

#LASSO REGRESSION
lasso=glmnet(ridgeTrain, trainy, alpha=1, lambda=grid)
set.seed(1)
cvLasso= cv.glmnet(ridgeTrain, trainy, alpha = 1)
bestlam= cvLasso$lambda.min  
View(bestlam)
lassoTrain=predict(lasso, s=bestlam, newx=ridgeTrain)
m4.1=mean((lassoTrain-trainy)^2)

lassoTest=glmnet(ridgeTest, testy, alpha=1, lambda=grid)
set.seed(1)
cvLassoTest= cv.glmnet(ridgeTest, testy, alpha = 1)
bestlamTest= cvLassoTest$lambda.min 
lassoPred=predict(lassoTest, s=bestlam, newx=ridgeTest)
m4.2=mean((lassoPred-testy)^2)

lassoData=glmnet(ridgeTrain, trainy, alpha=1, lambda=grid)
lassoCoef=predict(lassoData, type="coefficients", s=bestlam)[1:20,]
lassoCoef

m4.3=lassoCoef[lassoCoef!=0]

#Random Forest
bag=randomForest(y=factor(trainy, levels=c('0','1')), x=ridgeTrain, ntree=50)
bag

baghat=as.numeric(predict(bag))-1
m5.1=mean((as.numeric(baghat)-predictorsDnum$y)^2)

bag=randomForest(y=factor(testy, levels=c('0','1')), x=ridgeTest, ntree=50)
ybaghat=as.numeric(predict(bag))-1
m5.2=mean((as.numeric(ybaghat)-testDnum$y)^2)
m5.3=importance(bag)


#Optimal Model
finalModel= ybaghat

# END OF ASSIGNMENT