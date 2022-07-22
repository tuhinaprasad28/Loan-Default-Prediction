library(readxl)
library(stringdist)
library(dplyr)
library(tibble)
library(usdata)
library(stats)
library(Rcmdr)
library(caret)
library(corrplot)
library(plyr)
library(ROSE)
library(rpart)
library(rpart.plot)
library(nnet)
library(MASS)
library(devtools)
library(mlr)
library(InformationValue)
library(reshape)
library(Metrics)
library(ggplot2)
library(neuralnet)

mortgage= read_excel("Mortgage.xls", sheet = "MortgageDefaulters")
statedata_orig = read_excel("Mortgage.xls", sheet = "stateData")
statedata_orig = statedata_orig[-1,]
colnames(statedata_orig)[1]="State"



names(state.abb) <- state.name   
rep= ifelse((statedata_orig$State == "District of Columbia"), state2abbr("District of Columbia"), state.abb[statedata_orig$State])
statedata_orig$State=rep
statedata_orig$State

#------------------------------------question (a)------------------------------------------------------------------------------------------

statedata_merged= merge(mortgage, statedata_orig, by.x = "State")
statedata_merged

#-----------------------------------------------------------------------------------------------------------------------------
#------------cleaning the data-----------------------------------------

loan_default <- statedata_merged[ , colSums(is.na(statedata_merged)) < nrow(statedata_merged)]  # Remove columns with NA only
loan_default=na.omit(loan_default)

colnames(loan_default)[16]= "Avg_Median_Income"
colnames(loan_default)[17]= "Per_People_Poverty"
colnames(loan_default)[14]= "UPB_Appraisal"

#Making sure categorical variables are stored as factors and numerical variables are stored as numericals.
#In the program below, we are converting variables to factors and numericals.

loan_default$State=as.factor(loan_default$State)
loan_default$First_home=as.factor(loan_default$First_home)
loan_default$Status=as.factor(loan_default$Status)
loan_default$OUTCOME=as.factor(loan_default$OUTCOME)
loan_default$UPB_Appraisal=as.factor(loan_default$UPB_Appraisal)


#Avg Median To Numeric
loan_default$Avg_Median_Income = as.numeric(loan_default$Avg_Median_Income)

#Per People Poverty to Numeric
loan_default$Per_People_Poverty = as.numeric(loan_default$Per_People_Poverty)

str(loan_default)


# Showing  the distribution of values in the attributes and how they individually relate to the outcome of interest (dependent variable)
meltData <- melt(loan_default)
p <- ggplot(meltData, aes(factor(variable), value))
p + geom_boxplot() + facet_wrap(~variable, scale="free")
#-------------------------------------question (b,c,d)------------------------------------------------------------------------------------------------------------

#Removing variables with low variance and create a new dataset

variances<-apply(loan_default,2, var)
variances
rem_var= variances[which(variances<=0.050)]
rem_var
loan_final=loan_default[-11]
str(loan_final)


#Dropping dependent variable for calculating Multicollinearity
loan_subset = subset(loan_final, select = -c(OUTCOME))

#Identifying numeric variables
numericData <- loan_subset[sapply(loan_subset, is.numeric)]

#Calculating Correlation
descrCorr <- cor(numericData)

# Print correlation matrix and look at max correlation
print(descrCorr)

# Visualize Correlation Matrix
corrplot(descrCorr, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))

# Checking Variables that are highly correlated
highlyCorrelated = findCorrelation(descrCorr, cutoff=0.7)


#Identifying Variable Names of Highly Correlated Variables
highlyCorvar = colnames(numericData)[highlyCorrelated]

#Print highly correlated attributes
highlyCorvar

 
#Remove highly correlated variables and create a new dataset
Loan_Orig = loan_final[, -which(colnames(loan_final) %in% highlyCorvar)]
dim(Loan_Orig)
str(Loan_Orig)

#---------------Build Linear Regression Model---------------------------------------------------------------------
###Converting Outcome to Numeric

Outcome = as.numeric(ifelse(Loan_Orig$OUTCOME == "default", 1 , 0))
Loan_Orig1= data.frame(Loan_Orig, Outcome)
Loan_Orig1 = Loan_Orig1[-11]
str(Loan_Orig1)

Linear_mod <- Loan_Orig1[sapply(Loan_Orig1, is.numeric)]

set.seed(123)
indx1= sample(2,nrow(Linear_mod),replace= TRUE, prob=c(0.75,0.25))
train_data=Linear_mod[indx1==1,]
test_data=Linear_mod[indx1==2,]

str(Linear_mod)

fit = lm(Outcome ~ ., data=train_data)

#Prediction of Linear Model
pred_lmmodel= predict(fit, newdata=test_data)
Accuracy_lmmodel=mean(test_data$Outcome - pred_lmmodel)^2 #Mean Squared Error
Accuracy_lmmodel

#Check Model Performance
summary(fit)

#Extracting Coefficients
summary(fit)$coeff

par(mfrow=c(2,2))
plot(fit)

########################################Calculating Model Performance for Linear#######################################################

#Extracting R-squared value
summary(fit)$r.squared


#Extracting Adjusted R-squared value
summary(fit)$adj.r.squared


AIC(fit)

# -----------------------------------------------Logistic Regression Model------------------------------------------------------------------

#logistic Model

set.seed(123)
indx= sample(2,nrow(Loan_Orig),replace= TRUE, prob=c(0.75,0.25))
train_data_log=Loan_Orig[indx==1,]
test_data_log=Loan_Orig[indx==2,]

logitmodel= glm(OUTCOME ~., data=train_data_log, family="binomial")

coef(logitmodel)

summary(logitmodel)

layout(matrix(c(1,2,3,4),2,2))
plot(logitmodel)
#Prediction
pred_logit= predict(logitmodel, newdata=test_data_log, type= "response")

Class= ifelse(pred_logit>=0.5, "default","non-default")

table(Class)

#######################################Variable Selection########################################################################


#_________Step-wise Selection___________________________

full= glm(Outcome~.,data= Linear_mod, binomial("logit"),maxit=100)

stepMod=full %>% stepAIC(Trace=FALSE)

summary(full)

# Get the shortlisted variable.
shortlistedVars <- names(unlist(stepMod[[1]])) 
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"] # remove intercept
print(shortlistedVars)
str(Linear_mod)

newdf= data.frame(Loan_Orig$OUTCOME,Loan_Orig$Credit_score,Loan_Orig$Tot_mthly_debt_exp,Loan_Orig$LoanValuetoAppraised,Loan_Orig$Per_People_Poverty)

names(newdf) <- c('OUTCOME', 'Credit_score', 'Tot_mthly_debt_exp','LoanValuetoAppraised','Per_People_Poverty')
str(newdf)

# We will include these variables in our model: Outcome, Credit_score, 
# Tot_mthly_debt_exp, LoanValuetoAppraised and Per_People_poverty because AIC for these variables are the lowest

# Showing the distribution of values after sorting the the attributes and how they individually relate to the outcome of interest (dependent variable)

meltDatadf <- melt(newdf)
plotting <- ggplot(meltDatadf, aes(factor(variable), value))
plotting + geom_boxplot() + facet_wrap(~variable, scale="free")

#--------------------------------------------------------------------------------------------------------------------------
################################# Question 2#########################################################################################

###############################Balancing the data through under-sampling#############################################################################################
num_loan = unlist(lapply(newdf,is.numeric))
num_loan

loan_num= newdf[,num_loan]
str(loan_num)

mins = apply(loan_num, 2 , min)
maxs = apply(loan_num, 2 , max)
scaled= as.data.frame(scale(loan_num, center = mins, scale = maxs - mins))
loan_final = data.frame(scaled, newdf[!num_loan])
summary(loan_final)
str(loan_final)


set.seed(123)
indxbal= sample(2,nrow(loan_final),replace= TRUE, prob=c(0.75,0.25))
training=loan_final[indx1==1,]
testing=loan_final[indx1==2,]
str(training)
table(testing$OUTCOME)

Decision_train = rpart(OUTCOME~., data = training, method = "class")
rpart.plot(Decision_train)

TrgA <- ovun.sample(OUTCOME~., data=training,
                              p=0.3,
                              method="under")$data

TrgB <- ovun.sample(OUTCOME~., data=training,
                    p=0.1,
                    seed=1, method="under")$data


table(TrgA$OUTCOME)

table(TrgB$OUTCOME)


#____________________________________Decision Tree Models for TrgA And TrgB______________________________________________


Decision_Tree_TrgA = rpart(OUTCOME~., data = TrgA, method = "class", parms = list(split = "information"), control = rpart.control(minsplit = 0, minbucket = 0, cp = 0.01))
rpart.plot(Decision_Tree_TrgA)


Decision_Tree_TrgB = rpart(OUTCOME~., data = TrgB, method = "class", parms = list(split = "information"), control = rpart.control(minsplit = 0, minbucket = 0, cp = 0.0016))
rpart.plot(Decision_Tree_TrgB)

predict_TrgA <- predict(Decision_Tree_TrgA, testing, type = "prob")
pred_class_dtA= ifelse(predict_TrgA>=0.5, "default","non-default")
accuracydtA <- c(mean(testing$OUTCOME == pred_class_dtA))
accuracydtA

predict_TrgB <- predict(Decision_Tree_TrgB, testing, type = "prob")
pred_class_dtB= ifelse(predict_TrgB>=0.5, "default","non-default")
accuracydtB <- c(mean(testing$OUTCOME == pred_class_dtB))
accuracydtB

###########ROC Curve and LIft Chart for DT TrgA##########################################################

pred_tree = prediction(predict_TrgA[,2], testing$OUTCOME)
Roc_dt = performance(pred_tree, "tpr", "fpr")

plot(Roc_dt,colorize=T,
     main="ROC Curve for Decision Tree"
)

abline(a=0, b=1)

auc_dt <-performance(pred_tree,"auc")
auc_dt <-unlist(slot(auc_dt,"y.values"))
auc_dt

minauc_dt = min(round(auc_dt, digits = 4))
maxauc_dt = max(round(auc_dt, digits = 4))
minauct_dt = paste(c("min(AUC) = "), minauc_dt, sep = "")
maxauct_dt = paste(c("max(AUC) = "), maxauc_dt, sep = "")
legend(0.7, 0.5, c(minauct_dt, maxauct_dt, "\n"), border = "white", cex = 0.6, box.col = "blue")
abline(a= 0, b=1)


##Lift CHart for TrgA

Lift_dt <- performance(pred_tree,"lift","rpp")
plot(Lift_dt, main="lift curve", colorize=T)


########################################################################################################

####################################ROC curve for DT TrgB###############################################

pred_tree_B = prediction(predict_TrgB[,2], testing$OUTCOME)
Roc_dt_B = performance(pred_tree_B, "tpr", "fpr")

plot(Roc_dt_B,colorize=T,
     main="ROC Curve for Decision Tree"
)



abline(a=0, b=1)

auc_dt_B <-performance(pred_tree_B,"auc")
auc_dt_B <-unlist(slot(auc_dt_B,"y.values"))
auc_dt_B

minauc_dt_B = min(round(auc_dt_B, digits = 4))
maxauc_dt_B = max(round(auc_dt_B, digits = 4))
minauct_dt_B = paste(c("min(AUC) = "), minauc_dt_B, sep = "")
maxauct_dt_B = paste(c("max(AUC) = "), maxauc_dt_B, sep = "")
legend(0.7, 0.5, c(minauct_dt_B, maxauct_dt_B, "\n"), border = "white", cex = 0.6, box.col = "blue")
abline(a= 0, b=1)


##Lift Chart for TrgB

Lift_dt_B <- performance(pred_tree_B,"lift","rpp")
plot(Lift_dt_B, main="lift curve", colorize=T)


#______________________________ Regression Models for TrgA And TrgB________________________________________________
#logistic Model for TrgA

logitmodelA= glm(OUTCOME ~., data=TrgA, family=binomial)
summary(logitmodelA)

layout(matrix(c(1,2,3,4),2,2))
plot(logitmodelA)

#Prediction

pred_logitA= predict(logitmodelA, testing, type= "response")

pred_class= ifelse(pred_logitA>0.5, "default","non-default")
mean(testing$OUTCOME == pred_class)

#-----------logistic Model for TrgB--------------------------------------------


logitmodelB= glm(OUTCOME ~., data=TrgB, family="binomial")

layout(matrix(c(1,2,3,4),2,2))
plot(logitmodelB)


#Prediction

pred_logitB= predict(logitmodelB, testing, type= "response")

pred_classB= ifelse(pred_logitB>0.5, "default","non-default")

mean(testing$OUTCOME == pred_classB)

###########################################ROC curve for Logistic Regression###########################


log_predict <- predict(logitmodelA,newdata = testing,type = "response")
log_predict_A <- ifelse(log_predict > 0.5,1,0)
pr <- prediction(log_predict_A,testing$OUTCOME)
perf_log <- performance(pr,measure = "tpr",x.measure = "fpr")
plot(perf_log)

abline(a=0, b=1)

auc_log_A <-performance(pr,"auc")
auc_log_A <-unlist(slot(auc_log_A,"y.values"))

minauc_log_A = min(round(auc_log_A, digits = 2))
minauct_log_A = paste(c("min(AUC) = "), minauc_log_A, sep = "")
legend(0.7, 0.5, c(minauct_log_A, "\n"), border = "white", cex = 0.6, box.col = "blue")
abline(a= 0, b=1)


##Lift Chart for TrgA

Lift_log <- performance(pr,"lift","rpp")
plot(Lift_log, main="lift curve", colorize=T)

##########################Roc curve and Lift Chart of Logistic Regression for TrgB#############################################

log_predictB <- predict(logitmodelB,newdata = testing,type = "response")


table(Actualvalue=testing$OUTCOME,Predictedvalue=log_predictB>0.5)

ROCR_pred_test <- prediction(log_predictB,testing$OUTCOME)
perf_logB <- performance(ROCR_pred_test,measure = "tpr",x.measure = "fpr")
plot(perf_log)

auc_log_B <-performance(ROCR_pred_test,"auc")
auc_log_B <-unlist(slot(auc_log_B,"y.values"))
auc_log_B

minauc_log_B = min(round(auc_log_B, digits = 2))
minauct_log_B = paste(c("min(AUC) = "), minauc_log_B, sep = "")



Lift_logB <- performance(ROCR_pred_test,"lift","rpp")
plot(Lift_logB, main="lift curve",colorize=T)

#########################################################################################################


#____________________________________ Neural Models for TrgA And TrgB________________________________________________

#Normalizing TrgA

num_loanA = unlist(lapply(TrgA,is.numeric))
num_loanA

loan_numA= TrgA[,num_loanA]
str(loan_numA)

mins = apply(loan_numA, 2 , min)
maxs = apply(loan_numA, 2 , max)
scaledA= as.data.frame(scale(loan_numA, center = mins, scale = maxs - mins))
loan_finalA = data.frame(scaledA, TrgA[!num_loanA])
summary(loan_finalA)

#Neural Net for TrgA
nnA=nnet(OUTCOME~., data= loan_finalA, size = 5, decay = 0.001)


summary(nnA)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nnA)

#Prediction for TrgA
predict_nnA <- predict(nnA, testing, type = "raw")
predict_nnAA= ifelse(predict_nnA>=0.5, "default","non-default")
accuracynnA <- c(mean(testing$OUTCOME == predict_nnAA))
accuracynnA

#Normalizing TrgB
num_loanB = unlist(lapply(TrgB,is.numeric))
num_loanB

loan_numB= TrgB[,num_loanB]
str(loan_numB)

minsB = apply(loan_numB, 2 , min)
maxsB = apply(loan_numB, 2 , max)
scaledB= as.data.frame(scale(loan_numB, center = minsB, scale = maxsB - minsB))
loan_finalB = data.frame(scaledB, TrgB[!num_loanB])
summary(loan_finalB)

#Neural Net for TrgB
nnB=nnet(OUTCOME~., data= loan_finalB, size = 5,decay = 0.01)
summary(nnB)

source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nnB)

#Prediction for TrgB
predict_nnB <- predict(nnB, testing, type = "raw")
predict_nnB= ifelse(predict_nnB>=0.5, "default","non-default")
accuracynnB <- c(mean(testing$OUTCOME == predict_nnB))
accuracynnB
####################################ROC Curve and Lift Chart TrgA##########################################

nnA_predict <- predict(nnA,newdata = testing,type = "raw")
nn_predict_A <- ifelse(nnA_predict > 0.5,1,0)

prnnA <- prediction(nn_predict_A,testing$OUTCOME)

perf_prnnA <- performance(prnnA,measure = "tpr",x.measure = "fpr")
plot(perf_prnnA)


abline(a=0, b=1)

auc_nn_A <-performance(prnnA,"auc")
auc_nn_A <-unlist(slot(auc_nn_A,"y.values"))
auc_nn_A

minauc_nn_A = min(round(auc_nn_A, digits = 2))
maxauc_nn_A = max(round(auc_nn_A, digits = 2))
minauct_nn_A = paste(c("min(AUC) = "), minauc_nn_A, sep = "")
maxauct_nn_A = paste(c("max(AUC) = "), maxauc_nn_A, sep = "")
legend(0.7, 0.5, c(minauct_nn_A, maxauct_nn_A, "\n"), border = "white", cex = 0.6, box.col = "blue")
abline(a= 0, b=1)


##Lift Chart for TrgA

Lift_nnA <- performance(prnnA,"lift","rpp")
plot(Lift_nnA, main="lift curve", colorize=T)

####################################ROC Curve and Lift Chart TrgB##########################################

nnB_predict <- predict(nnB,newdata = testing,type = "raw")
nn_predict_B <- ifelse(nnB_predict > 0.5,1,0)
prnnB <- prediction(nn_predict_B,testing$OUTCOME)
perf_prnnB <- performance(prnnB,measure = "tpr",x.measure = "fpr")
plot(perf_prnnB)


abline(a=0, b=1)

auc_nn_B <-performance(prnnB,"auc")
auc_nn_B <-unlist(slot(auc_nn_B,"y.values"))
auc_nn_B

minauc_nn_B = min(round(auc_nn_B, digits = 2))
maxauc_nn_B = max(round(auc_nn_B, digits = 2))
minauct_nn_B = paste(c("min(AUC) = "), minauc_nn_B, sep = "")
maxauct_nn_B = paste(c("max(AUC) = "), maxauc_nn_B, sep = "")
legend(0.7, 0.5, c(minauct_nn_B, maxauct_nn_B, "\n"), border = "white", cex = 0.6, box.col = "blue")
abline(a= 0, b=1)


##Lift Chart for TrgA

Lift_nnB <- performance(prnnB,"lift","rpp")
plot(Lift_nnB, main="lift curve", colorize=T)

