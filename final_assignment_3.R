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
library(tidyverse)
library(caret)
library(leaps)
library(e1071)
library(ROCR)
library(pROC)
library(ggplot2)
library(kableExtra)
library(lares)
library(plyr)
library(reshape2)
library(digest)
library(party)
library(rpart)
library(e1071)
library(caret)
library(arules)
library(arulesViz)
library(rattle)
library(tree)
library(Metrics)

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
#-------------------------------------question (b)------------------------------------------------------------------------------------------------------------

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



#######################################question(c) : Derived attributed###############################################
# A good variable to be considered is "Inc_to_cred" which is a ratio of "Tot_mthly_incm" and "Credit_score": this would give us 
# a good explaination on how a good monthly income might lead to a higher credit score.


Loan_Orig$Inc_to_cred = Loan_Orig$Tot_mthly_incm/Loan_Orig$Credit_score


#Another Variable would be a ratio between the Purchase Price of the Home and the Total Monthly Income. This would suggest
#that the home owner will be able to pay off the loan easily or not.

Loan_Orig$Pur_To_Inc = Loan_Orig$pur_prc_amt/Loan_Orig$Tot_mthly_incm
str(Loan_Orig)


#######################################question(d): Variable Selection########################################################################

#_________Making Regression Model and doing variable selection___________________________
#We will remove Status because our target variable: OUTCOME is nothing but 
# the binary version of "Status" (either default or non-default) and hence both the attributes are 
# one and the same. We will also remove "State" variable because it has 52 categories which is nothing 
# but anomaly for our model to process

loan_optim = subset(Loan_Orig, select = -c(Status,State))
str(loan_optim)
full= glm(OUTCOME~.,data= loan_optim, family= binomial)
stepMod=full %>% stepAIC(Trace=FALSE)

# Get the shortlisted variable.
shortlistedVars <- names(unlist(stepMod[[1]])) 
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"] # remove intercept
print(shortlistedVars)

newdf= data.frame(loan_optim$OUTCOME,loan_optim$First_home,loan_optim$Credit_score,loan_optim$Tot_mthly_debt_exp,loan_optim$LoanValuetoAppraised,loan_optim$Per_People_Poverty)

names(newdf) <- c('OUTCOME','First_home', 'Credit_score', 'Tot_mthly_debt_exp','LoanValuetoAppraised','Per_People_Poverty')
str(newdf)

# We will include these variables in our model: Outcome, First_home, Credit_score, 
# Tot_mthly_debt_exp, LoanValuetoAppraised and Per_People_poverty because AIC for these variables are the lowest

# Showing the distribution of values after sorting the the attributes and how they individually relate to the outcome of interest (dependent variable)

meltDatadf <- melt(newdf)
plotting <- ggplot(meltDatadf, aes(factor(variable), value))
plotting + geom_boxplot() + facet_wrap(~variable, scale="free")

#--------------------------------------------------------------------------------------------------------------------------
################################# Question 2#########################################################################################

set.seed(123)
indx1= sample(2,nrow(newdf),replace= TRUE, prob=c(0.75,0.25))
training=newdf[indx1==1,]
testing=newdf[indx1==2,]

table(testing$OUTCOME)


TrgA <- ovun.sample(OUTCOME~., data=training,
                    p=0.3,
                    method="under")$data

TrgB <- ovun.sample(OUTCOME~., data=training,
                    p=0.1,
                    seed=1, method="under")$data

testA = ovun.sample(OUTCOME~., data=testing,
                    p=0.3,
                    method="under")$data

testB = ovun.sample(OUTCOME~., data=training,
                    p=0.1,
                    method="under")$data

table(TrgA$OUTCOME)

table(TrgB$OUTCOME)

str(testing$OUTCOME)

str(testA)

str(TrgA)
#____________________________________Decision Tree Models for TrgA And TrgB______________________________________________

#########################################DECISION TREE MODEL##########################################################

Decision_Tree_TrgA = rpart(OUTCOME~., data = TrgA, method = "class", parms = list(split = "gini"), control = rpart.control(minsplit = 0, minbucket = 0, cp = 0.01))
plot(Decision_Tree_TrgA$cptable)
print(Decision_Tree_TrgA$cptable)
rpart.plot(Decision_Tree_TrgA)


Decision_Tree_TrgB = rpart(OUTCOME~., data = TrgB, method = "class", parms = list(split = "gini"), control = rpart.control(minsplit = 0, minbucket = 0, cp = 0.003))
print(Decision_Tree_TrgB$cptable)
plot(Decision_Tree_TrgB$cptable)
rpart.plot(Decision_Tree_TrgB)


#prediction for TrgA
predict_dtA = predict(Decision_Tree_TrgA,testing,type = "class")
print(predict_dtA)
str(predict_dtA)
#Accuracy
dtA= mean(testing$OUTCOME == predict_dtA)
print(paste("% of predicted classifications correct", mean(dtA)))

caret::confusionMatrix(testing$OUTCOME,predict_dtA)


#prediction for TrgB
predict_dtB = predict(Decision_Tree_TrgB,testing,type = "class")
print(predict_dtB)
#Accuracy
dtB= mean(testing$OUTCOME == predict_dtB)
print(paste("% of predicted classifications correct", mean(dtB)))

caret::confusionMatrix(testing$OUTCOME,predict_dtB)

#########################################LIFT CURVE FOR DECISION TREE#################################################

#Lift for TrgA------------------------------------------------------------
pred_dt = predict(Decision_Tree_TrgA, testing, type ="prob")
pred_tree = ROCR::prediction(pred_dt[,2], testing$OUTCOME)

perf_dtA = ROCR ::performance(pred_tree, "acc")
plot(perf_dtA)

max_ind_dtA = which.max(slot(perf_dtA, "y.values")[[1]] )
acc_dtA = slot(perf_dtA, "y.values")[[1]][max_ind_dtA]
cutoff_dtA = slot(perf_dtA, "x.values")[[1]][max_ind_dtA]
print(c(accuracy= mean(dtA), cutoff = cutoff_dtA))

roc_dt = performance(pred_tree,"tpr","fpr")
plot(roc_dt, colorize = T, lwd = 2)
abline(a = 0, b = 1)

auc_dtA = performance(pred_tree, measure = "auc")
print(auc_dtA@y.values)

Lift_dtA <- performance(pred_tree,"lift","rpp")
plot(Lift_dtA, main="lift curve", colorize=T)

lift_dtA.values <-unlist(slot(Lift_dtA,"y.values"))

min_lift_dtA = min(round(lift_dtA.values,2),na.rm = TRUE)
min_lift_dtA
max_lift_dtA = max(round(lift_dtA.values,2),na.rm = TRUE)
max_lift_dtA
mint_lift_dtA = paste(c("min(Lift) = "), min_lift_dtA, sep = "")
maxt_lift_dtA = paste(c("max(Lift) = "), max_lift_dtA, sep = "")
legend(0.6, 0.95, c(mint_lift_dtA, maxt_lift_dtA, "\n"), border = "white", cex = 0.6, box.col = "blue")


#Lift for TrgB------------------------------------------------------------

pred_dtB = predict(Decision_Tree_TrgB, testing, type="prob")
pred_treeB = ROCR::prediction(pred_dtB[,2], testing$OUTCOME)

perf_dtB = performance(pred_treeB, "acc")
plot(perf_dtB)

max_ind_dtB = which.max(slot(perf_dtB, "y.values")[[1]] )
acc_dtB = slot(perf_dtB, "y.values")[[1]][max_ind_dtB]
cutoff_dtB = slot(perf_dtB, "x.values")[[1]][max_ind_dtB]
print(c(accuracy= mean(dtB), cutoff = cutoff_dtB))

roc_dtB = performance(pred_treeB,"tpr","fpr")
plot(roc_dtB, colorize = T, lwd = 2)
abline(a = 0, b = 1)

auc_dtB = performance(pred_treeB, measure = "auc")
print(auc_dtB@y.values)


Lift_dtB <- performance(pred_treeB,"lift","rpp")
plot(Lift_dtB, main="lift curve", colorize=T)

lift_dtB.values <-unlist(slot(Lift_dtB,"y.values"))

min_lift_dtB = min(round(lift_dtB.values,2),na.rm = TRUE)
min_lift_dtB
max_lift_dtB = max(round(lift_dtB.values,2),na.rm = TRUE)
max_lift_dtB
mint_lift_dtB = paste(c("min(Lift) = "), min_lift_dtB, sep = "")
maxt_lift_dtB = paste(c("max(Lift) = "), max_lift_dtB, sep = "")
legend(0.6, 0.98, c(mint_lift_dtB, maxt_lift_dtB, "\n"), border = "white", cex = 0.6, box.col = "blue")


#####################################################################################################################
#########################################REGRESSION MODEL##########################################################

#logistic Model for TrgA

logitmodelA= glm(OUTCOME ~., data=TrgA, family=binomial)
summary(logitmodelA)

layout(matrix(c(1,2,3,4),2,2))
plot(logitmodelA)

#Prediction

pred_logitA= predict(logitmodelA, testing, type= "response")
pred_class= ifelse(pred_logitA>0.5, "default","non-default")
regA= mean(testing$OUTCOME == pred_class)
print(paste("% of predicted classifications correct", mean(regA)))


#-----------logistic Model for TrgB--------------------------------------------


logitmodelB= glm(OUTCOME ~., data=TrgB, family="binomial")

layout(matrix(c(1,2,3,4),2,2))
plot(logitmodelB)


#Prediction

pred_logitB= predict(logitmodelB, testing, type= "response")
pred_classB= ifelse(pred_logitB>0.5, "default","non-default")
regB= mean(testing$OUTCOME == pred_classB)
print(paste("% of predicted classifications correct", mean(regB)))


#########################################LIFT CURVE FOR REGRESSION MODEL##########################################################

##Lift Chart for TrgA-------------------------------------------------

pred = ROCR::prediction(pred_logitA, testing$OUTCOME)
perf = performance(pred, "acc")
plot(perf)

max_ind = which.max(slot(perf, "y.values")[[1]] )
acc = slot(perf, "y.values")[[1]][max_ind]
cutoff = slot(perf, "x.values")[[1]][max_ind]
print(c(accuracy= acc, cutoff = cutoff))

roc = performance(pred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1)

auc = performance(pred, measure = "auc")
print(auc@y.values)

Lift_log <- performance(pred,"lift","rpp")
plot(Lift_log, main="lift curve", colorize=T)

liftA.values <-unlist(slot(Lift_log,"y.values"))

min_liftA = min(round(liftA.values,2),na.rm = TRUE)
min_liftA
max_liftA = max(round(liftA.values,2),na.rm = TRUE)
max_liftA
mint_liftA = paste(c("min(Lift) = "), min_liftA, sep = "")
maxt_liftA = paste(c("max(Lift) = "), max_liftA, sep = "")
legend(0.6, 0.8, c(mint_liftA, maxt_liftA, "\n"), border = "white", cex = 0.6, box.col = "blue")

##Lift Chart for TrgB----------------------------------------------

predB = ROCR::prediction(pred_logitB, testing$OUTCOME)
perfB = performance(predB, "acc")
plot(perfB)

max_indB = which.max(slot(perfB, "y.values")[[1]] )
accB = slot(perfB, "y.values")[[1]][max_indB]
cutoffB = slot(perfB, "x.values")[[1]][max_indB]
print(c(accuracy= accB, cutoff = cutoffB))

rocB = performance(predB,"tpr","fpr")
plot(rocB, colorize = T, lwd = 2)
abline(a = 0, b = 1)

aucB = performance(predB, measure = "auc")
print(aucB@y.values)

Lift_logB <- performance(predB,"lift","rpp")
plot(Lift_logB, main="lift curve", colorize=T)

liftB.values <-unlist(slot(Lift_logB,"y.values"))

min_liftb = min(round(liftB.values,2),na.rm = TRUE)
min_liftb
max_liftb = max(round(liftB.values,2),na.rm = TRUE)
max_liftb
mint_liftb = paste(c("min(Lift) = "), min_liftb, sep = "")
maxt_liftb = paste(c("max(Lift) = "), max_liftb, sep = "")
legend(0.6, 0.94, c(mint_liftb, maxt_liftb, "\n"), border = "white", cex = 0.6, box.col = "blue")


#####################################################################################################################
#########################################NEURAL NETWORK MODEL##########################################################

#Neural Model for TrgA--------------------------------------------------------------------
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

str(loan_finalA)


#Neural Net for TrgA
nnA=nnet(OUTCOME~., data= TrgA, size = 5,decay = 0.001,maxit = 500)

summary(nnA)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nnA)

summary(nnA$residuals)

#Accuracy for TrgA
pred_nnA= predict(nnA, testing,type = "class")
table(pred_nnA)
str(pred_nnA)
pred_nnA_fact = as.factor(pred_nnA)
caret::confusionMatrix(testing$OUTCOME,pred_nnA_fact)


nnA_mean= mean(testing$OUTCOME == pred_nnA)
print(paste("% of predicted classifications correct", mean(nnA_mean)))

test_neural = testing

test_neural$OUTCOME = ifelse(test_neural$OUTCOME=="default" ,1,0)

pred_nnA_numeric = ifelse(pred_nnA=="default" ,1,0)

library(Metrics)
MSE.NN = mse(test_neural$OUTCOME,predict(nnA, test_neural))
MSE.NN
# Calculate Root Mean Square Error (RMSE)
RMSE.NN = sqrt(mean(nnA$residuals^2))
RMSE.NN


#Neural Model for TrgA--------------------------------------------------------------------
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
nnB=nnet(OUTCOME~., data= loan_finalB, size = 4,decay=0.01)
summary(nnB)

source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nnB)

summary(nnB$residuals)

#Accuracy for TrgB

pred_nnB= predict(nnB, testing,type = "class")
table(pred_nnB)
str(pred_nnB)
pred_nnB_fact= as.factor(pred_nnB)
caret::confusionMatrix(testing$OUTCOME,pred_nnB_fact)


nnB_mean= mean(testing$OUTCOME == pred_nnB)
print(paste("% of predicted classifications correct", mean(nnB_mean)))
pred_nnB_numeric = ifelse(pred_nnB=="default" ,1,0)

library(Metrics)
MSE.NNB = mse(test_neural$OUTCOME,predict(nnB, test_neural))
MSE.NNB
# Calculate Root Mean Square Error (RMSE)
RMSE.NNB = sqrt(mean(nnB$residuals^2))
RMSE.NNB

#########################################LIFT CURVE FOR NEURAL MODEL##########################################################

##Lift Chart for TrgA-------------------------------------------------

pred_nnA_num=predict(nnA,testing,type="raw")
str(test_neural$OUTCOME)
pred_neuralA = ROCR::prediction(pred_nnA_num, test_neural$OUTCOME)
perf_neuralA = performance(pred_neuralA, "acc")
plot(perf_neuralA)

max_ind_neuralA = which.max(slot(perf_neuralA, "y.values")[[1]] )
acc_neuralA = slot(perf_neuralA, "y.values")[[1]][max_ind_neuralA]
cutoff_neuralA = slot(perf_neuralA, "x.values")[[1]][max_ind_neuralA]
print(c(accuracy= acc_neuralA, cutoff = cutoff_neuralA))

roc_neuralA = performance(pred_neuralA,"tpr","fpr")
plot(roc_neuralA, colorize = T, lwd = 2)
abline(a = 0, b = 1)

auc_neuralA = performance(pred_neuralA, measure = "auc")
print(auc_neuralA@y.values)

Lift_neuralA <- performance(pred_neuralA,"lift","rpp")
plot(Lift_neuralA, main="lift curve", colorize=T)

lift_neuralA.values <-unlist(slot(Lift_neuralA,"y.values"))

min_lift_neuralA = min(round(lift_neuralA.values,2),na.rm = TRUE)
min_lift_neuralA
max_lift_neuralA = max(round(lift_neuralA.values,2),na.rm = TRUE)
max_lift_neuralA
mint_lift_neuralA = paste(c("min(Lift) = "), min_lift_neuralA, sep = "")
maxt_lift_neuralA = paste(c("max(Lift) = "), max_lift_neuralA, sep = "")
legend(0.6, 0.8, c(mint_lift_neuralA, maxt_lift_neuralA, "\n"), border = "white", cex = 0.6, box.col = "blue")

##Lift Chart for TrgB----------------------------------------------

pred_nnB_num=predict(nnB,testing,type="raw")
str(test_neural$OUTCOME)
pred_neuralB = ROCR::prediction(pred_nnB_num, test_neural$OUTCOME)
perf_neuralB = performance(pred_neuralB, "acc")
plot(perf_neuralB)

max_ind_neuralB = which.max(slot(perf_neuralB, "y.values")[[1]] )
acc_neuralB = slot(perf_neuralB, "y.values")[[1]][max_ind_neuralB]
cutoff_neuralB = slot(perf_neuralB, "x.values")[[1]][max_ind_neuralB]
print(c(accuracy= acc_neuralB, cutoff = cutoff_neuralB))

roc_neuralB = performance(pred_neuralB,"tpr","fpr")
plot(roc_neuralB, colorize = T, lwd = 2)
abline(a = 0, b = 1)

auc_neuralB = performance(pred_neuralB, measure = "auc")
print(auc_neuralB@y.values)

Lift_neuralB <- performance(pred_neuralB,"lift","rpp")
plot(Lift_neuralB, main="lift curve", colorize=T)

lift_neuralB.values <-unlist(slot(Lift_neuralB,"y.values"))

min_lift_neuralB = min(round(lift_neuralB.values,2),na.rm = TRUE)
min_lift_neuralB
max_lift_neuralB = max(round(lift_neuralB.values,2),na.rm = TRUE)
max_lift_neuralB
mint_lift_neuralB = paste(c("min(Lift) = "), min_lift_neuralB, sep = "")
maxt_lift_neuralB = paste(c("max(Lift) = "), max_lift_neuralB, sep = "")
legend(0.8, 0.6, c(mint_lift_neuralB, maxt_lift_neuralB, "\n"), border = "white", cex = 0.6, box.col = "blue")


##############################Comparing Lift Charts for TrgA################################################################
#Comparing the Lift Charts for Regression Model, Neural Model and Decision tree of TrgA Dataset

par(mfrow=c(1,3))
plot(Lift_dtA, main="lift curve for DT-TrgA", colorize=T)
legend(0.6, 0.9, c(mint_lift_dtA, max_lift_dtA, "\n"), border = "white", cex = 0.5, box.col = "blue")


plot(Lift_log,
     colorize=T,
     main="lift curve for Regression-TrgA"
)
legend(0.6, 0.9, c(mint_liftA, maxt_liftA, "\n"), border = "white", cex = 0.5, box.col = "blue")

plot(Lift_neuralA,
     colorize=T,
     main="lift curve for Neural Model-TrgA"
)
legend(0.6, 0.5, c(mint_lift_neuralA, maxt_lift_neuralA, "\n"), border = "white", cex = 0.5, box.col = "blue")

##############################Comparing Lift Charts for TrgB################################################################
#Comparing the Lift Charts for Regression Model, Neural Model and Decision tree of TrgB Dataset

par(mfrow=c(1,3))
plot(Lift_dtB, main="lift curve for DT-TrgA", colorize=T)
legend(0.6, 0.96, c(mint_lift_dtB, max_lift_dtB, "\n"), border = "white", cex = 0.5, box.col = "blue")


plot(Lift_logB,
     colorize=T,
     main="lift curve for Regression-TrgA"
)
legend(0.6, 0.94, c(mint_liftb, maxt_liftb, "\n"), border = "white", cex = 0.5, box.col = "blue")

plot(Lift_neuralB,
     colorize=T,
     main="lift curve for Neural Model-TrgA"
)
legend(0.6, 0.6, c(mint_lift_neuralB, maxt_lift_neuralB, "\n"), border = "white", cex = 0.5, box.col = "blue")
