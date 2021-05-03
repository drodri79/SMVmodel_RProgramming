setwd("E:\\IntelligentSystem\\SD745Homework\\week6")

train_dataset <- read.csv('train.csv', header = TRUE)
testset <- read.csv('train.csv', header = TRUE)

summary(train_dataset)
summary(testset)

#Data preprocessing 
Loan_ID = testset$Loan_ID

train_dataset$Loan_ID = NULL

View(testset)
View(train_dataset)

#combining ApplicantIncome and Coapplicant Income
totalIncomeTrain = train_dataset$ApplicantIncome + train_dataset$CoapplicantIncome
train_dataset = cbind(train_dataset, totalIncomeTrain)
train_dataset$ApplicantIncome = NULL
train_dataset$CoapplicantIncome = NULL


#combining ApplicantIncome and Coapplicant Income testset
totalIncomeTrain = train_dataset$ApplicantIncome + train_dataset$CoapplicantIncome
train_dataset = cbind(train_dataset, totalIncomeTrain)
train_dataset$ApplicantIncome = NULL
train_dataset$CoapplicantIncome = NULL

str(train_dataset)

#converting into factors
train_dataset$Gender = factor(train_dataset$Gender, levels = c('Female','Male'), labels = c(0,1))
train_dataset$Married = factor(train_dataset$Married, levels = c('Yes', 'No'), labels = c(0,1))
train_dataset$Dependents = factor(train_dataset$Dependents, levels = c('0','1','2','3+'), labels = c(0,1,2,3))
train_dataset$Education = factor(train_dataset$Education, levels = c('Graduate','Not Graduate'), labels = c(0,1))
train_dataset$Self_Employed = factor(train_dataset$Self_Employed, levels = c('No','Yes'), labels = c(0,1))
train_dataset$Property_Area = factor(train_dataset$Property_Area, levels = c('Rural','Semiurban', 'Urban'), labels = c(0,1,2))
train_dataset$Credit_History = factor(train_dataset$Credit_History, levels = c("0","1"), labels = c(0,1))
train_dataset$Loan_Status = factor(train_dataset$Loan_Status, levels = c('N','Y'), labels = c(0,1))

test_set
#missing data of LoanAmount, loan term
#missing values of gender, married, education, .selfemployment and prperty area
train_dataset$LoanAmount = ifelse(is.na(train_dataset$Loan_Amount_Term), 
                                  ave(train_dataset$Loan_Amount_Term, FUN = function(x)mean(x, na.rm = TRUE
                                      )), 
                                      train_dataset$Loan_Amount_Term)

train_dataset$Gender = ifelse(is.na(train_dataset$Gender), NA %in% 1, train_dataset$Gender)
train_dataset$Married = ifelse(is.na(train_dataset$Married), NA %in% 1, train_dataset$Married)
train_dataset$Dependents = ifelse(is.na(train_dataset$Dependents),NA %in% 1, train_dataset$Dependents)
train_dataset$Education = ifelse(is.na(train_dataset$Education), NA %in% 1, train_dataset$Education)
train_dataset$Self_Employed = ifelse(is.na(train_dataset$Self_Employed),NA %in% 1, train_dataset$Self_Employed)
train_dataset$Credit_History = ifelse(is.na(train_dataset$Credit_History),NA %in% 1, train_dataset$Credit_History)

str(train_dataset)

#converting to numeric
train_dataset$Gender = as.numeric(train_dataset$Gender)
train_dataset$Married = as.numeric(train_dataset$Married)
train_dataset$Education = as.numeric(train_dataset$Education)
train_dataset$Self_Employed = as.numeric(train_dataset$Self_Employed)
train_dataset$Property_Area = as.numeric(train_dataset$Property_Area)
train_dataset$Dependents = as.numeric(train_dataset$Dependents)
train_dataset$Credit_History = as.numeric(train_dataset$Credit_History)

#scaling for normalization
train_dataset[,6:9] = scale(train_dataset[,6:9])
train_dataset$totalIncomeTrain = scale(train_dataset$totalIncomeTrain)

View(train_dataset)
str(train_dataset)


#SVM Model
library(e1071)
str(train_dataset)
View(train_dataset)

library(caTools)
set.seed(123)
split = sample.split(train_dataset$Loan_Status,SplitRatio = 0.75)

training_set = train_dataset[split,]
test_set = train_dataset[-split, ]

classifier= svm (Loan_Status ~., data = training_set , kernal = 'linear')
test_set$Predicted_LoanStatus = predict(classifier, newdata = test_set)

View(test_set)

#confusion matrix
table(test_set$Predicted_LoanStatus, test_set$Loan_Status)
summary(classifier)
