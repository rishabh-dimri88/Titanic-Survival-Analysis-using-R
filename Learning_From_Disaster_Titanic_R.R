setwd("C:/Rishabh/DIM(F)_FALL16/Kaggle") # set working direcotry and import datafile
train <- read.csv("C:/Rishabh/DIM(F)_FALL16/Kaggle/train.csv") # importing training dataset
View(train) # view train dataset
test <- read.csv("C:/Rishabh/DIM(F)_FALL16/Kaggle/test.csv")  # importing testing dataset
View(test) # view test dataset

#Basic analysis
plot(density(train$Age, na.rm = TRUE)) # Maximum were young around 30 years of age
plot(density(train$Fare, na.rm = TRUE))
plot(density(train$Survived, na.rm = TRUE))



str(train) # Structure of train dataset
str(test) # Structure of train dataset
train$Survived # exploring Survived variable in train dataset
table(train$Survived) # Exploring survived in tabular format
prop.table(table(train$Survived)) # Gives the proportion of total in tabular format, our conclusion is all perished
test$Survived <- rep(0,418) # Creating a column Survived in Test dataset and putting value 0 in it repeating 418 times
View(test) # view test dataset
submit <- data.frame(PassengerId=test$PassengerId,Survived=test$Survived) # creating new dataframe submit with just two columns of test dataset
write.csv(submit, file='All_Dead.csv', row.names = FALSE ) # Wrting new csv file to submit to kaggle

# SECOND analysis

table(train$Age) # exploring Age variable in train dataset
summary(train$Sex) # exploring Sex variable in train dataset
prop.table(table(train$Sex,train$Survived),1) # Gives the proportion of total in tabular format, our conclusion is females survived and males perished
test$Survived <- 0 # Setting 0 to Survived column in Test
test$Survived[test$Sex == 'female'] <- 1 # Females are given 1 in Survived column in Test
test$Survived # exploring Survived in test dataset
submit <- data.frame(PassengerId=test$PassengerId,Survived=test$Survived) # creating new dataframe submit with just two columns of test dataset
write.csv(submit, file='WithFemalesSurvived.csv', row.names = FALSE )# Wrting new csv file to submit to kaggle

# THIRD  analysis

summary(train$Age) # Summary of Age in Training dataset
train$Child <- 0 # creating a child column and setting its value as 0
train$Child[train$Age < 18] <- 1 # All the passenger with age below 18 are assigned 1 means they are children
aggregate(Survived ~ Child+Sex, data=train, FUN =length) # Summary in tabular format for more than two variables
aggregate(Survived ~ Child+Sex, data=train, FUN = function(x){sum(x)/length(x)}) # Summary of proportion in tabular format for more than two variable
train$Fare 
train$Pclass
prop.table(table(train$Sex,train$Pclass),1)
aggregate(Survived ~ Pclass+Sex, data=train, FUN = function(x){sum(x)/length(x)}) # Summary of proportion in tabular format for more than two variable
train$Fare2<- '30+' # binning the fare from train dataset in a new column fare2 
train$Fare2[train$Fare<30 &train$Fare>=20 ]<- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
aggregate(Survived ~ Sex+Fare2, data=train, FUN = function(x){sum(x)/length(x)})
aggregate(Survived ~ Pclass+Sex+Fare2, data=train, FUN = function(x){sum(x)/length(x)}) # Summary of proportion in tabular format for more than two variable,
#Insight here!, female in passenger class 3 and paid more than 20 fare still did not survive. 

test$Survived <- 0 # Setting the survived column in test dataset as 0
test$Survived[test$Sex == 'female'] <- 1 # Setting the survived column in test dataset as 1 for all females
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0 # Female with pclass 3 and fare > 20 are perished in testdataset
submit <- data.frame(PassengerId=test$PassengerId,Survived=test$Survived)
write.csv(submit, file='WithFemales&Class.csv', row.names = FALSE ) # Wrting new csv file to submit to kaggle


# FOURTH  Analysis

str(train)
library(rpart)
fit <- rpart(Survived ~ Pclass+Sex+Age+Parch+SibSp+Fare+Embarked, data=train, method="class") # Decision tree considering all variables
plot(fit)
text(fit)

install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
installed.packages

rpart.plot(fit)

prediction <- predict(fit, test, type='class') # predicting the test dataset based on decision tree of training dataset
submit<- data.frame(PassengerId=test$PassengerId,Survived=prediction)
write.csv(submit,file= "myFirstDecsnTree.csv", row.names= FALSE)  # Wrting new csv file to submit to kaggle

fit <- rpart(Survived ~ Pclass+Sex+Age+Parch+SibSp+Fare+Embarked, data=train, method="class", control = rpart.control(cp=0,minsplit =2 )) # Playing with dataset
rpart.plot(fit)

fit <- rpart(Survived ~ Pclass+Sex+Age+Parch+SibSp+Fare+Embarked, data=train, method="class", control = rpart.control(minsplit=2, cp=0.005))
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)

rpart.plot(new.fit)

train$Name[1]

#---- FEATURE ENGINEERING ---- 
# FIFTH Analsis
  
str(test)
test$Survived <- NA
combination <- rbind(train,test) # Combining two datasets Training and Testing into one
combination
str(combination)
View(combination)

combination$Name <- as.character(combination$Name) # Changing the datatype of Name column into character
str(combination)
combination$Name[1]
strsplit(combination$Name[1], split = '[,.]') # will be important to split texts when many texts in dataset
strsplit(combination$Name[1], split = '[,.]')[[1]][1] # Gives the first element of the vector after splitting
strsplit(combination$Name[1], split = '[,.]')[[1]][2]
strsplit(combination$Name, split='[,.]')[[1]][2]
strsplit(combination$Name, split='[,.]')[[2]][2]
strsplit(combination$Name, split='[,.]')[[3]][2]
combination$Title <- strsplit(combination$Name, split='[,.]')[[1]][2] # Will give the second element of the vector and will paste the same value in all the rows
# not what we want
View(combination)
combination$Title <- sapply(combination$Name, FUN=function(x){strsplit(x, split='[,.]')[[1]][2]}) # Saaply will work here. Will loop through all the rows
# sub  and gsub are used to replace first occurence of something or all occurence of something from a pattern
combination$Title <- sub(' ','',combination$Title) # Removes extra space infront of the title
table(combination$Title)
combination$Title[combination$Title %in% c("Mlle","Mme")] <- 'Mlle' # comining similar meaning titles into 1
combination$Title[combination$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combination$Title[combination$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
table(combination$Title)
combination$Title <- factor(combination$Title) # Converitng the datatype to factor
str(combination)
combination$FamilySize <- combination$SibSp + combination$Parch + 1 # Calculating the family size(new column) by summing numb of sibling/spouse, number of parent/child and one itself
combination$Surname <- sapply(combination$Name, FUN=function(x){strsplit(x, split='[,.]')[[1]][1]}) # extracting surname like we did for title
combination$FamilyId <- paste(as.character(combination$FamilySize),combination$Surname, sep='') # making a combination of familysie and surname, simly concatenate the two columns
combination$FamilyId[combination$FamilySize <2] <- "small" # Setting the Family Id as "Small" for all the FamilySize less than 2
table(combination$FamilyId) 
famiId<- data.frame(table(combination$FamilyId)) # Creating new dataframe in the table format
View(famiId)
famiId <- famiId[famiId$Freq <= 2,] # Filtering the new dataset keeping only rows which has freq less than or equal to 2
combination$FamilyId[combination$FamilyId %in% famiId$Var1] <- 'small' # Setting all the othe FamilyId as "Small" who had less than 2 frequency
combination$FamilyId <- factor(combination$FamilyId) # Changing the datatype to factor again

# Back to data split
train <- combination[1:891,] 
test <- combination[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyId, data=train, method="class") # Decision tree using new variables(obtained after feature engineering) in trainingn dataset.
rpart.plot(fit)
prediction <- predict(fit, test, type='class') # predicting the test dataset based on decision tree of training dataset
submit<- data.frame(PassengerId=test$PassengerId,Survived=prediction)
write.csv(submit,file= "FeatureEngineeredDecsnTree.csv", row.names= FALSE) # Wrting new csv file to submit to kaggle

# SIXTH Analysis

# Replacing missing values in Age variable
summary(combination$Age)

# using decision tree to impute the values
library(rpart)
library(rpart.plot)
AgeFit <- rpart(Age ~  Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, data=combination[!is.na(combination$Age),], method = "anova"  )
rpart.plot(AgeFit)
combination$Age[is.na(combination$Age)] <- predict(AgeFit, combination[is.na(combination$Age),])
combination$Age[is.na(combination$Age)]
summary(combination$Age)
summary(combination)

# seems like Emabrked has two blank values and Fare has 1 NA value
which(combination$Embarked=="") # checking what index has blank value in Emmbarked
combination$Embarked[62] <- 'S' # Most of the passengers embarked from southhampton so we put S in this missing one
combination$Embarked[830] <- 'S' # Similar as above
combination$Embarked[62] # checking the value replaced or not
summary(combination$Embarked)
which(is.na(combination$Fare)) # checking what index has NA value in Fare
summary(combination$Fare)
combination$Fare[1044] = 14.450
combination$Fare[1044]
summary(combination)


# reducing the factor levels below 32
str(combination)
summary(combination$FamilyId)
combination$FamilyId[combination$FamilyId=="Small"] <-'small' # That was my mistake, correcting Small and small
summary(combination$FamilyId2)
combination$FamilyId2 <- combination$FamilyId
combination$FamilyId2 <- as.character(combination$FamilyId2)
combination$FamilyId2[combination$FamilySize <=3] <- 'small'
combination$FamilyId2 <- factor(combination$FamilyId2)
str(combination$FamilyId2)

# Back to data split
train <- combination[1:891,] 
test <- combination[892:1309,]

# Random Forest
install.packages('randomForest')
library(randomForest)
set.seed(900)

fit <- randomForest(as.factor(Survived)~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyId2, data = train, importance=TRUE, ntree=2000 )
varImpPlot(fit)

Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "Random_Forest_Titanic.csv", row.names = FALSE)

# SEVENTH ANALYSIS
install.packages("party")
library(party)

set.seed(900)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyId, data = train, controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fit, test, OOB=TRUE, type="response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "Random_Forest2.csv", row.names = FALSE)
