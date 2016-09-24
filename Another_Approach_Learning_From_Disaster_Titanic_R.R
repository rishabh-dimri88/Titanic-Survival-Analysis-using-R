setwd("C:/Rishabh/DIM(F)_FALL16/Kaggle") # set working direcotry and import datafile
train <- read.csv("C:/Rishabh/DIM(F)_FALL16/Kaggle/train.csv") # importing training dataset
View(train) # view train dataset
test <- read.csv("C:/Rishabh/DIM(F)_FALL16/Kaggle/test.csv")  # importing testing dataset
View(test) # view test dataset

#Basic analysis
plot(density(train$Age, na.rm = TRUE)) # Maximum were young around 30 years of age
plot(density(train$Fare, na.rm = TRUE))
plot(density(train$Survived, na.rm = TRUE))

counts <- table(train$Survived, train$Sex)
barplot(counts, xlab = "Gender", ylab = "Number of People", main = "survived and deceased between male and female")
counts[2] / (counts[1] + counts[2])
counts[4] / (counts[3] + counts[4])

P_class <- table(train$Survived, train$Pclass)
barplot(P_class, xlab= "class", ylab="Number of people", main = "survived and deceased between different class" )

train$Sex <- gsub("female",1,train$Sex)
train$Sex <- gsub("male",0,train$Sex)
train$Sex

master_vector = grep("Master.",train$Name, fixed=TRUE) # returns vector of row number which has Master surname
master_vector
miss_vector = grep("Miss.", train$Name, fixed=TRUE)
mrs_vector = grep("Mrs.", train$Name, fixed=TRUE)
mr_vector = grep("Mr.", train$Name, fixed=TRUE)
dr_vector = grep("Dr.", train$Name, fixed=TRUE)

for (i in master_vector){
  train$Name[i]= "Master"
}
warnings()   # warnings are thrown in the output

class(master_vector) # to check the datatype of master_vector_object
class(train$Name)  # to check the datatype of Name in train. It was factor. Cause of problem
train$Name <- as.character(train$Name)

for (i in master_vector){  # Using for loop to change the Name according to its title
  train$Name[i]= "Master"  # All other non common titles are not taken into consideration
}
for (i in miss_vector){
  train$Name[i]= "Miss"
}
for (i in mrs_vector){
  train$Name[i]= "Mrs"
}
for (i in mr_vector){
  train$Name[i]= "Mr"
}
for (i in dr_vector){
  train$Name[i]= "Dr"
}

master_age= mean(train$Age[train$Name=="Master"],na.rm="TRUE") # finding the value of mean age when name is master, na.rm= true means ignoring missing values
master_age
round(master_age, digits=2)
miss_age= round(mean(train$Age[train$Name=="Miss"],na.rm="TRUE"),digits=2)
miss_age
mrs_age= round(mean(train$Age[train$Name=="Mrs"],na.rm="TRUE"),digits=2)
mr_age= round(mean(train$Age[train$Name=="Mr"],na.rm="TRUE"),digits=2)
mr_age
dr_age= round(mean(train$Age[train$Name=="Dr"],na.rm="TRUE"),digits=2)

nrow(train) # number of rows returned by train data

is.na(train[1,6]) # checking weather 1st row age values is NA or not
is.na(train[6,"Age"]) # checking weather 6th row age values is NA or not
train[2,6] 


train1 <- train # To check if code works properly 
View(train1)

for(i in 1:nrow(train1)){        # To assign average age values for all the missing values in different titles
    if(is.na(train1[i,"Age"])){
      if(train1$Name[i]=="Master"){
        train1$Age[i]=master_age}
    else if(train1$Name[i]=="Miss"){
      train1$Age[i]=miss_age}
    else if(train1$Name[i]=="Mrs"){
      train1$Age[i]=mrs_age}
    else if(train1$Name[i]=="Mr"){
      train1$Age[i]=mr_age}
    else if(train1$Name[i]=="Dr"){
      train1$Age[i]=dr_age}
    else {
      print("Title not found")}
    }
}
      
train1$Age <- round(train1$Age,digits=2)

# code worked properly so we will do the changes for Trainig dataset
# Impute the missing Age values

for(i in 1:nrow(train)){
  if(is.na(train[i,"Age"])){
    if(train$Name[i]=="Master"){
      train$Age[i]=master_age}
    else if(train$Name[i]=="Miss"){
      train$Age[i]=miss_age}
    else if(train$Name[i]=="Mrs"){
      train$Age[i]=mrs_age}
    else if(train$Name[i]=="Mr"){
      train$Age[i]=mr_age}
    else if(train$Name[i]=="Dr"){
      train$Age[i]=dr_age}
    else {
      print("Title not found")}
  }
}

train$Age <- round(train$Age,digits=2)

# Creating a new variable child in train dataset. First setting value as NA and then conditon
  
train$Child= NA
for(i in 1:nrow(train)){
  if(train$Age[i]<=12){
    train$Child[i]=1}
    else {
      train$Child[i]=0
    }
}

# Creating a new variable Mother in train dataset. First setting value as NA and then
# conditon, if title is MRS and numberof Parentchild >0. This includes any case where
# title is Mrs and number of parents greater than 0.

train$Mother= NA
for(i in 1:nrow(train)){
  if(train$Name[i]=="Mrs" & train$Parch[i]>0){
    train$Mother[i]=1}
  else {
    train$Mother[i]=0
  }
}

# Creating a variable family by adding the the parent child+siblings+1

train$Family = NA
for(i in 1:nrow(train)){
  train$Family[i]= train$SibSp[i]+train$Parch[i]+1
}


train = train[-c(1,9:12)]

# Clean Test data

PassengerId = test[1]
test = test[-c(1, 8:11)]

test$Sex = gsub("female", 1, test$Sex)
test$Sex = gsub("male", 0, test$Sex)

test1 <- test
#View(test1)
test_master_vector = grep("Master.",test1$Name)
test_miss_vector = grep("Miss.", test1$Name)
test_mrs_vector = grep("Mrs.", test1$Name)
test_mr_vector = grep("Mr.", test1$Name)
test_dr_vector = grep("Dr.", test1$Name)

test1$Name <- as.character(test1$Name)

for(i in test_master_vector) {
  test1[i, 2] = "Master"
}
for(i in test_miss_vector) {
  test1[i, 2] = "Miss"
}
for(i in test_mrs_vector) {
  test1[i, 2] = "Mrs"
}
for(i in test_mr_vector) {
  test1[i, 2] = "Mr"
}
for(i in test_dr_vector) {
  test1[i, 2] = "Dr"
}

test_master_age = round(mean(test1$Age[test1$Name == "Master"], na.rm = TRUE), digits = 2)
test_miss_age = round(mean(test1$Age[test1$Name == "Miss"], na.rm = TRUE), digits =2)
test_mrs_age = round(mean(test1$Age[test1$Name == "Mrs"], na.rm = TRUE), digits = 2)
test_mr_age = round(mean(test1$Age[test1$Name == "Mr"], na.rm = TRUE), digits = 2)
test_dr_age = round(mean(test1$Age[test1$Name == "Dr"], na.rm = TRUE), digits = 2)

for (i in 1:nrow(test1)) {
  if (is.na(test1[i,4])) {
    if (test1[i, 2] == "Master") {
      test1[i, 4] = test_master_age
    } else if (test1[i, 2] == "Miss") {
      test1[i, 4] = test_miss_age
    } else if (test1[i, 2] == "Mrs") {
      test1[i, 4] = test_mrs_age
    } else if (test1[i, 2] == "Mr") {
      test1[i, 4] = test_mr_age
    } else if (test1[i, 2] == "Dr") {
      test1[i, 4] = test_dr_age
    } else {
      print(paste("Uncaught title at: ", i, sep=""))
      print(paste("The title unrecognized was: ", test1[i,2], sep=""))
    }
  }
}

test1[89, 4] = "age_missed"
class(test1$Age)

test1$Child= NA
for(i in 1:nrow(test1)){
  if(test1$Age[i]<=12){
    test1$Child[i]=1}
  else {
    test1$Child[i]=0
  }
}


test1$Mother= NA
for(i in 1:nrow(test1)){
  if(test1$Name[i]=="Mrs" & test1$Parch[i]>0){
    test1$Mother[i]=1}
  else {
    test1$Mother[i]=0
  }
}


test1$Family = NA
for(i in 1:nrow(test1)){
  test1$Family[i]= test1$SibSp[i]+test1$Parch[i]+1
}  
  
test2 <-test1

# Logistic regression model training
# As the target is binary
head(train)

fit <- glm(Survived~Pclass+Sex+Age+Sex*Pclass+Child+Mother+Family, data=train)
summary(fit)

class(test2$Age)
test2$Age <- as.numeric(test2$Age)
View(test2)
prediction <- predict.glm(fit, newdata = test2)
prediction


test <- read.csv("C:/Rishabh/DIM(F)_FALL16/Kaggle/test.csv")
nrow(test)
length(prediction)

submit <- data.frame(PassengerId=test$PassengerId,Survived=prediction)
View(submit)
submit$Survived

submit$Survived[89]=0.013180660 # Imputed a random value to the missing value in Survived

for(i in 1:nrow(submit)){
  if (submit$Survived[i] <= 0.5){
    submit$Survived[i]=0}
  else{
    submit$Survived[i]=1}
}

write.csv(submit,file= "Logistic_regression_titanic.csv", row.names=FALSE)



