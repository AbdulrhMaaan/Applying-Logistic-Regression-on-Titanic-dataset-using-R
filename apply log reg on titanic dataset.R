#apply log reg on titanic dataset

data <-read.csv("F:/Github_Repo/Applying Logistic Regression on Titanic dataset using R/Titanic.csv")

summary(data)

#know the dimention of data   #row * #col
dim(data)

#discover if there is null valuse or not 
for(i in 1:12)
  cat( colnames(data)[i] , "",sum(is.na(data[,i])),"\n" )

#can also detect null valuse using funcition sapply
sapply(data, function(x) sum(is.na(x)))
help(sapply)


#copy data and apply data cleaning 
log_data <- data

#now Age , cabin , Fare has missing values

# exchange the null value of age  with mean of ages 
log_data$Age[is.na(log_data$Age)] <- mean(log_data$Age, na.rm=TRUE)

?round
log_data$Age<-round(log_data$Age,0)

log_data
print ( sum(is.na(log_data[,6])) )



# delete the row of Fare missing valuse  or replace it using mean  or replace it using mean of pclass cluster

#if i put a mean value it can be not close to the right value and it will effect the model results
log_data$Fare[is.na(log_data$Fare)] <- mean(log_data$Fare, na.rm=TRUE)


# sex feature must be numerical  so lets convert it 
print(is.factor(log_data$Sex)) # false 
log_data$Sex <- as.factor(log_data$Sex)
print(is.factor(log_data$Sex))     # True
contrasts(log_data$Sex)  # how R see ur col

# Embarked feature must be numerical  so lets convert it 
print(is.factor(log_data$Embarked)) # false 
log_data$Embarked <- as.factor(log_data$Embarked)
print(is.factor(log_data$Embarked))     # True
contrasts(log_data$Embarked) #how R see ur col


# WE WILL CANCEL  PASSENGERID NAME Cabin colms
log_data_cleaned<- subset(log_data,select = c(2,3,5,6,7,8,10,12))
contrasts(log_data_cleaned$Sex)
contrasts(log_data_cleaned$Embarked)

log_data_cleaned$Fare<-round(log_data_cleaned$Fare,0)

dim()

#now data is ready .. lets apply the log model 
fit <- glm(Survived ~ .,family =binomial(link = 'logit'),data=log_data_cleaned,maxit=100)
summary(fit) # this return problem that all features are not significant and then model is nothing " i think its beacuse of the data so spares  many 00000000"
testpint<- log_data_cleaned[ 1,2:8]
Preiction<- predict(fit,testpint) 
print(Preiction)

