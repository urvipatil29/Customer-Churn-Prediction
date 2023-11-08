#Importing the Libraries
library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)

#Importing the Dataset
library(readxl)
Customer <- read_excel("Customer.xlsx")
View(Customer)

#Structure of the dataset 
str(Customer)

#Missing Values
sapply(Customer, function(Customer) sum(is.na(Customer)))
Customer <- Customer[complete.cases(Customer), ]

#Data Wrangling
Customer$OnlineSecurity <- as.factor(mapvalues(Customer$OnlineSecurity, 
                                               from=c("No internet service"),
                                               to=c("No")))
Customer$OnlineBackup <- as.factor(mapvalues(Customer$OnlineBackup, 
                                               from=c("No internet service"),
                                               to=c("No")))
Customer$DeviceProtection <- as.factor(mapvalues(Customer$DeviceProtection, 
                                             from=c("No internet service"),
                                             to=c("No")))
Customer$TechSupport <- as.factor(mapvalues(Customer$TechSupport, 
                                           from=c("No internet service"),
                                           to=c("No")))
Customer$StreamingTV <- as.factor(mapvalues(Customer$StreamingTV, 
                                            from=c("No internet service"),
                                            to=c("No")))
Customer$StreamingMovies <- as.factor(mapvalues(Customer$StreamingMovies, 
                                            from=c("No internet service"),
                                            to=c("No")))
Customer$MultipleLines <- as.factor(mapvalues(Customer$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))

min(Customer$tenure); max(Customer$tenure)

group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}
Customer$tenure_group <- sapply(Customer$tenure,group_tenure)
Customer$tenure_group <- as.factor(Customer$tenure_group)

Customer$SeniorCitizen <- as.factor(mapvalues(Customer$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))

Customer$customerID <- NULL
Customer$tenure <- NULL

Customer$Churn <- as.factor(mapvalues(Customer$Churn,
                                              from=c("No", "Yes"),
                                              to=c("0","1")))

#Exploratory data analysis and Feature Selection
##Correlation between numeric variables
numeric.var <- sapply(Customer, is.numeric)
corr.matrix <- cor(Customer[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

##...The Monthly Charges and Total Charges are correlated...##
##...So one of them will be removed from the model. We remove Total Charges...##
Customer$TotalCharges <- NULL

##Bar plots of categorical variables
p1 <- ggplot(Customer, aes(x=gender)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p2 <- ggplot(Customer, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p3 <- ggplot(Customer, aes(x=Partner)) + ggtitle("Partner") + xlab("Partner") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p4 <- ggplot(Customer, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p1, p2, p3, p4, ncol=2)

p5 <- ggplot(Customer, aes(x=PhoneService)) + ggtitle("Phone Service") + xlab("Phone Service") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p6 <- ggplot(Customer, aes(x=MultipleLines)) + ggtitle("Multiple Lines") + xlab("Multiple Lines") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p7 <- ggplot(Customer, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p8 <- ggplot(Customer, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p5, p6, p7, p8, ncol=2)

p9 <- ggplot(Customer, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p10 <- ggplot(Customer, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p11 <- ggplot(Customer, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p12 <- ggplot(Customer, aes(x=StreamingTV)) + ggtitle("Streaming TV") + xlab("Streaming TV") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p9, p10, p11, p12, ncol=2)

p13 <- ggplot(Customer, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p14 <- ggplot(Customer, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p15 <- ggplot(Customer, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p16 <- ggplot(Customer, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p17 <- ggplot(Customer, aes(x=tenure_group)) + ggtitle("Tenure Group") + xlab("Tenure Group") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p13, p14, p15, p16, p17, ncol=2)

#(1) Logistic Regression
##split the data into training and testing sets
intrain<- createDataPartition(Customer$Churn,p=0.7,list=FALSE)
set.seed(2017)
training<- Customer[intrain,]
testing<- Customer[-intrain,]

dim(training); dim(testing)

##Fitting the Logistic Regression Model
LogModel <- glm(Churn ~ .,family=binomial(link="logit"),data=training)
print(summary(LogModel))

##Feature Analysis
anova(LogModel, test="Chisq")

##Assessing the predictive ability of the Logistic Regression model
testing$Churn <- as.character(testing$Churn)
testing$Churn[testing$Churn=="No"] <- "0"
testing$Churn[testing$Churn=="Yes"] <- "1"
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))

##Logistic Regression Confusion Matrix
print("Confusion Matrix for Logistic Regression"); table(testing$Churn, fitted.results > 0.5)

##Odds Ratio
library(MASS)
exp(cbind(OR=coef(LogModel), confint(LogModel)))

# (2)Decision Tree
##Decision Tree Visualization
##...For illustration purpose, we are going to use only three variables for plotting Decision Trees...##
##...they are “Contract”, “tenure_group” and “PaperlessBilling”...##
training$Contract<-factor(training$Contract)
training$tenure_group<-factor(training$tenure_group)
training$PaperlessBilling<-factor(training$PaperlessBilling)

tree <- ctree(Churn~Contract+tenure_group+PaperlessBilling, training)
plot(tree)

testing$Contract<-factor(testing$Contract)
testing$tenure_group<-factor(testing$tenure_group)
testing$PaperlessBilling<-factor(testing$PaperlessBilling)

##Decision Tree Confusion Matrix
pred_tree <- predict(tree, testing)
print("Confusion Matrix for Decision Tree")
table(Predicted = pred_tree, Actual = testing$Churn)

##Decision Tree Accuracy
p1 <- predict(tree, training)
tab1 <- table(Predicted = p1, Actual = training$Churn)
tab2 <- table(Predicted = pred_tree, Actual = testing$Churn)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))

# (3)Random Forest
##Random Forest Initial Model
rfModel <- randomForest(Churn ~., data = training)
print(rfModel)

##Random Forest Confusion Matrix
pred_rf <- predict(rfModel, testing)
print("Confusion Matrix for RF")
table(Predicted = pred_rf, Actual = testing$Churn)

##Random forest Accuracy
p2 <- predict(rfModel, training)
tab1 <- table(Predicted = p2, Actual = training$Churn)
tab2 <- table(Predicted = pred_rf, Actual = testing$Churn)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))

plot(rfModel)

##Tune Random Forest Model
t <- tuneRF(training[, -18], training[, 18], stepFactor = 0.5, plot = TRUE, ntreeTry = 200, trace = TRUE, improve = 0.05)

##Fit the Random Forest Model After Tuning
rfModel_new <- randomForest(Churn ~., data = training, ntree = 200, mtry = 2, importance = TRUE, proximity = TRUE)
print(rfModel_new)

##Random Forest Confusion Matrix After Tuning
pred_rf_new <- predict(rfModel_new, testing)
print("Confusion Matrix for RF after tuning")
table(Predicted = pred_rf, Actual = testing$Churn)

##Random Forest Feature Importance
varImpPlot(rfModel_new, sort=T, n.var = 10, main = 'Top 10 Feature Importance')