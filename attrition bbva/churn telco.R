#https://towardsdatascience.com/predict-customer-churn-with-r-9e62357d47b4


library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)

churn <- read.csv('https://raw.githubusercontent.com/treselle-systems/customer_churn_analysis/master/WA_Fn-UseC_-Telco-Customer-Churn.csv')
str(churn)

sapply (churn, function (x) sum (is.na (x)))

churn <- churn [complete.cases (churn),]

cols_recode1 <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
  churn[,cols_recode1][,i] <- as.factor(mapvalues
                                        (churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}


churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))

min(churn$tenure); max(churn$tenure)

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
churn$tenure_group <- sapply(churn$tenure,group_tenure)
churn$tenure_group <- as.factor(churn$tenure_group)

churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))

churn$customerID <- NULL
churn$tenure <- NULL

numeric.var <- sapply(churn, is.numeric)
corr.matrix <- cor(churn[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")


churn$TotalCharges <- NULL

p1 <- ggplot(churn, aes(x=gender)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p2 <- ggplot(churn, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p3 <- ggplot(churn, aes(x=Partner)) + ggtitle("Partner") + xlab("Partner") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p4 <- ggplot(churn, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p1, p2, p3, p4, ncol=2)

p5 <- ggplot(churn, aes(x=PhoneService)) + ggtitle("Phone Service") + xlab("Phone Service") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p6 <- ggplot(churn, aes(x=MultipleLines)) + ggtitle("Multiple Lines") + xlab("Multiple Lines") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p7 <- ggplot(churn, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p8 <- ggplot(churn, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p5, p6, p7, p8, ncol=2)


p9 <- ggplot(churn, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p10 <- ggplot(churn, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p11 <- ggplot(churn, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p12 <- ggplot(churn, aes(x=StreamingTV)) + ggtitle("Streaming TV") + xlab("Streaming TV") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p9, p10, p11, p12, ncol=2)


p13 <- ggplot(churn, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p14 <- ggplot(churn, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p15 <- ggplot(churn, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p16 <- ggplot(churn, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p17 <- ggplot(churn, aes(x=tenure_group)) + ggtitle("Tenure Group") + xlab("Tenure Group") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p13, p14, p15, p16, p17, ncol=2)




intrain<- createDataPartition(churn$Churn,p=0.7,list=FALSE)
set.seed(2017)
training<- churn[intrain,]
testing<- churn[-intrain,]

dim(training); dim(testing)

LogModel <- glm(Churn ~ .,family=binomial(link="logit"),data=training)
print(summary(LogModel))

anova(LogModel, test="Chisq")

testing$Churn <- as.character(testing$Churn)
testing$Churn[testing$Churn=="No"] <- "0"
testing$Churn[testing$Churn=="Yes"] <- "1"
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))

print("Confusion Matrix for Logistic Regression"); table(testing$Churn, fitted.results > 0.5)

library(MASS)
exp(cbind(OR=coef(LogModel), confint(LogModel)))

tree <- tree(Churn~Contract+tenure_group+PaperlessBilling, training)
plot(tree)

pred_tree <- predict(tree, testing)
print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree, Actual = testing$Churn)

p1 <- predict(tree, training)
tab1 <- table(Predicted = p1, Actual = training$Churn)
tab2 <- table(Predicted = pred_tree, Actual = testing$Churn)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))



rfModel <- randomForest(Churn ~., data = training)
print(rfModel)

pred_rf <- predict(rfModel, testing)
caret::confusionMatrix(pred_rf, testing$Churn) #  error


plot(rfModel)

t <- tuneRF(training[, -18], training[, 18], stepFactor = 0.5, 
            plot = TRUE, ntreeTry = 200, trace = TRUE, improve = 0.05)

rfModel_new <- randomForest(Churn ~., data = training, ntree = 200, mtry = 2, importance = TRUE, proximity = TRUE)
print(rfModel_new)

pred_rf_new <- predict(rfModel_new, testing)
caret::confusionMatrix(pred_rf_new, testing$Churn) # error 

varImpPlot(rfModel_new, sort=T, n.var = 10, main = 'Top 10 Feature Importance')




################################################################################################################

################################################################################################################

################################################################################################################

library(stats)
library(caTools)
library(Amelia)
library(dplyr)

# set your working directory

# read the telecom dataset input file
telecomDataframe <- read.csv('https://raw.githubusercontent.com/treselle-systems/customer_churn_analysis/master/WA_Fn-UseC_-Telco-Customer-Churn.csv')
                        

# print the structure of the dataframe
print(str(telecomDataframe))

# check for the NA values 
any(is.na(telecomDataframe))

# visualize the missing values using the missing map from the Amelia package
missmap(telecomDataframe,col=c("yellow","red"))

# create new column "tenure_interval" from the tenure column
group_tenure <- function(tenure){
  if (tenure >= 0 && tenure <= 6){
    return('0-6 Month')
  }else if(tenure > 6 && tenure <= 12){
    return('6-12 Month')
  }else if (tenure > 12 && tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 && tenure <=36){
    return('24-36 Month')
  }else if (tenure > 36 && tenure <=48){
    return('36-48 Month')
  }else if (tenure > 48 && tenure <= 62){
    return('48-62 Month')
  }else if (tenure > 62){
    return('> 62 Month')
  }
}

# apply group_tenure function on each row of dataframe
telecomDataframe$tenure_interval <- sapply(telecomDataframe$tenure,group_tenure)
telecomDataframe$tenure_interval <- as.factor(telecomDataframe$tenure_interval)

# Ignore the variables with more levels while predicting the model
# Columns "customerID" and "tenure" having more levels
telecomDataframe <- select(telecomDataframe,-customerID,-tenure)

# The value of the following columns affecting the model and introducing the NA value for "No phone service" and  and "No internet service" need to cleanup the data for these columns MultipleLine,OnlineSecurity,OnlineBackup,DeviceProtection,TechSupport,StreamingTV,StreamingMovies
telecomDataframe$MultipleLines <- as.character(telecomDataframe$MultipleLines)
telecomDataframe$OnlineSecurity <- as.character(telecomDataframe$OnlineSecurity)
telecomDataframe$OnlineBackup <- as.character(telecomDataframe$OnlineBackup)
telecomDataframe$DeviceProtection <- as.character(telecomDataframe$DeviceProtection)
telecomDataframe$TechSupport <- as.character(telecomDataframe$TechSupport)
telecomDataframe$StreamingTV <- as.character(telecomDataframe$StreamingTV)
telecomDataframe$StreamingMovies <- as.character(telecomDataframe$StreamingMovies)

# convert factor variables into character variables before changing the values
telecomDataframe$MultipleLines[telecomDataframe$MultipleLines=="No phone service"] <- "No"
telecomDataframe$OnlineSecurity[telecomDataframe$OnlineSecurity=="No internet service"] <- "No"
telecomDataframe$OnlineBackup[telecomDataframe$OnlineBackup=="No internet service"] <- "No"
telecomDataframe$DeviceProtection[telecomDataframe$DeviceProtection=="No internet service"] <- "No"
telecomDataframe$TechSupport[telecomDataframe$TechSupport=="No internet service"] <- "No"
telecomDataframe$StreamingTV[telecomDataframe$StreamingTV=="No internet service"] <- "No"
telecomDataframe$StreamingMovies[telecomDataframe$StreamingMovies=="No internet service"] <- "No"

# converting character variables into factor variables
telecomDataframe$MultipleLines <- as.factor(telecomDataframe$MultipleLines)
telecomDataframe$OnlineSecurity <- as.factor(telecomDataframe$OnlineSecurity)
telecomDataframe$OnlineBackup <- as.factor(telecomDataframe$OnlineBackup)
telecomDataframe$DeviceProtection <- as.factor(telecomDataframe$DeviceProtection)
telecomDataframe$TechSupport <- as.factor(telecomDataframe$TechSupport)
telecomDataframe$StreamingTV <- as.factor(telecomDataframe$StreamingTV)
telecomDataframe$StreamingMovies <- as.factor(telecomDataframe$StreamingMovies)

# check the number of NA rows if it is relatively small in number then ignore those rows from the analysis
telecomDataframe <- na.omit(telecomDataframe)

# set the seed it will output same output when ever the model is executed
set.seed(123)

# sample the input data with 70% for training and 30% for testing
sample <- sample.split(telecomDataframe$Churn,SplitRatio=0.70)
trainData <- subset(telecomDataframe,sample==TRUE)
testData <- subset(telecomDataframe,sample==FALSE)

# logistic regression model on top training the data
telecomModel <- glm(Churn ~ .,family=binomial(link="logit"),data=trainData)
print(summary(telecomModel))

# test the model with test dataset
test.predictions <- predict(telecomModel,newdata=testData,type="response")

# if the prediction probability is greater than 0.5 then those 
# customers are classified as churned customer less than 0.5 are classified as not churning customer
fitted.results <- ifelse(test.predictions > 0.5,1,0)
testData$Churn <- as.character(testData$Churn)
testData$Churn[testData$Churn=="No"] <- "0"
testData$Churn[testData$Churn=="Yes"] <- "1"

# calculating the misclassfication rate
misClasificationError <- mean(fitted.results!=testData$Churn)
print(misClasificationError)

# calculating the accuracy rate
accuracyRate <- 1-misClasificationError
print(accuracyRate)

# confusion matrix
table(testData$Churn,test.predictions > 0.5)

# cbinding actual results with the predicted results
results <- cbind(fitted.results,testData$Churn)
colnames(results) <- c("predicted","actual")
results <- as.data.frame(results)
print(results)


################################################################################################################

################################################################################################################

################################################################################################################
#https://datascienceplus.com/using-mca-and-variable-clustering-in-r-for-insights-in-customer-attrition/


require(caret)
require(plyr)
require(car)
require(dplyr)
require(reshape2)
theme_set(theme_bw(12))

# read the telecom dataset input file
churn <- read.csv('https://raw.githubusercontent.com/treselle-systems/customer_churn_analysis/master/WA_Fn-UseC_-Telco-Customer-Churn.csv')



## recode selected observations 
churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))

churn$InternetService <- as.factor(mapvalues(churn$InternetService, 
                                             from=c("Fiber optic"),
                                             to=c("Fiberoptic")))

churn$PaymentMethod <- as.factor(mapvalues(churn$PaymentMethod,
                                           from=c("Credit card (automatic)","Electronic check","Mailed check",
                                                  "Bank transfer (automatic)"),
                                           to=c("Creditcard","Electronicheck","Mailedcheck","Banktransfer")))

churn$Contract <- as.factor(mapvalues(churn$Contract,
                                      from=c("Month-to-month",
                                             "Two year", "One year"),
                                      to=c("MtM","TwoYr", "OneYr")))


cols_recode1 <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
  churn[,cols_recode1][,i] <- as.factor(mapvalues
                                        (churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}


churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))



cols_drop <- c(1, 20)
churn <- churn[,-cols_drop]

summary(churn$MonthlyCharges)

summary(churn$tenure)

churn$tenure <- as.factor(car::recode(churn$tenure, "1:9 = 'ShortTenure'; 
                               9:29 = 'MediumTenure'; else = 'LongTenure'"))

churn$MonthlyCharges <- as.factor(car::recode(churn$MonthlyCharges, "1:35 = 'LowCharge';35:70 = 'MediumCharge'; else = 'HighCharge'"))

mean(is.na(churn)) 

inTrain <- createDataPartition(churn$Churn, p=0.7, list=FALSE)
## set random seed to make reproducible results
set.seed(324)
training <- churn[inTrain,]
testing <- churn[-inTrain,]

dim(training) ; dim(testing)


require(FactoMineR)
require(factoextra)
res.mca <- MCA(training, quali.sup=c(17,19), graph=FALSE)
fviz_mca_var(res.mca, repel=TRUE)


require(ClustOfVar)
# run variable clustering excluding the target variable (churn) 
variable_tree <- hclustvar(X.quali = training[,1:18])
#plot the dendrogram of variable groups
plot(variable_tree)

# requesting for 25 bootstrap samplings and a plot
stability(variable_tree, B=25)


## cut the variable tree into 9(?) groups of variables 
clus <- cutreevar(variable_tree,9)
## print the list of variables in each cluster groups
print(clus$var)

# overall customer churn rate
round(prop.table(table(training$Churn))*100,1)


cols_aggr_demog <- c(1:4,6:7,9:14,16)
variable <- rep(names(training[,cols_aggr_demog]),each=4)
demog_counts=c()
for(i in 1:ncol(training[,cols_aggr_demog])) {
  demog_count <- ddply(training, .(training[,cols_aggr_demog][,i],training$Churn), "nrow")
  names(demog_count) <- c("class","Churn","count")
  demog_counts <- as.data.frame(rbind(demog_counts, demog_count))
}

demog_churn_rate <- as.data.frame(cbind(variable, demog_counts))
demog_churn_rate1 <- dcast(demog_churn_rate, variable + class ~ Churn, value.var="count")
demog_churn_rate2 <- mutate(demog_churn_rate1, churn_rate=round((Yes/(No+Yes)*100)-26.5,1))
demog <- as.data.frame(paste(demog_churn_rate2$variable,demog_churn_rate2$class))
names(demog) <- c("Category")
demog2 <- as.data.frame(cbind(demog,demog_churn_rate2))
cols_aggr_nlev3 <- c(5,8,15,18)
variable <- rep(names(training[,cols_aggr_nlev3]),each=6)
nlev3_counts=c()
for(i in 1:ncol(training[,cols_aggr_nlev3])) {
  nlev3_count <- ddply(training, .(training[,cols_aggr_nlev3][,i],training$Churn), "nrow")
  names(nlev3_count) <- c("class","Churn","count")
  nlev3_counts <- as.data.frame(rbind(nlev3_counts, nlev3_count))
}

nlev3_churn_rate <- as.data.frame(cbind(variable, nlev3_counts))
nlev3_churn_rate1 <- dcast(nlev3_churn_rate, variable + class ~ Churn, value.var="count")
nlev3_churn_rate2 <- mutate(nlev3_churn_rate1, churn_rate=round((Yes/(No+Yes)*100)-26.5,1))
nlev3 <- as.data.frame(paste(nlev3_churn_rate2$variable,nlev3_churn_rate2$class))
names(nlev3) <- c("Category")
nlev3 <- as.data.frame(cbind(nlev3,nlev3_churn_rate2))
variable <- rep("PaymentMethod",8)
nlev4_count <- ddply(training, .(training[,17],training$Churn), "nrow")
names(nlev4_count) <- c("class","Churn","count")
nlev4_churn_rate <- as.data.frame(cbind(variable, nlev4_count))
nlev4_churn_rate1 <- dcast(nlev4_churn_rate, variable + class ~ Churn, value.var="count")
nlev4_churn_rate2 <- mutate(nlev4_churn_rate1, churn_rate=round((Yes/(No+Yes)*100)-26.5,1))
nlev4 <- as.data.frame(paste(nlev4_churn_rate$variable4,nlev4_churn_rate2$class))
names(nlev4) <- c("Category")
nlev4 <- as.data.frame(cbind(nlev4,nlev4_churn_rate2))
final_agg <- as.data.frame(rbind(demog2, nlev3, nlev4))

ggplot(final_agg, aes(Category, churn_rate, color=churn_rate < 0)) +
  geom_segment(aes(x=reorder(Category, -churn_rate), xend = Category,
                   y = 0, yend = churn_rate), 
               size = 1.1, alpha = 0.7) +
  geom_point(size = 2.5) +
  theme(legend.position="none") +
  xlab("Variable") +
  ylab("Customer Churn (%)") +
  ggtitle("Customer Attrition rate \n Difference from the overall average (%)") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  coord_flip()






