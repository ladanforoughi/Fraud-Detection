if(!require(tidyverse))install.packages("tidyvers", respo = "https:\\cran.us.ir-project-org")
if(!require(dplyr))install.packages("dplyr", respo = "https:\\cran.us.ir-project-org")
if(!require(ggplot2))install.packages("ggplot2", respo = "https:\\cran.us.ir-project-org")
if(!require(caret))install.packages("caret", respo = "https:\\cran.us.ir-project-org")
if(!require(knitr))install.packages("knitr", respo = "https:\\cran.us.ir-project-org")
if(!require(gridExtra))install.packages("gridExtra", respo = "https:\\cran.us.ir-project-org")
if(!require(kableExtra))install.packages("kaggleExtra", respo = "https:\\cran.us.ir-project-org")
if(!require(readr))install.packages("readr", respo = "https:\\cran.us.ir-project-org")
if(!require(purrr))install.packages("purrr", respo = "https:\\cran.us.ir-project-org")
if(!require(randomForest))install.packages("randomForest", respo = "https:\\cran.us.ir-project-org")
if(!require(pROC))install.packages("pROC", respo = "https:\\cran.us.ir-project-org")
if(!require(fastDummies))install.packages("fastDummies", respo = "https:\\cran.us.ir-project-org")
if(!require(rpart.plot))install.packages("rpart.plot", respo = "https:\\cran.us.ir-project-org")
if(!require(data.table))install.packages("data.table", respo = "https:\\cran.us.ir-project-org")
if(!require(reshape2))install.packages("reshape2", respo = "https:\\cran.us.ir-project-org")
if(!require(graphics))install.packages("graphics", respo = "https:\\cran.us.ir-project-org")


library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(knitr)
library(gridExtra)
library(kableExtra)
library(readr)
library(purrr)
library(randomForest)
library(pROC)
library(fastDummies)
library(rpart.plot)
library(data.table)
library(reshape2)
library(graphics)

#******************** downloading Data set
temp <- tempfile()
url <- "https://www.kaggle.com/ealaxi/paysim1"
download.file(url,"temp")  
rawdata <- fread("PS_20174392719_1491204439457_log.csv", header=TRUE)
unlink(temp)
Fraud <- rename(rawdata)
rm(rawdata,temp,url)

#*********************Specification of Dataset

head(Fraud)
dim(Fraud)
names(Fraud)
summary(Fraud)

# Check to see if there is any missing data
any(is.na(Fraud)) 

#####################################
#               EDA                 #
#####################################

Fraud <- Fraud %>% 
  mutate(hour = step %% 24,
         diff_balance = oldbalanceOrg - newbalanceOrig,
         diff_balance_recipient = oldbalanceDest - newbalanceDest)

# Calculate the number of reported Fraud 
number_of_Fraud <- Fraud %>% group_by(isFraud) %>% summarise(n = n())
kable(number_of_Fraud,"pandoc", caption = "Number of Fraud")

# Calculate the An illegal attempt business 
Number_of_illegal<- Fraud %>% group_by(isFlaggedFraud) %>% summarise(n = n())
kable(Number_of_illegal,"pandoc", caption = "Number of Illegal Business")

# Effect of each type of Transaction 
Number_of_each_transaction <- Fraud %>% group_by(type) %>% summarise(n = n())
kable(Number_of_each_transaction, 
      "pandoc", 
      caption = "Number of Each Type of Transaction", 
      align = "c")

Fraud %>% group_by(type) %>% 
  ggplot(aes(type, fill = factor(isFraud))) +
  geom_bar(stat = "count") + scale_y_log10() +
  scale_fill_manual(values = c("pink", "blue"))+
  xlab("Type of Transaction") +
  ylab("Log Transformed Number of Transaction") +
  ggtitle("Number of Transaction for each Type")+
  theme(axis.text.x = element_text(angle = 90 ,hjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill= guide_legend(title= "IsFraud"))

# Number of Fraud in type of Transaction
Number_of_Fraud_in_each_type <- Fraud %>% filter(isFraud == 1) %>% group_by(type) %>% 
  summarise(n = n())
kable(Number_of_Fraud_in_each_type, 
      "pandoc", 
      caption = "Number of Fraud in Each type of Transaction", 
      align = "c")

# Effect of hours on number of fraud 
Fraud %>% mutate(hour = step %% 24) %>% 
  filter(type %in% c("CASH_OUT","TRANSFER")) %>% 
  group_by(hour) %>% 
  ggplot(aes(hour, fill = factor(isFraud))) + 
  scale_fill_manual(values = c("pink","blue"))+
  geom_bar(stat = "count")+ scale_y_log10() +
  facet_grid(type ~ .) +
  xlab("Hour") + ylab("Count") +
  ggtitle("Number of Transaction versus of Hour") +
  guides(fill = guide_legend(title = "isFraud"))

# calculate the percentage of Fraud in two transaction payment based on hour
Fraud %>% filter(type %in% c("CASH_OUT","TRANSFER")) %>% 
  ggplot(aes(hour,fill= as.factor(isFraud))) + 
  geom_density(alpha = 0.5)+
  scale_fill_manual(values = c("pink",'blue'))

percentage_of_fraud_based_hour<- Fraud %>% mutate(hour = step %% 24) %>% 
  filter(type %in% c("TRANSFER","CASH_OUT")) %>% 
  group_by(hour) %>% 
  summarise(Yes_Fraud = mean(isFraud == 1),
            No_Fraud = mean(isFraud == 0),
            percent_of_Number_of_Fraud = Yes_Fraud * 100/(Yes_Fraud+No_Fraud)) %>%
  arrange(desc(percent_of_Number_of_Fraud)) %>% select(hour, percent_of_Number_of_Fraud) 
kable(percentage_of_fraud_based_hour, "pandoc", caption = "percentage of fraud versus on hour of transaction in two method of payment")

############################################################
#                           Amount                         #
############################################################
#Effect of Amount on Fraud 
Fraud_amount <- Fraud %>% filter(isFraud == 1) %>% ggplot(aes(amount)) +
  geom_histogram(fill = "blue", col= "blue", bins = 100) +
  ggtitle("Fraudulent")+ ylab("Number of Transaction")+ xlab("Amount($)")
NoFraud_amount <- Fraud %>% filter(isFraud == 0) %>% ggplot(aes(amount)) +
  geom_histogram(fill = "pink", col= "pink", bins = 100) +
  ggtitle("None Fraudulent")+ ylab("Number of Transaction")+ xlab("Amount($)")
grid.arrange(Fraud_amount,NoFraud_amount,nrow = 2, top="Figure ? -Distribution of amount versus number of Transaction") 

Fraud %>% filter(type %in% c("CASH_OUT","TRANSFER")) %>% 
  ggplot(aes(amount,fill= as.factor(isFraud))) + 
  geom_density(alpha = 0.5)+
  scale_fill_manual(values = c("pink",'blue'))


# amount of Transaction versus Fraud and none-Fraud
Fraud %>% group_by(isFraud) %>%
  ggplot(aes(y = amount, fill = as.factor(isFraud))) + 
  geom_boxplot() + scale_y_log10()+
  scale_fill_manual(values = c("pink","blue"))+
    guides(fill = guide_legend(title = "IsFraud"))+
  ylab("Amount") + xlab("IsFraud") +
  theme(axis.text.x = element_blank())+
  ggtitle("Amount of fraudulent vs non−fradulent Transaction")+
  theme(plot.title = element_text(hjust = 0.5))

#effect of hour on amount of fraud
Fraud %>% 
  filter(type %in% c("CASH_OUT","TRANSFER")) %>% 
  mutate(hour = step %% 24) %>%
  filter(isFraud == 1) %>% 
  group_by(hour) %>% ggplot(aes(hour, amount, fill=type)) +
  geom_col() + 
  scale_y_log10() + 
  facet_grid(type ~.)


#########################################################
#   Difference Balance/Difference Balance Recipient     #
#########################################################

p1 <- Fraud %>% filter(type %in% c("CASH_OUT","TRANSFER")) %>% 
  ggplot() +
  geom_histogram(aes(diff_balance), bins = 30 , fill ="blue") + 
  scale_x_log10()
p2<- Fraud %>% filter(type %in% c("CASH_OUT","TRANSFER"))%>% 
  ggplot() +
  geom_histogram(aes(diff_balance_recipient), bins = 30 , fill ="pink") + 
  scale_x_log10()

grid.arrange(p1,p2, nrow = 2)

Fraud %>% filter(type %in% c("CASH_OUT","TRANSFER")) %>% 
  filter(isFraud == 1) %>% 
  ggplot(aes(amount,diff_balance_recipient,col = type)) +
  geom_point() +
  facet_grid( .~ type)

Fraud %>% group_by(type)%>% 
  summarise(number = n()/nrow(Fraud)* 100) %>% 
  ggplot(aes(x="", y=order(number), fill=type)) +
  geom_bar(stat="identity", width=2) +
  coord_polar("y")+
  theme_void() + 
  geom_text(aes(label = number), color = "black", size=3) +
  scale_fill_brewer(palette="Set1")

# effect of hours on Difference balance  
Diff_balance_based_on_six_hour <- Fraud %>% 
  mutate(Diff_balance = (oldbalanceOrg-newbalanceOrig),
                 hour = step %% 24) %>%
  filter(type %in% c("TRANSFER","CASH_OUT") & hour %in% c(1:7)) 

Diff_balance_based_on_six_hour %>%
  ggplot(aes(hour,Diff_balance, fill = factor(isFraud))) +
  geom_col() + 
  scale_y_continuous() +
  theme(axis.text.y = element_blank()) + 
  ylab("Difference Balance") +
  xlab("Hour") +
  ggtitle("Difference of balance versus hours on two different type of payment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid( type ~ .)

Diff_balance_based_on_six_hour %>% filter(isFraud == 1) %>%
  summarise(max(Diff_balance))

# effect of hours on Difference balance of rectipien  
Diff_balance_rectipien_based_on_six_hour<- Fraud %>% 
  mutate(Diff_balance_recipien = abs(oldbalanceDest-newbalanceDest),
                 hour = step %% 24) %>%
  filter(type %in% c("TRANSFER","CASH_OUT") & hour %in% c(1:7))

Diff_balance_rectipien_based_on_six_hour %>%
  ggplot(aes(hour,Diff_balance_recipien, fill = factor(isFraud))) +
  geom_col() + 
  scale_y_continuous() +
  theme(axis.text.y = element_blank()) + 
  ylab("Difference Balance") +
  xlab("Hour") +
  ggtitle("Difference of balance versus hours on two different type of payment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid( type ~ .)

Diff_balance_rectipien_based_on_six_hour %>% filter(isFraud == 1) %>%
  summarise(max(Diff_balance_recipien))


  
Fraud <- Fraud %>% filter(type %in% c("CASH_OUT", "TRANSFER")) %>%
  mutate(hour = step %% 24,
         diff_balance = oldbalanceOrg - newbalanceOrig,
         diff_balance_recipient = oldbalanceDest - newbalanceDest,
         isFraud = as.factor(isFraud)) %>%
  select(hour, type, amount, diff_balance, diff_balance_recipient,
         isFraud)

########################################################
#                      Modeling                        #
########################################################
set.seed(1)
dat <- sample_frac(Fraud, size = 0.4, replace = FALSE)

index <- createDataPartition(dat$isFraud, times = 1, p = 0.2, list = FALSE)
train <- dat %>% slice(-index)
validation <- dat %>% slice(index)

# train dataset split to train_set and test_set 
index_test <- createDataPartition(train$isFraud, times = 1, p= 0.2, list = FALSE)
train_set <- train %>% slice(-index_test)
test_set <- train %>% slice(index_test)

# Number of rows of each datasets
train_row = nrow(train)
validation_row = nrow(validation)
train_set_row = nrow(train_set)
test_set_row = nrow(test_set)
kable(rbind(train_row,validation_row,train_set_row,test_set_row), "pandoc", caption = "The number of Rows of each Datasets", align = "c")

# Guessing  method
set.seed(1, sample.kind = "Rounding")
y_hat_gussing <- sample(c("1","0"), length(index_test), replace = TRUE) %>% factor()
Accuracy_gussing <- confusionMatrix(y_hat_gussing,test_set$isFraud)$overall["Accuracy"]
sensitivity_gussing <- sensitivity(y_hat_gussing,test_set$isFraud)
specificity_gussing <- specificity(data = y_hat_gussing, reference = test_set$isFraud)
F_1 <- F_meas(y_hat_gussing, test_set$isFraud)
Results <- tibble(method = "Guessing",
                  Accuracy = Accuracy_gussing,
                  sensitivity = sensitivity_gussing,
                  specificity = specificity_gussing,
                  F_1 = F_1)

kable((Results[1:1,]),"pandoc", caption = "Model Specification", align = "c")


# effect of hour on accuracy
y_hat_timeeffect <- ifelse(test_set$hour>=2 & test_set$hour <= 6 ,"1","0") %>% factor() 
Accuracy_timeeffect <- confusionMatrix(y_hat_timeeffect,test_set$isFraud)$overall["Accuracy"]
sensitivity_timeeffect <- sensitivity(y_hat_timeeffect,test_set$isFraud)
specificity_timeeffect <- specificity(data = y_hat_timeeffect, reference = test_set$isFraud)
F_1_timeeffect <- F_meas(y_hat_timeeffect, test_set$isFraud)
Results <- add_row(Results, 
                   method = "Timeeffect",
                   Accuracy = Accuracy_timeeffect,
                   sensitivity = sensitivity_timeeffect,
                   specificity = specificity_timeeffect,
                   F_1 = F_1_timeeffect)


kable((Results[1:2,]), "pandoc", caption = "Model Specification", align = "c")


# Rpart Classification Model
rpart <- train(isFraud ~ .,
                method = "rpart",
                tuneGrid = data.frame(cp = seq(0, 0.1, len = 25)),
                data = train_set)
plot(rpart) 

#Decision Tree
rpart.plot(rpart$finalModel, type = 5, main = "Decision Tree", cex = 0.75)

y_hat_rpart <- predict(rpart, test_set)

Accuracy_rpart <- mean(y_hat_rpart == test_set$isFraud)
sensitivity_rpart <- sensitivity(data = y_hat_rpart, reference = test_set$isFraud)
specificity_rpart <- specificity(data = y_hat_rpart, reference = test_set$isFraud)
F_1_rpart <- F_meas(y_hat_rpart, test_set$isFraud)
F_1_rpart
Results <- add_row(Results, 
                   method = "Rpart", 
                   Accuracy = Accuracy_rpart,
                   sensitivity = sensitivity_rpart,
                   specificity = specificity_rpart,
                   F_1 = F_1_rpart)

kable((Results[1:3,]), "pandoc", caption = "Model Specification", align = "c")

ggplot(varImp(rpart)) + ggtitle("Predictive importance of variables in Rpart") +
  theme(plot.title = element_text(hjust = 0.5))
#####################################################
#                   Random Forest                   #
#####################################################
set.seed(1)
Random_forest <- train(isFraud ~ ., 
                       data = train_set,
                       trControl = trainControl(method = "cv", number = 5),
                       importance = TRUE, 
                       method = "rf", 
                       ntree = 25, 
                       tuneGrid = data.frame(mtry = seq(1,6,1))) 

# Graph of Importance of Features
ggplot(varImp(Random_forest)) + 
  ggtitle("Predictive importance of variables in Random Forest") +
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Variables") + ylab("Percentage of variable Importancy")

# calculate the Accuracy,sensitivity, specificity and F1 of Forest Model
y_hat_rf <- predict(Random_forest, test_set)

Accuracy_rf <- confusionMatrix(y_hat_rf,test_set$isFraud)$overall["Accuracy"]
sensitivity_rf <- sensitivity(y_hat_rf,test_set$isFraud)
specificity_rf <- specificity(y_hat_rf,test_set$isFraud)
F_1_rf <- F_meas(y_hat_rf, test_set$isFraud)

Results <- add_row(Results, 
                   method = "Random Forest", 
                   Accuracy = Accuracy_rf,
                   sensitivity = sensitivity_rf,
                   specificity = specificity_rf,
                   F_1 = F_1_rf)
kable((Results[1:4,]), 
      "pandoc", 
      caption = "Model Specification", 
      align = "c")


# Random Forest for Validation
set.seed(1, sample.kind = "Rounding")
y_hat_rf_validation <- predict(Random_forest, validation)

Accuracy_rf_validation <- confusionMatrix(y_hat_rf_validation, validation$isFraud)$overall["Accuracy"]
sensitivity_rf_validation <- sensitivity(y_hat_rf_validation, validation$isFraud)
specificity_rf_validation <- specificity(y_hat_rf_validation, validation$isFraud)
F_1_rf_validation <- F_meas(y_hat_rf_validation, validation$isFraud)

Results <- add_row(Results, 
                   method = "Random Forest-validation", 
                   Accuracy = Accuracy_rf_validation,
                   sensitivity = sensitivity_rf_validation,
                   specificity = specificity_rf_validation,
                   F_1 = F_1_rf_validation)
kable((Results[1:5,]), 
      "pandoc", 
      caption = "Model Specification", 
      align = "c")

