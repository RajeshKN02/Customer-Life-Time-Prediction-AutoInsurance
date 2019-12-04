getwd()
setwd('/Users/binay/Desktop/Rajesh/CLTV_Insurance')
ls()
rm(list=ls())


################   packages to be installed      ############################

packages <- c("magrittr","dplyr","ggplot2","MASS")
for(p in packages){
  suppressPackageStartupMessages(library(p,quietly = TRUE,character.only = TRUE))
}

####################       Customer Data        ####################################

library(data.table)
Customer_Data <- fread("https://raw.githubusercontent.com/Rajesh/CustomerLifeTimeValue-Prediction/master/Project_Modeled_Data.csv")
glimpse(Customer_Data)
summary(Customer_Data)


CustDataFinal <- Customer_Data[,2:23]
glimpse(CustDataFinal)
summary(CustDataFinal)


#################################### Exploratory Data Analysis ########################################################


# Check for any missing Values

apply(CustDataFinal,2,function(x) {any(is.na(CustDataFinal))})
sum(is.na(CustDataFinal))

# Income Range Validation of Population having more than $50,000 as Annual Incomce

glimpse(CustDataFinal)
dim(CustDataFinal)
sum((CustDataFinal$Income >=50000)/dim(CustDataFinal)[1])*100 # Here we got almost 35% Population has >= $50000 Income Range


glimpse(CustDataFinal)
names(CustDataFinal)
#"State"                          "Location.Code"                  "Education"                     
# "Gender"                         "Coverage"                       "EmploymentStatus"              
# "Income"                         "Marital.Status"                 "Monthly.Premium.Auto"          
# "No.Of.Open.Complaints"          "No.Of.Policies"                 "Renew.Offer.Type"              
# "Vehicle.Class"                  "Customer.Lifetime.Value"        "Vehicle.Size"                  
# "Policy"                         "Policy.Type"                    "Sales.Channel"                 
# "Total.Claims.Amount"            "Monthly.Since.Last.Claims"      "Months.Since.Policy.Inception"
# "Response"

# Try To Change Colnames which will be compatible for further Analysis
glimpse(CustDataFinal)

# setnames(CustDataFinal,old = c(CustDataFinal$`Location Code`),new = C("Location.Code"))
names(CustDataFinal)[2] <- "Location.Code" 
names(CustDataFinal)[8] <- "Marital.Status"
names(CustDataFinal)[9] <- "Monthly.Premium.Auto"
names(CustDataFinal)[10] <- "No.Of.Open.Complaints"
names(CustDataFinal)[11] <- "No.Of.Policies"
names(CustDataFinal)[12] <- "Renew.Offer.Type"
names(CustDataFinal)[13] <- "Vehicle.Class"
names(CustDataFinal)[14] <- "Customer.Lifetime.Value"
names(CustDataFinal)[15] <- "Vehicle.Size"
names(CustDataFinal)[17] <- "Policy.Type"
names(CustDataFinal)[18] <- "Sales.Channel"
names(CustDataFinal)[19] <- "Total.Claims.Amount"
names(CustDataFinal)[20] <- "Monthly.Since.Last.Claims"
names(CustDataFinal)[21] <- "Months.Since.Policy.Inception"

# To understand Income Level against Education

ggplot(data = CustDataFinal,aes(Income,color = "Red")) +
  geom_density() +
    facet_wrap(~Education) +
      theme(axis.title.x = element_text(angle = 90,colour = "blue"))


# To understand Income Level against Vehicle Class Owned

ggplot(data=CustDataFinal,aes(Income, color= 'red')) +
  geom_density()  +
  facet_wrap(~ Vehicle.Class) + 
  theme(axis.text.x = element_text(angle = 90, colour = 'blue' ))


# To understand Income Level against Marital Status

ggplot(data = CustDataFinal,aes(Income,color='red')) +
  geom_density() +
    facet_wrap(~Marital.Status) +
      theme(axis.text.x = element_text(angle = 90,colour = 'blue'))


# To understand Total Claim Amount against Vehicle Size

ggplot(data=CustDataFinal,aes(Total.Claims.Amount, color= 'red')) +
  geom_density()  +
  facet_wrap(~Vehicle.Size) + 
  theme(axis.text.x = element_text(angle = 90, color = 'blue' ))


# To understand Total Claim Amount against Vehicle Class

ggplot(data = CustDataFinal,aes(Total.Claims.Amount,color='red')) +
  geom_density() +
    facet_wrap(~Vehicle.Class) +
      theme(axis.text.x = element_text(angle = 90,colour = 'blue'))


# To understand the Outlier State(Factorial) vs Income

ggplot(data=CustDataFinal,aes(factor(State), Income)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('State')
  

# To understand the Outlier State(Factorial) vs Income

ggplot(data = CustDataFinal,aes(State,Income)) + geom_boxplot() + 
    theme(axis.text.x = element_text(angle = 90,hjust = 1)) +xlab('State')


# To understand the Outlier Coverage(Factorial) vs Income

ggplot(data = CustDataFinal,aes(as.factor(Coverage), Income)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Coverage')



# To understand the Outlier Education(Factorial) vs Income

ggplot(data = CustDataFinal,aes(factor(Education), Income)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Education')



# To understand the Outlier Education(Factorial) vs Customer.Lifetime.Value

ggplot(data = CustDataFinal,aes(factor(Education), Customer.Lifetime.Value)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Education')



# To understand the Outlier Coverage(Factorial) vs Customer.Lifetime.Value

ggplot(data = CustDataFinal,aes(factor(Coverage), Customer.Lifetime.Value)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Coverage')



# To understand the Outlier Vehicle.Class(Factorial) vs Customer.Lifetime.Value

ggplot(data = CustDataFinal,aes(factor(Vehicle.Class), Customer.Lifetime.Value)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Vehicle.Class')



# To understand the Outlier State(Factorial) vs Total.Claim.Amount

ggplot(data = CustDataFinal,aes(factor(State), Total.Claims.Amount)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('State')



# To understand the Outlier State(Factorial) vs Customer.Lifetime.Value

ggplot(data = CustDataFinal,aes(factor(State), Customer.Lifetime.Value)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('State')



# To understand the Outlier State(Factorial) vs Monthly.Premium.Auto

ggplot(data = CustDataFinal,aes(factor(State), Monthly.Premium.Auto)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('State')
  
  
  
  
# Check for Vehicle Size,Coverage,Income with total count and display in ggplot

CustDataFinal %>% select(Vehicle.Size,Coverage,Income) %>% filter(Vehicle.Size==c("Medium","Large","Small")) %>%
  arrange(Coverage) %>% group_by(Vehicle.Size,Coverage) %>% summarise(Total_Count=n()) %>%
    ggplot(aes(Coverage,Total_Count)) + geom_bar(aes(fill= Vehicle.Size),position = "dodge",stat = "Identity") +
      theme_classic() + theme(axis.text.x = element_text(angle = 90,hjust = 1))




CustDataFinal %>% select(Vehicle.Class, Coverage, Customer.Lifetime.Value) %>% filter(Vehicle.Class == c('Two-Door Car','Four-Door Car','SUV','Luxury SUV','Luxury Car','Sports Car')) %>% 
  arrange(Coverage) %>% group_by(Vehicle.Class, Coverage) %>% summarize(Count = n()) %>% ggplot(aes(Coverage, Count)) + 
  geom_bar(aes(fill = Vehicle.Class), position = 'dodge', stat = 'identity') + 
  theme_classic() +theme(axis.text.x = element_text(angle = 90, hjust =1))



# Check the  Outlier
boxplot(CustDataFinal$Customer.Lifetime.Value, data = CustDataFinal , xlab = "Customer LifeTime Value")

ggplot(CustDataFinal, aes(x=Customer.Lifetime.Value)) + geom_histogram(bins = 20, col = 'blue') + theme_classic()
ggplot(CustDataFinal, aes(x=Income)) + geom_histogram(bins = 30, col = 'orange') + theme_classic()
ggplot(CustDataFinal, aes(x=Monthly.Premium.Auto)) + geom_histogram(bins = 30, col = 'cyan') + theme_classic()
ggplot(CustDataFinal, aes(x=Months.Since.Policy.Inception)) + geom_histogram(bins = 30, col = 'green') + theme_classic()





################################### DATA PREPROCESSING   ################################################################


############  MANAGING OUTLIERS  ######################

ManageOutlier <- function(x){
  quantiles <- quantile( x, c(.00, .97 ) )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}
CustDataFinal$Customer.Lifetime.Value <- ManageOutlier(CustDataFinal$Customer.Lifetime.Value)
min(CustDataFinal$Customer.Lifetime.Value)     # 1898.008
max(CustDataFinal$Customer.Lifetime.Value)     # 26199.54
mean(CustDataFinal$Customer.Lifetime.Value)    # 7730.575
median(CustDataFinal$Customer.Lifetime.Value)  # 5780.182
write.csv(CustDataFinal$Customer.Lifetime.Value,"Outliers_1.csv")

getwd()
Out_1 <- fread("/Users/Rajesh/Desktop/CLTV_Insurance/Outliers_1.csv")
glimpse(Out_1)

## After Managing Outlier

boxplot(CustDataFinal$Customer.Lifetime.Value, data = CustDataFinal , xlab = "Customer LifeTime Value")


############ Factoring the Data ######################

# Converting all the Character data type to Factor 

################# Factoring the Data ######################

glimpse(CustDataFinal)


CustDataFinal$State = factor(CustDataFinal$State,
                           levels = c('Washington','Arizona','Nevada','Oregon','California'),
                           labels = c(1, 2, 3, 4, 5))

CustDataFinal$Gender = factor(CustDataFinal$Gender,
                            levels = c('M', 'F'),
                            labels = c(1,2))

CustDataFinal$Vehicle.Size = factor(CustDataFinal$Vehicle.Size,
                                  levels = c('Large','Medsize','Small'),
                                  labels = c(1,2,3))

CustDataFinal$Policy = factor(CustDataFinal$Policy,
                            levels = c('Corporate L1','Corporate L2','Corporate L3','Personal L1','Personal L2' ,'Personal L3', 'Special L1', 'Special L2', 'Special L3'),
                            labels = c(1,2,3,4,5,6,7,8,9))

CustDataFinal$Coverage = factor(CustDataFinal$Coverage,
                              levels = c('Basic','Extended','Premium'),
                              labels = c(1,2,3))

CustDataFinal$EmploymentStatus = factor(CustDataFinal$EmploymentStatus, 
                                      levels = c('Employed','Unemployed','Disabled','Medical Leave','Retired'),
                                      labels = c(1,2,3,4,5))

CustDataFinal$Response = factor(CustDataFinal$Response,
                              levels = c('No','Yes'),
                              labels = c(1,2))

CustDataFinal$Education = factor(CustDataFinal$Education,
                               levels = c('High School or Below', 'Bachelor','College', 'Master', 'Doctor' ),
                               labels = c(1,2,3,4,5))

CustDataFinal$Location.Code = factor(CustDataFinal$Location.Code,
                                   levels = c('Rural', 'Suburban', 'Urban'),
                                   labels = c(1,2,3))

CustDataFinal$Marital.Status = factor(CustDataFinal$Marital.Status,
                                    levels = c('Single','Married','Divorced'),
                                    labels = c(1,2,3))

CustDataFinal$Policy.Type = factor(CustDataFinal$Policy.Type,
                                 levels = c('Corporate Auto', 'Personal Auto', 'Special Auto'),
                                 labels = c(1,2,3))

CustDataFinal$Renew.Offer.Type = factor(CustDataFinal$Renew.Offer.Type,
                                      levels = c('Offer1','Offer2','Offer3','Offer4'),
                                      labels = c(1,2,3,4))

CustDataFinal$Sales.Channel = factor(CustDataFinal$Sales.Channel,
                                   levels = c('Agent', 'Call Center', 'Branch','Web'),
                                   labels = c(1,2,3,4))

CustDataFinal$Vehicle.Class = factor(CustDataFinal$Vehicle.Class,
                                   levels = c('Two-Door Car','Four-Door Car','SUV','Luxury Car','Luxury SUV', 'Sports Car'),
                                   labels = c(1,2,3,4,5,6))



#########################  Splitting the Dataset into trainingData  and testData    ################################


library(caTools)
set.seed(123)
split <- sample.split(CustDataFinal$Customer.Lifetime.Value,SplitRatio = 0.75)
training_data <- subset(CustDataFinal,split== TRUE)
test_data <- subset(CustDataFinal,split== FALSE)
glimpse(training_data)
dim(test_data)


#########################  BUILDING / VALIDATING MULTIPLE LINEAR REGRESSION MODEL FOR CLV Predicton ###################

# Note : Backward Elimination appraoch to validate the appropriate Value with p-Value > 0.5
#      : Response Variable needs to be Transformed for predicting  CLTV
#      : Validate the Linear Regression for (Target Variable)^2

#######################################################################################################################


library(MASS)
Regressor <- lm(formula = (Customer.Lifetime.Value)^2~.,data = training_data)
summary(Regressor)
# Residual standard error: 1.4e+08 on 6801 degrees of freedom
# Multiple R-squared:  0.1286,	Adjusted R-squared:  0.1225 
# F-statistic: 20.91 on 48 and 6801 DF,  p-value: < 2.2e-16

stepAIC(Regressor)

# Step:  AIC=256987.7
# (Customer.Lifetime.Value)^2 ~ Gender + Coverage + EmploymentStatus + 
#  Marital.Status + Monthly.Premium.Auto + No.Of.Open.Complaints + 
#  No.Of.Policies + Renew.Offer.Type + Vehicle.Size + Sales.Channel


#       Df                Sum of Sq        RSS    AIC
#<none>                                1.3373e+20 256988
#- Sales.Channel          3 1.2630e+17 1.3385e+20 256988
#- Vehicle.Size           2 9.0498e+16 1.3382e+20 256988
#- Gender                 1 5.2104e+16 1.3378e+20 256988
#- Marital.Status         2 1.1021e+17 1.3384e+20 256989
#- EmploymentStatus       4 2.1984e+17 1.3395e+20 256991
#- Coverage               2 2.3806e+17 1.3396e+20 256996
#- No.Of.Open.Complaints  1 2.1829e+17 1.3394e+20 256997
#- Renew.Offer.Type       3 4.3578e+17 1.3416e+20 257004
#- No.Of.Policies         1 3.7434e+17 1.3410e+20 257005
#- Monthly.Premium.Auto   1 1.4721e+19 1.4845e+20 257701
# Coefficients:
# (Intercept)                Gender2              Coverage2              Coverage3      EmploymentStatus2  
#   -42626600                5548537               -9831114              -20276038              -13376021  
# EmploymentStatus3      EmploymentStatus4      EmploymentStatus5        Marital.Status2        Marital.Status3  
#     -14338628               -4599148               -1667559                7523705               13041100  
# Monthly.Premium.Auto  No.Of.Open.Complaints         No.Of.Policies      Renew.Offer.Type2      Renew.Offer.Type3  
#        1512516               -6175732               -3132417              -18078856               -6536315  
# Renew.Offer.Type4          Vehicle.Size2          Vehicle.Size3         Sales.Channel2         Sales.Channel3  
#       -18633685               10606524               13551121               10076296                7269922  
# Sales.Channel4  
#      -2763  

glimpse(training_data)
StepRegressor <- lm(formula = (Customer.Lifetime.Value)^2~Coverage+EmploymentStatus+Monthly.Premium.Auto+
                      No.Of.Open.Complaints+No.Of.Policies+Renew.Offer.Type+Response,data = training_data)

summary(StepRegressor)
#  Residual standard error: 140100000 on 6836 degrees of freedom
#  Multiple R-squared:  0.1231,	Adjusted R-squared:  0.1215 
#  F-statistic: 73.85 on 13 and 6836 DF,  p-value: < 2.2e-16

stepAIC(StepRegressor) # 256990.9
extractAIC(StepRegressor)


# Validating the Linear Regression for log(Target Variable)

Regressor <- lm(formula = log(Customer.Lifetime.Value) ~ .,data = training_data)

summary(Regressor)

# Residual standard error: 0.5447 on 6801 degrees of freedom
# Multiple R-squared:  0.2638,	Adjusted R-squared:  0.2586 
# F-statistic: 50.77 on 48 and 6801 DF,  p-value: < 2.2e-16

stepAIC(Regressor)
#  Step:  AIC=-8296.49
#  log(Customer.Lifetime.Value) ~ Coverage + EmploymentStatus + 
#  Marital.Status + Monthly.Premium.Auto + No.Of.Open.Complaints + 
#  No.Of.Policies + Renew.Offer.Type + Vehicle.Class + Vehicle.Size


#    DF                      Sum of Sq    RSS     AIC
# <none>                               2027.2 -8296.5
# - Coverage               2     2.058 2029.3 -8293.5
# - Vehicle.Size           2     2.587 2029.8 -8291.8
# - Marital.Status         2     3.701 2030.9 -8288.0
# - No.Of.Open.Complaints  1     7.169 2034.4 -8274.3
# - EmploymentStatus       4    10.065 2037.3 -8270.6
# - Monthly.Premium.Auto   1    12.237 2039.4 -8257.3
# - Vehicle.Class          5    18.508 2045.7 -8244.2
# - Renew.Offer.Type       3    18.456 2045.7 -8240.4
# - No.Of.Policies         1   113.222 2140.4 -7926.2


#    Coefficients:
#     (Intercept)              Coverage2              Coverage3      EmploymentStatus2      EmploymentStatus3  
#       7.945714               0.065328               0.104498              -0.092985              -0.063821  
#   EmploymentStatus4      EmploymentStatus5        Marital.Status2        Marital.Status3   Monthly.Premium.Auto  
#     -0.075830              -0.041978               0.054531               0.064093               0.006172  
#   No.Of.Open.Complaints         No.Of.Policies      Renew.Offer.Type2      Renew.Offer.Type3      Renew.Offer.Type4  
#           -0.035399               0.054469              -0.116631              -0.063011              -0.123998  
#     Vehicle.Class2         Vehicle.Class3         Vehicle.Class4         Vehicle.Class5         Vehicle.Class6  
#       -0.003177               0.162702               0.076555               0.072467               0.160885  
#   Vehicle.Size2          Vehicle.Size3  
#     0.059404               0.070418  

glimpse(training_data)
StepRegressor <- lm(formula = log(Customer.Lifetime.Value) ~ Coverage + EmploymentStatus + Marital.Status + Monthly.Premium.Auto + 
                      No.Of.Open.Complaints + No.Of.Policies + Renew.Offer.Type + Vehicle.Class+Policy ,data = training_data)

summary(StepRegressor)

#   Residual standard error: 0.5449 on 6822 degrees of freedom
#   Multiple R-squared:  0.2608,	Adjusted R-squared:  0.2579 
#   F-statistic: 89.14 on 27 and 6822 DF,  p-value: < 2.2e-16

extractAIC(StepRegressor) # -8288.809


# Validating the Linear Regression for sqrt(Target Variable)

Regressor <- lm(formula = sqrt(Customer.Lifetime.Value) ~ ., data = training_data)

summary(Regressor)
#  Residual standard error: 25.06 on 6801 degrees of freedom
#  Multiple R-squared:  0.2192,	Adjusted R-squared:  0.2137 
#  F-statistic: 39.78 on 48 and 6801 DF,  p-value: < 2.2e-16

stepAIC(Regressor) # 44155.7

StepRegressor <- lm(formula = sqrt(Customer.Lifetime.Value) ~ EmploymentStatus + Marital.Status + Monthly.Premium.Auto + 
                  No.Of.Open.Complaints + No.Of.Policies + Renew.Offer.Type + Vehicle.Class + Policy +  Sales.Channel,
                  data = training_data)


summary(StepRegressor)

#    Residual standard error: 25.06 on 6821 degrees of freedom
#    Multiple R-squared:  0.2166,	Adjusted R-squared:  0.2134 
#    F-statistic: 67.35 on 28 and 6821 DF,  p-value: < 2.2e-16

extractAIC(StepRegressor) # 44162.87



# Validating Linear Regression for 1/(Target Variable)

Regressor <- lm(formula = 1/Customer.Lifetime.Value~.,data = training_data)
summary(Regressor)

#   Residual standard error: 8.993e-05 on 6801 degrees of freedom
#   Multiple R-squared:  0.3354,	Adjusted R-squared:  0.3308 
#   F-statistic: 71.52 on 48 and 6801 DF,  p-value: < 2.2e-16

extractAIC(Regressor) #   -127586.2
stepAIC(Regressor) # AIC= -127608.7



#### BASED ON THE OUTPUT FROM BACKWARD ELIMINATION and fitting the model with identified significant variables ####

StepRegressor <- lm(formula = 1/Customer.Lifetime.Value ~ Location.Code + Education + Coverage + EmploymentStatus + Marital.Status + Monthly.Premium.Auto + No.Of.Open.Complaints + No.Of.Policies + Renew.Offer.Type + Vehicle.Class, data = training_data)

summary(StepRegressor)

#  Residual standard error: 9e-05 on 6824 degrees of freedom
#  Multiple R-squared:  0.3323,	Adjusted R-squared:  0.3298 
#  F-statistic: 135.8 on 25 and 6824 DF,  p-value: < 2.2e-16

extractAIC(StepRegressor) # -127599.6


# PREDICTING CUSTOMER LIFETIME VALUE FOR THE TEST DATASET

PredictedValue = predict(StepRegressor, newdata = test_data)

summary(PredictedValue)

#      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -4.505e-05  1.485e-04  1.987e-04  1.903e-04  2.411e-04  3.040e-04 


# Validating the Predicted Values and Converting / Transforming the Predicted values

PredictedValuecheck <- 1/PredictedValue
summary(PredictedValuecheck)

#       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   -633000    4132    5005    5877    6685  344164 

write.csv(PredictedValuecheck, "MLRPredictedValueCheck.csv")

# plotting the values to validate the data

ggplot(test_data, aes(test_data$Customer.Lifetime.Value, PredictedValuecheck, color = 'red')) +
  geom_point(color = 'red') + geom_abline(color = 'blue') + 
   ggtitle('Plotting Predicted Customer Lifetime Values') +
     xlab('Actual Values') + ylab('Predicted values')


# Let's Validate some Random values to check the accuracy of prediction.
# Random check on the Prediction - Actual Value = 7328.977
# unique(sort(CustDataFinal$No.Of.Policies))
unique(test_data$Location.Code)
glimpse(test_data)
 pred = predict(StepRegressor, data.frame(
     Location.Code = 'Suburban',  Education = 'Master',
     EmploymentStatus = 'Employed' ,
     Marital.Status = 'Married',
     Coverage = 'Extended' ,
     Renew.Offer.Type = 'Offer2',
     Vehicle.Class = 'Four-Door Car',
     No.Of.Policies= 8,
     Monthly.Premium.Auto = 91 ,
     No.Of.Open.Complaints = 0
   ))
 pred = 1/pred
 ############################## End of Multiple Linear Regression ##############################################
 
 #---------------------------------------------------------------------------------------------------#
 
 
 
 ############################## Random Forest Model ##############################################
 
 
 
 library(randomForest)
 library(caret)
 
 
 # After multiple retries, reducing the number of trees to 20 and mtry = 5
randomforestfitting <- randomForest(Customer.Lifetime.Value ~ ., training_data, ntree = 20, mtry = 5)
summary(randomforestfitting) 


# Predicting Training data with all variables.
randomforestpredictionfulltrain <- predict(randomforestfitting, newdata = training_data)

summary(randomforestpredictionfulltrain)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   2003    4140    6040    7775    9806   25651 

# Checking R Sqaured value 
R2 <- 1- (sum((training_data$Customer.Lifetime.Value-randomforestpredictionfulltrain)^2) / sum((training_data$Customer.Lifetime.Value - mean(training_data$Customer.Lifetime.Value))^2))
print(R2 * 100) # R Squared Value :-  94.14591 %


#Checking RMSE
caret::RMSE(randomforestpredictionfulltrain, training_data$Customer.Lifetime.Value) # RMSE : 1369.619



# Checking / Validating Variable Importance

varImp = importance(randomforestfitting)

varImpPlot(randomforestfitting)


# Fitting the model using only the Important Variables.

names(training_data)
randomforestfittingwithimp <- randomForest(Customer.Lifetime.Value ~ No.Of.Policies + Monthly.Premium.Auto + 
Vehicle.Class + Total.Claims.Amount + Monthly.Since.Last.Claims + Months.Since.Policy.Inception + Income + Policy + State + Education + Sales.Channel, data = training_data, ntree = 20, mtry = 5)


# Predicting on Train Data
randomForestpredictionTrain <- predict(randomforestfittingwithimp, newdata = training_data)

summary(randomForestpredictionTrain)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   2039    4047    5844    7758    9712   26150 

# Checking R Sqaured value 
R2 <- 1- (sum((training_data$Customer.Lifetime.Value-randomForestpredictionTrain)^2) / sum((training_data$Customer.Lifetime.Value - mean(training_data$Customer.Lifetime.Value))^2))
print(R2 * 100) # R Squared Value :- 94.85744 %


# Predicting for Test Data

randomForestpredictionTest <- predict(randomforestfittingwithimp, newdata =  test_data)

summary(randomForestpredictionTest)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   2066    4014    5691    7565    9898   25665 


#Calculating R Squared for Test data
R2 <- 1- (sum((test_data$Customer.Lifetime.Value-randomForestpredictionTest)^2) / sum((test_data$Customer.Lifetime.Value - mean(test_data$Customer.Lifetime.Value))^2))
print(R2 * 100) # R Squared Value : 77.55874

test_data$predictedvalues <- randomForestpredictionTest

write.csv(test_data, 'RandomForestpredictedvalues.csv')

# plotting Actual v/s Predicted Values for Training Data Set
ggplot(training_data, aes(training_data$Customer.Lifetime.Value, randomForestpredictionTrain, color = 'red')) +
  geom_point(color = 'red') + geom_abline(color = 'blue') + 
  ggtitle('Plotting Predicted Customer Lifetime Values') +
  xlab('Actual Values') + ylab('Predicted values')

# plotting Actual v/s Predicted Values for Test Data Set
ggplot(test_data, aes(test_data$Customer.Lifetime.Value, randomForestpredictionTest, color = 'red')) +
  geom_point(color = 'red') + geom_abline(color = 'blue') + 
  ggtitle('Plotting Predicted Customer Lifetime Values') +
  xlab('Actual Values') + ylab('Predicted values')


############################## End of Multiple Linear Regression ##############################################




#---------------------------------------------------------------------------------------------------#



############################## XG BOOST ##############################################

install.packages("Metrics")
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
library(Metrics)
library(Matrix)
library(caTools)


# Use the Original Dataset as the character variables are being transformed to Factor

# ls()
# rm(XGData)
XGDataset <- Customer_Data[,2:23]

glimpse(XGDataset)
summary(XGDataset)

colnames(XGDataset)

names(XGDataset)[2] <- "Location.Code" 
names(XGDataset)[8] <- "Marital.Status"
names(XGDataset)[9] <- "Monthly.Premium.Auto"
names(XGDataset)[10] <- "No.Of.Open.Complaints"
names(XGDataset)[11] <- "No.Of.Policies"
names(XGDataset)[12] <- "Renew.Offer.Type"
names(XGDataset)[13] <- "Vehicle.Class"
names(XGDataset)[14] <- "Customer.Lifetime.Value"
names(XGDataset)[15] <- "Vehicle.Size"
names(XGDataset)[17] <- "Policy.Type"
names(XGDataset)[18] <- "Sales.Channel"
names(XGDataset)[19] <- "Total.Claims.Amount"
names(XGDataset)[20] <- "Monthly.Since.Last.Claims"
names(XGDataset)[21] <- "Months.Since.Policy.Inception"



############  MANAGING OUTLIERS  ######################

XGDataset$Customer.Lifetime.Value <- ManageOutlier(XGDataset$Customer.Lifetime.Value)
boxplot(XGDataset$Customer.Lifetime.Value, data = XGDataset , xlab = "Customer LifeTime Value")

#######################################################

# Split the dataset into Train and Test

set.seed(123)
dim(XGDataset)
glimpse(XGDataset)

sum(apply(XGDataset, 2, function(x) any(is.na(x))))
sum(is.na(XGDataset))
dim(XGDataset)



split <- sample.split(XGDataset$Customer.Lifetime.Value, SplitRatio = 0.75)
xgtraindataset <- subset(XGDataset, split == TRUE)
xgTestDataSet <- subset(XGDataset, split == FALSE)
tail(xgtraindataset)
head(xgTestDataSet)
count(xgtraindataset)
count(xgTestDataSet)



# Standardizing the Train Dataset

xgtraindataset_fine <- sparse.model.matrix(Customer.Lifetime.Value~., data = xgtraindataset)
head(xgtraindataset_fine)

xgtrain_label <- xgtraindataset[,Customer.Lifetime.Value]
head(xgtrain_label)


# Here we convert into matrix for XG Boost algorithm
training_matrix <- xgb.DMatrix(data = as.matrix(xgtraindataset_fine), label=xgtrain_label)



#Standardizing the Test Dataset

xgTestDataSet_fine <- sparse.model.matrix(Customer.Lifetime.Value~.,data = xgTestDataSet)
head(xgTestDataSet_fine)

xgtest_label <- xgTestDataSet[,Customer.Lifetime.Value]
head(xgtest_label)

test_matrix <- xgb.DMatrix(data=as.matrix(xgTestDataSet_fine),label=xgtest_label)


#Standardizing minidataset for complete CLV Prediction and Model Validation

xgDataSet_fine <- sparse.model.matrix(Customer.Lifetime.Value ~., data = XGDataset)
head(xgDataSet_fine)
xgDataSet_label <- XGDataset[,Customer.Lifetime.Value]
XGDataSet_matrix <- xgb.DMatrix(data = as.matrix(xgDataSet_fine), label = xgDataSet_label)
head(XGDataSet_matrix)


# Updating the Parameters

?verbose
xgb_params <- list("objective" = "reg:linear", "eval_metric" = "rmse" , "verbose" = 2 )
watchlist <- list(train = training_matrix, test = test_matrix)


# Applying eXtreme Gradient Boost Model

xgboostmodel <- xgb.train(params = xgb_params, data = training_matrix, watchlist = watchlist, 
                          nround = 100, eta = 0.1, max.depth = 6 )



# Checking the importance of the variables

ValidateImportance <- xgb.importance(feature_names = names(training_matrix), model = xgboostmodel)
print(ValidateImportance)
head(ValidateImportance)


#                       Feature       Gain      Cover  Frequency
#1:                No.Of.Policies 0.67269455 0.12196770 0.08565824
#2:          Monthly.Premium.Auto 0.22351913 0.21304881 0.19224063
#3:                        Income 0.01817869 0.12839613 0.11660854
#4:           Total.Claims.Amount 0.01442062 0.16354942 0.09873583
#5:     Monthly.Since.Last.Claims 0.01359577 0.05931292 0.05732345
#6: Months.Since.Policy.Inception 0.01112156 0.10448712 0.06473409



# plotting to find the Important Variables for predicting Customer Life Time Value
xgb.plot.importance(importance_matrix =  ValidateImportance)
#xgb.plot.tree(model = ValidateImportance )


################ Working with Important Variables ##############################################


ImpVarData <- Customer_Data[,c(12,10,8,21,20,22,23,5,11,15 )]
glimpse(ImpVarData)
glimpse(CustDataFinal)
colnames(ImpVarData)[1]  <- "No.Of.Policies"
colnames(ImpVarData)[6]  <- "Months.Since.Policy.Inception"
colnames(ImpVarData)[10] <- "Customer.Lifetime.Value"
colnames(ImpVarData)[9] <- "No.Of.Open.Complaints"
colnames(ImpVarData)[5] <- "Total.Claims.Amount"
colnames(ImpVarData)[4] <- "Monthly.Since.Last.Claims"
colnames(ImpVarData)[2] <- "Monthly.Premium.Auto"


summary(ImpVarData)


############  MANAGING OUTLIERS  ######################

ImpVarData$Customer.Lifetime.Value <- ManageOutlier(ImpVarData$Customer.Lifetime.Value)

boxplot(ImpVarData$Customer.Lifetime.Value, data = ImpVarData , xlab = "Customer LifeTime Value")

#######################################################


# Data Splitting

library(caTools)

set.seed(100)
split <- sample.split(ImpVarData$Customer.Lifetime.Value,SplitRatio = 0.75)
xgTrain_imp <- subset(ImpVarData,split==TRUE)
xgTest_imp <- subset(ImpVarData,split==FALSE)

glimpse(xgTrain_imp)
glimpse(xgTest_imp)

# Standardizing the Train Dataset

xgTrain_imp_fine <- sparse.model.matrix(Customer.Lifetime.Value~.,data = xgTrain_imp)
head(xgTrain_imp_fine)
xgtrain_label_imp <- xgTrain_imp[,Customer.Lifetime.Value]
head(xgtrain_label_imp)
training_matrix_imp <- xgb.DMatrix(data = as.matrix(xgTrain_imp_fine), label = xgtrain_label_imp)
head(training_matrix_imp)


# Standardizing the Test Dataset

xgTest_imp_fine <- sparse.model.matrix(Customer.Lifetime.Value~.,data = xgTest_imp)
head(xgTest_imp_fine)
xgtest_label_imp <- xgTest_imp[,Customer.Lifetime.Value]
head(xgtest_label_imp)
test_matrix_imp <- xgb.DMatrix(data = as.matrix(xgTest_imp_fine), label = xgtest_label_imp)
head(test_matrix_imp)


#Standardizing ImpVar Dataset for complete CLV Prediction and Model Validation

ImpVarData_set <- sparse.model.matrix(Customer.Lifetime.Value~.,data = ImpVarData)
head(ImpVarData_set)
ImpVarData_label  <- ImpVarData[,Customer.Lifetime.Value]
ImpVarData_Matrix <- xgb.DMatrix(data = as.matrix(ImpVarData_set), label=ImpVarData_label)
head(ImpVarData_Matrix)


# Updating the Parameters

xgb_params_imp <- list("objective" = "reg:linear", "eval_metric" = "rmse" , "verbose" = 2 )
watchlist_imp <- list(train = training_matrix_imp, test = test_matrix_imp)


# Applying eXtreme Gradient Boost Model
xgboostmodel_imp <- xgb.train(params = xgb_params, data = training_matrix_imp, watchlist = watchlist, 
                          nround = 100, eta = 0.1, max.depth = 6 )
summary(xgboostmodel_imp)



# Predicting the Values from Test Matrix


pred_xgboost_imp <- predict(xgboostmodel_imp, newdata = test_matrix_imp)

summary(pred_xgboost_imp)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1516    4028    6046    7845   10539   26235 

# Calculating the R Sqaured values for Test Matrix We get
# R Squared value : ranging between 69.11% to 79.78% for Training and Test data set.
R2 <- 1- (sum((xgTest_imp$Customer.Lifetime.Value-pred_xgboost_imp)^2) / sum((xgTest_imp$Customer.Lifetime.Value - mean(xgTest_imp$Customer.Lifetime.Value))^2))
print(R2 * 100) # R Squared Value : 77.32857

write.csv(pred_xgboost_imp, "CLVPredictedValues.csv")
write.csv(xgTest_imp, "xgTestDataSet.csv")


# Calculating RMSE for Predicted Values
rmse(xgTest_imp$Customer.Lifetime.Value, pred_xgboost_imp)
postResample(xgTest_imp$Customer.Lifetime.Value, pred_xgboost_imp)

#       RMSE     Rsquared          MAE 
# 2647.7554095    0.7747475 1253.8750966 



#Predicting for the entire dataset

pred_xgboostfull <- predict(xgboostmodel_imp, newdata = ImpVarData_Matrix)
write.csv(pred_xgboostfull, "pred_xgboostfull.csv")


# Calculating RMSE
rmse( ImpVarData$Customer.Lifetime.Value, pred_xgboostfull)
postResample(ImpVarData$Customer.Lifetime.Value, pred_xgboostfull)

#          RMSE     Rsquared          MAE 
#  2239.0906101    0.8443315 1041.9238513 


#------------------ Verdict ----------------------- #

############ WITH OUTLIERS ########
#      RMSE     Rsquared          MAE 
# 2647.7554095    0.7747475 1253.8750966
# R Squared Value : 77.48%

########### WITHOUT OUTLIERS #############

#       RMSE     Rsquared          MAE 
# 2239.0906101    0.8443315 1041.9238513 
# R Squared Value : 84.43%

###########################################

# Plotting the values to look at the Residuals.

ggplot(xgTest_imp, aes(xgTest_imp$Customer.Lifetime.Value, pred_xgboost_imp, color = 'red')) +
  geom_point(color = 'red') + geom_abline(color = 'blue') + 
  ggtitle('Plotting Predicted Customer Lifetime Values') +
  xlab('Actual Importance Values') + ylab('Predicted Important values')


###### XG BOOST MODEL IS STABLE AND CAN BE USED FOR BETTER CUSTOMER LIFETIME VALUE PREDICTION #######