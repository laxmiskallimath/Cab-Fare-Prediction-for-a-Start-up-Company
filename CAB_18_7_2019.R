#************************************************************************#
# Cab fare  Prediction   
#************************************************************************#

# Clean the environment -------------------------------------------------
rm(list=ls())

# Set working directory ---------------------------------------------------
setwd("E:/EDWISOR/Project/Cab_Fare_Project")


# Load required Libraries for analysis  ----------------------------------
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced","C50",
      "dummies", "e1071", "Information", "MASS", "rpart", "gbm", "ROSE",
      'sampling', 'DataCombine', 'inTrees',"scales","psych","gplots")

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

# Load the data -----------------------------------------------------------
Train_Cab <- read.csv("train_cab.csv")


# Explore the data  -------------------------------------------------------
# Check class of the data
class(Train_Cab)

#Check the dimensions(no of rows and no of columns)
dim(Train_Cab)

#Check names of dataset(no need of renaming variables)
names(Train_Cab)

#Check top(first) rows of dataset 
head(Train_Cab,8)

#Check bottom(last) rows of dataset 
tail(Train_Cab)

#Check structure of dataset(data structure of each variable)
str(Train_Cab)

#Check summary of dataset 
summary(Train_Cab)

# Variable Identification 
# Target variable - fare_amount 
str(Train_Cab$fare_amount) # fare_amount is a continous variabe 

# Data type conversion
# As observed, we have to change fare_amount from factor to numeric
Train_Cab$fare_amount = as.numeric(as.character(Train_Cab$fare_amount))

summary((Train_Cab$fare_amount))

# we need to convert pickupdatetime as well we did it in feature engineering 

# Missing Value Analysis --------------------------------------------------

# Total number of missing values present in whole datset 
sum(is.na(Train_Cab)) 

# Missing values present in each column in the dataset
apply(Train_Cab,2,function(x){sum(is.na(x))}) 

# Store these missing values present in each variable in to data frame 
missing_val = data.frame(apply(Train_Cab,2,function(x){sum(is.na(x))}))

# Creating new variable Column with values as rownames 
missing_val$Columns = row.names(missing_val)

# Rename column 1 in missing_val dataframe
names(missing_val)[1] =  "Missing_percentage"

# Lets calculate  percentage of missing values
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(Train_Cab))*100

# sort the missing percentage in descending order
missing_val = missing_val[order(-missing_val$Missing_percentage),]

# Delete rownames 
row.names(missing_val) <- NULL

# Re order the columns for clear understanding
missing_val <- missing_val[,c(2,1)]
Train_Cab = na.omit(Train_Cab)

sum(is.na(Train_Cab)) 

# Feature engineering -----------------------------------------------------
# We need to change pickup_datetime from factor to datetime
# But first, let's replace UTC in pickup_datetime variable with ''(space)

Train_Cab$pickup_datetime <- gsub('// UTC','',Train_Cab$pickup_datetime)

# Now convert variable pickup_dattime to date time format by creating
# new variable with name Date 

Train_Cab$date <- as.Date(Train_Cab$pickup_datetime)

# Lets split this new variable Date into year,month,weekday 
# Extract the year
Train_Cab$year <- substr(as.character(Train_Cab$date),1,4)

# Extract the month
Train_Cab$month <-substr(as.character(Train_Cab$date),6,7)

# Extract the weekday 
Train_Cab$day <- weekdays(as.POSIXct(Train_Cab$date),abbreviate = F)

# Extract the date 
Train_Cab$date <- substr(as.character(Train_Cab$date),9,10)

# Extract the time / hour
Train_Cab$hour <- substr(as.factor(Train_Cab$pickup_datetime),12,13)

#Lets delete picupdate time as we converted this variable into day,month,year,hour
Train_Cab$pickup_datetime <- NULL

dim(Train_Cab)

head(Train_Cab)

# Lets check summary again after new feature creation
summary(Train_Cab)

# Lets check for NA after new feature creation 
sum(is.na(Train_Cab))

apply(Train_Cab,2,function(x){sum(is.na(x))})

Train_Cab = na.omit(Train_Cab)

dim(Train_Cab)

# Outlier analysis --------------------------------------------------------
# Boxplots-Distribution and outlier check
numeric_index <- sapply(Train_Cab,is.numeric)# Selecting only numeric 
numeric_index
numeric_data <-Train_Cab[,numeric_index]
cnames <- colnames(numeric_data)
cnames 

# Boxplot for all continous varaibles 
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = cnames[i]), data = Train_Cab)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot for",cnames[i])))
}

gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,gn6,ncol=2)

# We have different methods remove outliers here we used capping method -------
# Capping with Upperfence/Lower Fence Value in case of outliers 
# Q1 <- first quartile(25%)
# Q3 <- third quartile(75%)
# IQR <- Inter quartile range = Q3-Q1
# UL <- Q3 +1.5*IQR Uppler limit/upper fence
# LL <- Q1 - 1.5*IQR Lower limit /lower fence


######  fare_amount ######
# Let use summary function to check min max values and identify outliers 
summary(Train_Cab$fare_amount)

Q1 <- quantile(Train_Cab$fare_amount,0.25)#6
Q3 <- quantile(Train_Cab$fare_amount,0.75)#12.5
UL <- Q3 + (1.5*IQR(Train_Cab$fare_amount)) # 22.25
LL <- Q1 -(1.5*IQR(Train_Cab$fare_amount)) #-3.75

Train_Cab[Train_Cab$fare_amount<LL,"fare_amount"] <- LL
Train_Cab[Train_Cab$fare_amount>UL,"fare_amount"] <- UL

#Lets sort the data to find out any other anamolies like -ve and 0's 
fare_asc <-Train_Cab[order(Train_Cab$fare_amount),] 
fare_desc <- Train_Cab[order(-Train_Cab$fare_amount),]

# We have some negative values and zeros lets treat them as NA and delete 
Train_Cab$fare_amount[Train_Cab$fare_amount <= 0] <- NA
Train_Cab$fare_amount[Train_Cab$fare_amount == 0.01] <- NA
sum(is.na(Train_Cab))
Train_Cab <- na.omit(Train_Cab)

------------------------------------------------------------------------------------
# pickup_longitude
------------------------------------------------------------------------------------
#Originally, Latitudes range from -90 to 90.
#Originally, Longitudes range from -180 to 180.

summary(Train_Cab$pickup_longitude)

Q1 <- quantile(Train_Cab$pickup_longitude,0.25)#-73.99126
Q3 <- quantile(Train_Cab$pickup_longitude,0.75)#-73.96684
UL <- Q3 + (1.5*IQR(Train_Cab$pickup_longitude))# -73.92884
LL <- Q1-(1.5*IQR(Train_Cab$pickup_longitude)) # -74.03013

Train_Cab[Train_Cab$pickup_longitude < LL ,"pickup_longitude"] <- LL 
Train_Cab[Train_Cab$pickup_longitude > UL ,"pickup_longitude"] <- UL 

---------------------------------------------------------------------------------------
# pickup_latitude
---------------------------------------------------------------------------------------
summary(Train_Cab$pickup_latitude)  
  
Q1 <- quantile(Train_Cab$pickup_latitude,0.25)#40.73494
Q3 <- quantile(Train_Cab$pickup_latitude,0.75)#40.76735
UL <- Q3 + (1.5*IQR(Train_Cab$pickup_latitude))#40.81598
LL <- Q1 - (1.5*IQR(Train_Cab$pickup_latitude))#40.81598

Train_Cab[Train_Cab$pickup_latitude < LL,"pickup_latitude"] <- LL
Train_Cab[Train_Cab$pickup_latitude > UL,"pickup_latitude"] <- UL


#### Dropoff longitude #####
summary(Train_Cab$dropoff_longitude)

Q1 <- quantile(Train_Cab$dropoff_longitude,0.25)#-73.99118 
Q3 <- quantile(Train_Cab$dropoff_longitude,0.75)#-73.96365
UL <- Q3 + (1.5*IQR(Train_Cab$dropoff_longitude))#-73.92234 
LL <- Q1 - (1.5*IQR(Train_Cab$dropoff_longitude))#-74.03249 

Train_Cab[Train_Cab$dropoff_longitude < LL,"dropoff_longitude"] <- LL
Train_Cab[Train_Cab$dropoff_longitude > UL,"dropoff_longitude"] <- UL

-----------------------------------------------------------------------------------------
# dropoff_lattitude
-----------------------------------------------------------------------------------------
summary(Train_Cab$dropoff_latitude)

Q1 <- quantile(Train_Cab$dropoff_latitude,0.25)#40.73466
Q3 <- quantile(Train_Cab$dropoff_latitude,0.75)#40.76801
UL <- Q3 + (1.5*IQR(Train_Cab$dropoff_latitude))#40.81599 
LL <- Q1 - (1.5*IQR(Train_Cab$dropoff_latitude))#40.68

Train_Cab[Train_Cab$dropoff_latitude < LL,"dropoff_latitude"] <- LL
Train_Cab[Train_Cab$dropoff_latitude > UL,"dropoff_latitude"] <- UL

# passenger_count
summary(Train_Cab$passenger_count)

Q1 <- quantile(Train_Cab$passenger_count,0.25)
Q3 <- quantile(Train_Cab$passenger_count,0.75)

UL <- round(Q3 + (1.5*IQR(Train_Cab$passenger_count))) # 4
LL <- round(Q1 - (1.5*IQR(Train_Cab$passenger_count))) #0


# practically maximum 6 passenger can travel in a cab 
Train_Cab[Train_Cab$passenger_count < LL,"passenger_count"] <-LL
Train_Cab[Train_Cab$passenger_count > 6,"passenger_count"] <- UL

# Lets delete 0 which are less in number say around 58 
# Practically, we can have minimum of 1 and maximum of 6 passengers

Train_Cab$passenger_count[Train_Cab$passenger_count < 1] <- NA
Train_Cab = na.omit(Train_Cab)

# Lets visualize boxplots again after outlier removal 

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = cnames[i]), data = Train_Cab)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot for",cnames[i])))
}

gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,gn6,ncol=2)

# Lets check dimensions of data after outlier removal 
dim(Train_Cab)

# Now, let's create distance using Haversine Formula 
# Calculates the geodesic distance between two points specified by
# radian latitude/longitude using the Haversine formula 

library(purrr)
library(geosphere)
library(rlist)

get_geo_distance = function(long1, lat1, long2, lat2) {
  loadNamespace("purrr")
  loadNamespace("geosphere")
  longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
  longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
  distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
  distance_m = list.extract(distance_list, position = 1)
  #if (units == "km") {
  distance = distance_m / 1000.0;

  distance
}

# Applying distance formula for train data
for(i in (1:nrow(Train_Cab)))
{
  Train_Cab$distance[i]= get_geo_distance(Train_Cab$pickup_longitude[i],Train_Cab$pickup_latitude[i],Train_Cab$dropoff_longitude[i],Train_Cab$dropoff_latitude[i])
}

# Lets check data after distance variable creation
head(Train_Cab)

# Lets check whether distance variables has any outlier using boxplot 
ggplot(aes_string(y = 'fare_amount', x = "distance"), data = subset(Train_Cab))+
  stat_boxplot(geom = "errorbar", width = 0.25) +
  geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
               outlier.size=1, notch=FALSE) +theme(legend.position="bottom")+
   ggtitle(paste("Box plot of distnce variable with outliers"))

# Lets check summary of distance variable no outliers 
summary(Train_Cab$distance)

# we can notice this variable has values  which are less than 1 around 2978 in No
# we will replace these values with average distance as the numner of 0's are more
# length(Train_Cab$distance[Train_Cab$distance < 1])#2978

Train_Cab$distance[Train_Cab$distance < 1] <- mean(Train_Cab$distance)

# The data left after all preprocessing 
df = Train_Cab
dim(df)
head(Train_Cab)

# Exploratory Analysis with visualizations after data  cleaning --------------------------------

# Lets check distribution of each numeric and categorical variables
names(Train_Cab)

# Univariate Analysis of continous variables 

# Lets save numeric varaibles 
num_var <- c("fare_amount","pickup_longitude" ,"pickup_latitude", "dropoff_longitude",
             "dropoff_latitude","passenger_count","distance")

# Histogram for continuous variables to check  distribution of each variable 

# fare_amount 
ggplot(Train_Cab, aes_string(x = Train_Cab$fare_amount)) + 
  geom_histogram(fill="skyblue", colour = "black") + geom_density() +
  theme_bw() + xlab("Fare amount") + ylab("Frequency")+ggtitle("distribution of fare_amount")
#right skewed 

# pickup_longitude 
ggplot(Train_Cab, aes_string(x = Train_Cab$pickup_longitude)) + 
  geom_histogram(fill="skyblue", colour = "black",bins = 100,boundry =1) + geom_density() +
  theme_bw() + xlab("pickup_longitude") + ylab("Frequency")+ggtitle("distribution of pickup_longitude")
# almost normally distributed 

# pickup_latitude
ggplot(Train_Cab, aes_string(x = Train_Cab$pickup_latitude)) + 
  geom_histogram(fill="skyblue", colour = "black",bins = 100) + geom_density() +
  theme_bw() + xlab("pickup_latitude") + ylab("Frequency")+ggtitle("Frequency of pickup_latitude")
# almost normally distributed

# dropoff_longitude
ggplot(Train_Cab, aes_string(x = Train_Cab$dropoff_longitude)) + 
  geom_histogram(fill="skyblue", colour = "black",bins = 100) + geom_density() +
  theme_bw() + xlab("dropoff_longitude") + ylab("Frequency")+ggtitle("Frequency of dropoff_longitude")
# almost normally distributed

# dropoff_latitude
ggplot(Train_Cab, aes_string(x = Train_Cab$dropoff_latitude)) + 
  geom_histogram(fill="skyblue", colour = "black",bins = 100) + geom_density() +
  theme_bw() + xlab("dropoff_latitude") + ylab("Frequency")+ggtitle("Frequency of dropoff_latitude")
# almost normally distributed

# passenger_count
ggplot(Train_Cab, aes_string(x = Train_Cab$passenger_count)) + 
  geom_histogram(fill="skyblue", colour = "black") + geom_density() +
  theme_bw() + xlab("passenger_count") + ylab("Frequency")+ggtitle("distribution of passenger_count")

# distance 
ggplot(Train_Cab, aes_string(x = Train_Cab$distance)) + 
  geom_histogram(fill="skyblue", colour = "black") + geom_density() +
  theme_bw() + xlab("distance") + ylab("Frequency")+ggtitle(" distribution of distance ")
#right skewed 

# Bivariate Analysis ------------------------------------------------------
# Bar plot for categorical and target variables 

# Visualization between fare_amount and years.
ggplot(data = Train_Cab, aes(x = year,y = fare_amount))+
  geom_bar(stat = "identity",color ="DarkSlateBlue")+
  labs(title = "Fare Amount Vs. year", x = "years", y = "Fare")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme(axis.text.x = element_text( color="black", size=6, angle=45))
# We can see, in year 2013 there were rides which got high fare_amount

# Visualization between fare_amount and months.
ggplot(Train_Cab, aes(x = month,y = fare_amount))+ 
  geom_bar(stat = "identity",color = "DarkSlateBlue")+
  labs(title = "Fare Amount Vs. Month", x = "Month", y = "Fare")+
  theme(axis.text.x = element_text( color="navy blue", size=8))
# We can see month March collects the highest fare_amount


# Visualization between fare_amount and weekday.
ggplot(data = Train_Cab, aes(x = day,y = fare_amount))+
  geom_bar(stat = "identity",color = "DarkSlateBlue")+
  labs(title = "Fare Amount Vs. weekday", x = "Days of the week", y = "Fare")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme(axis.text.x = element_text( color="black", size=6, angle=45))
# We can see that  Thursday to Saturday rides has the highest fare_amount 

# Visualization between fare_amount and time.
ggplot(data = Train_Cab, aes(x = hour, y = fare_amount))+
  geom_bar(stat = "identity",color = "DarkSlateBlue")+
  labs(title = "Fare Amount Vs.hour", x = "hour", y = "Fare")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme(axis.text.x = element_text( color="black", size=6, angle=45))
# Rides taken during 6 pm to 10 pm gives highest fare_amount
# Lets plot scatter plot for target and continous variables 

# Visualization between fare_amount and pickup_longitude.
ggplot(Train_Cab,aes(pickup_longitude,fare_amount)) + 
  geom_point(alpha=0.5,color="DarkSlateBlue") +
  labs(title = "Scatter Plot b/w pickup_longitude and fare", x = "pickup_longitude", y = "Fare Amount")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()

ggplot(Train_Cab,aes(pickup_latitude,fare_amount)) + 
  geom_point(alpha=0.5,color="DarkSlateBlue") +
  labs(title = "Scatter Plot b/w pickup_latitude and fare", x = "pickup_latitude", y = "Fare Amount")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()

ggplot(Train_Cab,aes(dropoff_longitude,fare_amount)) + 
  geom_point(alpha=0.5,color="DarkSlateBlue") +
  labs(title = "Scatter Plot b/w dropoff_longitude and fare", x = "dropoff_longitude", y = "Fare Amount")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()

ggplot(Train_Cab,aes(dropoff_latitude,fare_amount)) + 
  geom_point(alpha=0.5,color="DarkSlateBlue") +
  labs(title = "Scatter Plot b/w dropoff_latitude and fare", x = "dropoff_latitude", y = "Fare Amount")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()

ggplot(Train_Cab,aes(passenger_count,fare_amount)) + 
  geom_point(alpha=0.5,color="DarkSlateBlue") +
  labs(title = "Scatter Plot b/w passengercount and fare", x = "passenger_count", y = "Fare Amount")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()
# single passenger are frequent travellers 

ggplot(Train_Cab,aes(distance,fare_amount)) + 
  geom_point(alpha=0.5,color="DarkSlateBlue") +
  labs(title = "Scatter Plot b/w distance and fare", x = "Distance", y = "Fare Amount")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()
# We can say as distance increases fare amount also increases 


# Feature selection ------------------------------------------------------
numeric_index <- sapply(Train_Cab,is.numeric)# Selecting only numeric 
numeric_index
numeric_data <-Train_Cab[,numeric_index]
cnames <- colnames(numeric_data)
cnames

# Correlation Plot for to select significant continous variables 
cor(Train_Cab[,numeric_index])
corrgram(Train_Cab[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

# We can say distance variable is moderately correlated with fare amount 
# rest of the variables also correlated positively and negative but we can 
# say them as weakly correlated we can use them in model 

# Anova Test is performed between cat_var (categorical independent variables) & fare_amount (continuous target variable) 
str(Train_Cab)
names(Train_Cab)
cat_var <-c("date","year","month","day","hour")

# aov(Train_Cab$fare_amount~Train_Cab$year)
# for all categorical variables
for(i in cat_var){
  print(i)
  Anova_test_result = summary(aov(formula = fare_amount~Train_Cab[,i],Train_Cab))
  print(Anova_test_result)
}
names(Train_Cab)

# From the anova result, we can observe Date and day 
# has p value > 0.05, so delete this variable not consider in model.
# lets delete date and day variable
Train_Cab$day <- NULL
Train_Cab$date <- NULL

head(Train_Cab)

# Feature Scaling ---------------------------------------------------------

# In our dataset fare amount and distance are the two continous
# variables whose disribution is slightly skewed

# Checking distance variable distribution using histogram
ggplot(Train_Cab, aes_string(x = Train_Cab$distance)) + 
  geom_histogram(fill="skyblue", colour = "black") + geom_density() +
  theme_bw() + xlab("distance") + ylab("Frequency")+ggtitle(" distribution of distance ")
# this variable is right skewed 

# Lets take log transformation to remove skewness
# Lets define function for log transformation of variables
signedlog10 = function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

# Applying log function to distance variable
Train_Cab$distance = signedlog10(Train_Cab$distance)

# Checking distance distribution after applying function
ggplot(Train_Cab, aes_string(x = Train_Cab$distance)) + 
  geom_histogram(fill="skyblue", colour = "black") + geom_density() +
  theme_bw() + xlab("distance") + ylab("Frequency")+ggtitle(" distribution of distance after log transformation")

head(Train_Cab)


# Model development -------------------------------------------------------
# Let's clean R Environment, as it uses RAM which is limited
library(DataCombine)
rmExcept("Train_Cab")

# Split the data set into train and test 
set.seed(1234)
train.index = createDataPartition(Train_Cab$fare_amount, p = .80, list = FALSE)
train = Train_Cab[train.index,]
test  = Train_Cab[-train.index,]


# Linear Regression model -------------------------------------------------

# fit linear regression model
# we will use the lm() function in the stats package
linear_Reg_model <- lm(fare_amount ~.,data = train)
summary(linear_Reg_model)

# Lets check the assumptins of ols regression 
#Error should follow normal distribution and no hetroscadacity
# assumptions are checked usig residual plot and normal qq plot 
# Change the panel layout to 2 x 2
par(mfrow = c(2, 2))
plot(linear_Reg_model)

# No multicolinearity between Independent variables 
# library(usdm)
# vif(Train_Cab[,-1])

#vif check model 
library(car)
vif(linear_Reg_model) # all vif values are less than 5 there is no multicolinearity among  the IV

# No autocorrelation between errors
dwt(linear_Reg_model) # dwt < 2 so there is no autocorrelation in error 

# predicting for test data
predictions_LR <- predict(linear_Reg_model,test[,-1])

# For tes data 
print(postResample(pred = predictions_LR, obs =test$fare_amount))

# RMSE  Rsquared       MAE 
# 3.6961954 0.5462518 2.7383352

MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}

MAPE(test[,1],predictions_LR) #0.37

# We can say our mode is around 63% accurate 

# Desicision Tree ---------------------------------------------------------
library(rpart)  			        # Popular decision tree algorithm
library(rattle)					      # Fancy tree plot
library(rpart.plot)				    # Enhanced tree plots
library(RColorBrewer)				  # Color selection for fancy tree plot


fit <- rpart(fare_amount ~.,data = train,method = 'anova')

fit

rpart.plot(fit,type=4,digits=3,fallen.leaves=T,tweak = 1.5)

# predict for test cases
 
predictions_DTR <- predict(fit,test[,-1])

print(postResample(pred = predictions_DTR , obs =test$fare_amount))

# RMSE  Rsquared       MAE 
# 3.1394298 0.6724915 2.1658239 

MAPE(test[,1],predictions_DTR) #0.24

# We can say our decision tree model around 76% accurate 

# Random Forest  ----------------------------------------------------------

RF_model = randomForest(fare_amount ~.,data = train, importance = TRUE, ntree = 200)

#Predict test data using random forest model

RF_Predictions = predict(RF_model, test[,-1])

print(postResample(pred = RF_Predictions , obs =test$fare_amount))

# RMSE  Rsquared       MAE 
# 2.5961599 0.7764808 1.7104151 

MAPE(test[,1],RF_Predictions)#0.2047504

# Our random forest model is 78% accurate 

# Results -----------------------------------------------------------------

LinearRegression  = c(RMSE =3.96 ,Rsquared =0.54 ,MAE =2.73 ,MAPE =0.37 )

DecisionTree = c(RMSE =3.13  ,Rsqared =0.67 ,MAE =2.16 ,MAPE =0.24 )

RandomForest = c(RMSE =2.59  ,Rsquared =0.77 ,MAE =1.71 ,MAPE = 0.20)

Final_Results_in_R <- cbind(LinearRegression,DecisionTree,RandomForest)

Final_Results_in_R

t(Final_Results_in_R)

# From Results we can say Random forest model is 
# performing well and having optimum values compared to rest other 
# models on train dataset so we are going freeze this data to RF model



# Model evaluation  -------------------------------------------------------

# Load the test dataset and all required data preprocessing as we have done for Train_Cab data

Test_Cab <- read.csv("test.csv")

# Exploring  the  test data 

# Check class of the data
class(Test_Cab)

#Check the dimensions(no of rows and no of columns)
dim(Test_Cab)

#Check names of dataset(no need of renaming variables)
names(Test_Cab)

#Check top(first) rows of dataset 
head(Test_Cab)

#Check bottom(last) rows of dataset 
tail(Test_Cab)

#Check structure of dataset(data structure of each variable)
str(Test_Cab)

#Check summary ofs dataset 
summary(Test_Cab)

# we are going to change pickup_datetime from factor to datetime
# But first, let's replace UTC in pickup_datetime variable with ''(space)

Test_Cab$pickup_datetime <- gsub('// UTC','',Test_Cab$pickup_datetime)

# Now convert variable pickup_dattime to date time format by creating
# new variable with name Date 
Test_Cab$date <- as.Date(Test_Cab$pickup_datetime)

# Lets split this new variable Date into year,month,weekday 
# Extract the year
Test_Cab$year <- substr(as.character(Test_Cab$date),1,4)

# Extract the month
Test_Cab$month <-substr(as.character(Test_Cab$date),6,7)

# Extract the weekday 
Test_Cab$day <- weekdays(as.POSIXct(Test_Cab$date),abbreviate = F)

# Extract the date 
Test_Cab$date <- substr(as.character(Test_Cab$date),9,10)

# Extract the time 
Test_Cab$hour <- substr(as.factor(Test_Cab$pickup_datetime),12,13)

# Let us delete pickup_dataetime as we extracted required substitutes of date 
Test_Cab$pickup_datetime <- NULL


# Data after datatype conversion 
Data1 <- Test_Cab # will keep a copy of original data 
head(Test_Cab)


# Missing Value Analysis --------------------------------------------------
sum(is.na(Test_Cab)) # no missing values 

# Outlier analysis --------------------------------------------------------
summary(Test_Cab) # no outliers

# Lets calculate distance for test data using function which we already created for train data
# # #Applying distance formula for train data
get_geo_distance = function(long1, lat1, long2, lat2) {
  loadNamespace("purrr")
  loadNamespace("geosphere")
  longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
  longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
  distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
  distance_m = list.extract(distance_list, position = 1)
  #if (units == "km") {
  distance = distance_m / 1000.0;
  
  distance
}


for(i in (1:nrow(Test_Cab)))
{
  Test_Cab$distance[i]= get_geo_distance(Test_Cab$pickup_longitude[i],Test_Cab$pickup_latitude[i],Test_Cab$dropoff_longitude[i],Test_Cab$dropoff_latitude[i])
}

head( Test_Cab)

#write.csv(Test_Cab,"distance_test.csv")

# Lets check whether distance variables has any outlie
summary(Test_Cab$distance)

# distance can not be less than 1 so we replace with average distance
Test_Cab$distance[Test_Cab$distance < 1] = mean(Test_Cab$distance)

# During model development, we deleted few varaibles based on anova test and correlation analysis 
# The variables in the test case should exactly match with the variables in the trained model 
Test_Cab$date <- NULL
Test_Cab$day <- NULL

# Feature scaling for test data -------------------------------------------

# Checking distance variable distribution using histogram
ggplot(Test_Cab, aes_string(x = Test_Cab$distance)) + 
  geom_histogram(fill="skyblue", colour = "black") + geom_density() +
  theme_bw() + xlab("distance") + ylab("Frequency")+ggtitle(" distribution of distance ")

# We are going to define function using log
signedlog10 = function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

# Applying log function to distance variable
Test_Cab$distance = signedlog10(Test_Cab$distance)

# Checking distance distribution after applying function 
ggplot(Test_Cab, aes_string(x = Test_Cab$distance)) + 
  geom_histogram(fill="skyblue", colour = "black") + geom_density() +
  theme_bw() + xlab("distance") + ylab("Frequency")+ggtitle(" distribution of distance ")

# Let's look at summary again
summary(Test_Cab$distance)


# Model evaluation using this test data -----------------------------------
# Code for development of model
RFModel = randomForest(fare_amount~., train, ntree = 200, method = "anova")

# Predicting model on Test_Cab data

RFTest_Cab = predict(RFModel, Test_Cab)

# Adding our obtained predictions as Predicted Fare Amount variable to test_cab dataset

Test_Cab$Predicted_fare_amount = RFTest_Cab

# Here's a glance of predicted values

head(Test_Cab)

summary(Test_Cab$Predicted_fare_amount)

summary(Train_Cab$fare_amount)

# Finally, we designed a model, which predicts the cab fare.

# Exporting the output to hard disk for further use
write.csv(Test_Cab, "E:/EDWISOR/Project/Cab_Fare_Project/output.csv",
          row.names = FALSE)

getwd()

??rpart

