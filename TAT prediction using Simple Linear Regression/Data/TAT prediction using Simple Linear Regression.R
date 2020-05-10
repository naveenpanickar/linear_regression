# Simple Linear Regression. Project Turn Around Time (TAT) prediction based on nc issue count to handle in a project

#Check working directory

getwd()

#If needed Change and Set work directory 

#setwd(dir)

# Import libraries

##install.packages("catools")
library(caTools)

##install.packages("dplyr")\
library(dplyr)

##install.packages("ggplot")
library(ggplot2)


#Load the Data

df = read.csv('data/nc_tat.csv')


# let's Preprocess the data first.

#Top rows Preview  
head(df) 
df
 
# Review Data

#Checking Number of Rows  
nrow(df)  
#Checking Number of Columns  
ncol(df) 
#Checking Column Names  
names(df)
#Remove unwanted column from Data Frame
df <- subset( df, select = -X )
head(df)

#Checking Summary(Mean, Mode, Median)  
summary(df)

#Check and handle missing values 

is.na(df)

# for tat

df$tat<- ifelse(is.na(df$tat),mean(df$tat,na.rm = T),df$tat)

# For nc_count

df$nc_count<-ifelse(is.na(df$nc_count),mean(df$nc_count,na.rm = T),df$nc_count)

head(df,10)


#Check linear trend by ploting graph  
plot(df$nc_count)  
plot(df$tat,col="red")  
plot(df$tat,df$nc_count,col="blue")  

#Check outliers by ploting Box Plot Graph  
boxplot(df$nc_count)  
boxplot(df$tat)  
boxplot(df,col=3)

# assign the outlier values into a vector

outliers <- boxplot(df$nc_count, plot=FALSE)$out

# Check the results

print(outliers)


# Removing the outliers
# First you need find in which rows the outliers are

df[which(df$nc_count %in% outliers),]

# Now you can remove the rows containing the outliers, one possible option is:

df <- df[-which(df$nc_count %in% outliers),]

# If you check now with boxplot, you will notice that those pesky outliers are gone

boxplot(df$nc_count)


# Splitting data into training and test sets

set.seed(123)

split<- sample.split(df$tat, SplitRatio = 0.7)

# training_set

training_set<- subset(df, split==T)

training_set

# test set

test_set <- subset(df, split==F)

test_set

# Build a linear regression on training set

model<-lm(tat~nc_count, data=training_set)

# Check the summary of the model

summary(model)

# model attributes
attributes(model)

# fitted.values means that 

fitted<- predict(model, newdata=training_set)

#head(fitted)

training_pred<-data.frame(training_set,fitted)

#head(training_pred)


# predicting new values based on the test set

y_pred<-predict(model, newdata=test_set)

head(y_pred)

#y_pred<-predict(model, newdata = data.frame(nc = c(8, 21, 50)))

#y_pred

test_pred<-data.frame(nc_count=test_set$nc_count,tat=test_set$tat,tat_p=y_pred, Type1="Predicted")

#head(test_pred)


training_fit<-data.frame(training_set,model_fitted=model$fitted.values)

#head(training_fit)

# Create a new data frame, which includes both predicted and observed values



training_model<-training_fit %>% dplyr::mutate(Type1="Observed") %>% bind_rows(test_pred)

#head(training_model)

View(training_model)

# Visualing the result


ggplot() + 
  geom_point(aes(x = training_set$nc_count, 
                 y = training_set$tat), colour = 'red') + 
  geom_line(aes(x = training_set$nc_count,  
                y = predict(model, newdata = training_set)), 
            colour = 'blue') + 
  ggtitle('tat vs nc_count (Training Set') + 
  xlab('nc_count (Training Set)') + 
  ylab('tat') 


# Visualize the test set 

ggplot() + 
  geom_point(aes(x = test_set$nc, 
                 y = test_set$tat), 
             colour = 'red') + 
  geom_line(aes(x = training_set$nc,  
                y = predict(model, newdata = training_set)), 
            colour = 'blue') + 
  ggtitle('tat vs nc_count (Test Set)') + 
  xlab('nc_count (Test Set)') + 
  ylab('tat') 

