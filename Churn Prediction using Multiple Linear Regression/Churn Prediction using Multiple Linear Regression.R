#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("broom")
#install.packages("ggpubr")

library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

getwd()

## Step 1: Load the data into R

churn.data <- read.csv("Data/churn.data.csv")
##View(churn.data)

summary(churn.data)

##Step 2: assumptions for linear regression.

##Check Correlation using the R function cor():

cor(churn.data$tenure_ratio, churn.data$call_ratio)

##Normality Test - To check whether the dependent variable follows a normal distribution,

hist(churn.data$churn_ratio)

##Check Linearity

plot(churn_ratio ~ tenure_ratio, data=churn.data)

plot(churn_ratio ~ call_ratio, data=churn.data)

##Step 3: Implement Multiple Linear Regression

churn_ratio.lm<-lm(churn_ratio ~ tenure_ratio + call_ratio, data = churn.data)

summary(churn_ratio.lm)

##Step 4: Check for homoscedasticity

par(mfrow=c(2,2))
plot(churn_ratio.lm)
par(mfrow=c(1,1))

##Step 5: Visualize the results with a graph

#Create a new dataframe with the information needed to plot the model

plotting.data<-expand.grid(
  tenure_ratio = seq(min(churn.data$tenure_ratio), max(churn.data$tenure_ratio), length.out=30),
  call_ratio=c(min(churn.data$call_ratio), mean(churn.data$call_ratio), max(churn.data$call_ratio)))

#Predict the values of heart disease based on your linear model

plotting.data$predicted.y <- predict.lm(churn_ratio.lm, newdata=plotting.data)

#Optional - Round the call_ratio numbers to two decimals; Will make legend easier to read

plotting.data$call_ratio<- round(plotting.data$call_ratio, digits = 2)

#Change the 'call_ratio' variable into a factor
#This allows us to plot the interaction between tenure_ratio and churn_ratio at each of the three levels of call_ratio we chose.

plotting.data$call_ratio <- as.factor(plotting.data$call_ratio)

plotting.data$call_ratio 

#Plot the original data

tenure.plot <- ggplot(churn.data, aes(x=tenure_ratio, y=churn_ratio)) +
  geom_point()

tenure.plot

#Add the regression lines

tenure.plot <- tenure.plot +
  geom_line(data=plotting.data, aes(x=tenure_ratio, y=predicted.y, color=call_ratio), size=1.25)

tenure.plot

#Make the graph ready for publication

tenure.plot <-
  tenure.plot +
  theme_bw() +
  labs(title = "Rates of churn_ratio (% of population) \n as a function of tenure_ratio and call_ratio",
       x = "tenure_ratio (% of population)",
       y = "churn_ratio (% of population)",
       color = "Call_ratio \n (% of population)")

tenure.plot

#Add the equation for the regression line

tenure.plot + annotate(geom="text", x=30, y=1.75, label=" = 15 + (-0.2*tenure_ratio) + (0.178*call_ratio)")


##Step 6: Report your results

#In our survey of 500 towns, we found significant relationships between the tenure_ratio and the churn_ratio and the call_ratio and churn_ratio (p < 0 and p<0.001, respectively).
#Specifically we found a 0.2% decrease (± 0.0014) in the churn_ratio for every 1% increase in tenure_ratio, and a 0.178% increase (± 0.0035) in the churn_ratio for every 1% increase in call_ratio.