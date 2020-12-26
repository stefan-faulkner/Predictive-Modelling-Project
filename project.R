#Predictive Modelling Project
#Student ID: 2514007F
 
#Loading neccesary libraries
library(tidyverse)
library(ggplot2)
library(GGally)
library(ggfortify)
library(gridExtra)
library(corrplot)
library(car)
library(olsrr)



#Loading the housing dataset provided to us
housing <- read_csv(file.choose())
head(housing)

str(housing)   #Looking on our varius data types


colnames(housing)   #Familiarizing with our column names

#Performing Exploratory Data Analysis


plot1<- ggplot(data=housing, aes(y=price,x=elevation)) +geom_point()

plot2<-  ggplot(data=housing, aes(y=price,x=dist_am1)) +geom_point()
plot3<- ggplot(data=housing, aes(y=price,x=dist_am2)) +geom_point()
plot4<- ggplot(data=housing, aes(y=price,x=dist_am3)) +geom_point()


housing$bath<- as.factor(housing$bath)  #Usint the as.factor function to specify bath as a categorical variable
plot5<- ggplot(data=housing, aes(y=price,x=bath)) +geom_point()

plot6<- ggplot(data=housing, aes(y=price,x=sqft)) +geom_point()
plot7<- ggplot(data=housing, aes(y=price,x=precip)) +geom_point()

grid.arrange(plot1,plot2,plot3,plot4, nrow=2) #Looking on our scatterplots we can see we have an 
#outlier in our response variable and our explantatory avribales is nearly constant.


grid.arrange(plot5,plot6,plot7) # Of course we would still see the outlier for our response variable 
#but if we look closely on the bath explantory variabe we can see a slight positive correlation 
#indicating to us that as the number of baths increase there is an increase in price and the other covariates we can see its nearly constant.


housing$parking<- as.factor(housing$parking)  #Usint the as.factor function to specify parking as a categorical variable
ggplot(data=housing, aes(y=price,x=parking)) +geom_boxplot() #we can see that when parking=Covered , it has a slightly higher median value than the other observations


ggpairs(housing)  # The above chart is a nice diagnostic of each variable by itself and their relationship with other.
#We can aslo get an idea what the  sample correlation coefficient  is which is known as r.


new_df<- subset(housing,selec=c(price,elevation,dist_am1,dist_am2,dist_am3,bath,sqft,precip))
correlation<-cor(drug_data)
par(mfrow=c(1, 1))
corrplot(correlation,method="color")
#We can see from this correlation plot that price is positively correlated with dist_amt1,dist_am2,dist_am3, bath and sqft
#Note we see a higher psotive correlation with bath and sqft as it is the closest to 1



#Now let's start just by  building a generic model with all the predictors

model_1<- lm(price~., data=housing) 

ggcoef(model_1,exclude_intercept = TRUE)
summary(model_1)

#We can see that all the variables are not significant i.e p values are less than 0.5
#And we can also see that ta=heir confidence intervals include 0.
#So we will do a model selection  to try and reduce our model .
#The aim is to use the step function which uses the AIC Criterion 
#for our model electionmpare with other model selections.


step<-step(model_1)
print(step)
plot(step)



backwards<-ols_step_backward_p(housing,prem=0.05)
summary(backwards$model)
plot(backward)


forward<-ols_step_forward_p(housing,penter=0.05)
plot(forward)
summary(forward$model)

#Reduced Model -Concluding with the step model based on comparing the amount of predictors  for the other two model selections backward and foward we also examined.

#
#The reason for chosing the Stepwise Regression was that I believe it would
#be very effeective since we had a lot of potential explantory variables, i.e 8 to be exact
#Although note that we had considered other model selections
# We could have chosen a different final model especialy if we had change 
#the signifance value for our p value criterion (If the model selection uses that criterion)

housing.step<- lm(price~bath +sqft,data=housing)
summary(housing.step)
autoplot(model_1,which=1:5)



#Identifing our outlier from our final model chosen
outlierTest(housing.step)

#We can  see it says 348
#This represent the record in our price column in our 
#housing data set with a figure of 12500000
# I have made the decision not to the remove the outlier.






































