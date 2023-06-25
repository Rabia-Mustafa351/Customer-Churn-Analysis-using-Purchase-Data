
# 20i-1853
#Rabia Mustafa
#Installing and loading the package

install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
#####################3

library(readr)
df <- read_csv("D:/Fourth Semester/ADV STATS/Detailed_Dataset.csv")
View(df)

#----------------------------------------------------

df$Customer_wealth<-''
df[df$Total_Revenue <= 500,]$Customer_wealth<-'Poor Customer'
df[(df$Total_Revenue > 500) & (df$Total_Revenue <= 1000),]$Customer_wealth<-'Normal Customer'
df[df$Total_Revenue > 1000,]$Customer_wealth<-'Rich Customer'


df$Customer_Visits<-''
df[df$Total_Visit_Days <= 6,]$Customer_Visits<-'occasional Customer'
df[df$Total_Visit_Days > 6,]$Customer_Visits<-'Frequent Customer'

#-------------------1--------------------
#Does the purchase amount of the customers increase over the period of time?

#Ho: difference between means is zero
#Ha: difference between means is greater than zero
diff <- df$W1totalSales-df$W5totalSales
hist(diff,breaks=500, xlim=c(-1000,1000)) #data is somewhat normally distributed
t.test(df$W1totalSales, df$W5totalSales,alternative = 'greater')
#Fail to reject Null hypothesis
#No purchase amount of customers do not increase over the period of time

  
#-------------------2--------------------
#We believe rich people are prone to churn as compared to poor customers.

revenue_quantile <- quantile(df$Total_Revenue)

df$Status <- ifelse(df$Total_Revenue >= revenue_quantile[4], "Rich",
                    ifelse(df$Total_Revenue >= revenue_quantile[2], "Moderate", "Poor"))


#Ho: Rich and poor people have same churn rate
#Ha: Rich are more prone to churn than poor people

#Since we have categorical variables at hand we shall use Chi-Square test
st = df$Status[df$Label=='Churned']
ta = table(st)
obs <- c(ta[2],ta[3])
print(obs)
exp <- c(1/2,1/2)
print(exp)
chisq.test(obs,p=exp)

#Since p-value is less than 0.05, there is a significant difference
#between observed and expected values
#Hence, rich people are prone to churn compared to poor people

#-------------------3--------------------
#We believe that the churned customers visit the stores occasionally.
#Since we have five weeks, we'll say occasionally means less 6 visits 
#Ho: churn customers have 5 or more visits
#Ha: churn customers have less than 5 visits

new <- df$TotalVisitDaysInHistory[df$Label=='Churned']
t.test(new, mu = 5,alternative='less')

#Hence we reject our null hypothesis. The churned customers indeed visit the
#store occasionally.

#------------------4-----------------------
#Overall spending of churned customers is significantly lesser than the non-churned customers

#new_diff <- df$Total_Revenue[df$label=='Churned'] - df$Total_Revenue[df$label=='Not Churned'] 
#hist(new_diff)
Churned_customer_spending=df[df$Max_Purchase_In_A_Day & df$Label=='Churned',]
not_churned_spending=df[df$Max_Purchase_In_A_Day & df$Label=='Not Churned',]
summary(Churned_customer_spending$Max_Purchase_In_A_Day)
summary(not_churned_spending$Max_Purchase_In_A_Day)

spending<-c(sum(df[df$Label == 'Churn',]$Total_Revenue),sum(df[df$Label == 'Not Churned',]$Total_Revenue))
label<-unique(df$Label)

barplot(spending,names.arg=label,xlab="Label",ylab="Total Revenue",col=c("green","purple"),
        main="Revenue chart")

#Ho: no difference between spending of churned and non-churned customers
#Ha: spending of churned customers is significantly less
t.test(df$Total_Revenue[df$Label=='Churned'], df$Total_Revenue[df$Label=='Not Churned'], alternative = 'less')

#As p < 2.2e-16, we reject null hypothesis. 
#The spending of churned customers is significantly lesser than non-churned customers


#------------------5-----------------------

#-----------------------------------------

# Use linear model to predict churned customers by using features of your liking from the data set. 
#Report the accuracy of your model.

#We will split the dataset into Train and Test dataset

trn<- caret::createDataPartition(df$Label,p=0.7,list=FALSE)
set.seed(2017)
train<- df[trn,]
test<- df[-trn,]

#Now, we will try to fit the model within the glm() for logistic regression
L_Model <- glm(as.factor(Label) ~ .,family=binomial(link="logit"),data=train)
# model summary
print(summary(L_Model))

#Now we will evaluate the predictive ability  of the model
test$Label <- as.character(test$Label)
test$Label[test$Label=="Not Churned"] <- "0"
test$Label[test$Label=="Churned"] <- "1"
FitResult <- predict(L_Model,newdata=test,type='response')
FitResult <- ifelse(FitResult > 0.5,1,0)
MisClassificationError <- mean(FitResult != test$Label)
print(paste('Accuracy of Logistic Regression',1-MisClassificationError))
#Lets make a confusion matrix for the logistic regression performed above
print("Logistic Regression Confusion Matrix");
table(test$Label, FitResult > 0.5)


