#LOGISTIC REGRESION

#importing the data

#titanic_test <- read.csv("titanic_test.csv")
titanic_train <- read.csv("titanic_train.csv")

#overalls of the data
str(titanic_train)
summary(titanic_train)

#removing the Passenger ID and teh name columns from the data set
titanic_train <- titanic_train[ , c(-1,-4)]
ncol(titanic_train)
colnames(titanic_train)
dim(titanic_train)


#data visualization for exploratory data analysis
multi.hist(titanic_train[ ,c(4, 8)]) #age?

#visualizing teh missing data in the data set (training and test)
install.packages('Amelia')
library(Amelia) 
missmap(titanic_train, main = "Missing Values vs. Observed(Train)")

missmap(titanic_test, main = "Missing Values vs. Observed values (Test)")


#imputing the data, taking the mean age for missing values in the age column
# titanic_age <- titanic_train$Age
# ave_age <- mean(titanic_age, na.rm = T)
# rm(titanic_age)
# rm(ave_age)

condition_true <- (is.na(titanic_train[,"Age"]) == TRUE)
#count(condition_true) - incorrect
table(condition_true)
# > table(condition_true)
# condition_true
# FALSE  TRUE 
# 714   177

titanic_train$Age[is.na(titanic_train$Age)]
#> titanic_train$Age[is.na(titanic_train$Age)]
# [1] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
# [28] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
# [55] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
# [82] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
# [109] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
# [136] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
# [163] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA


for(i in condition_true){
  titanic_train$Age[is.na(titanic_train$Age)] <- mean(titanic_train$Age, na.rm = T)
}
summary(titanic_train) 
#previous 177 NA's are now updated with the mean age value #but this has to be improved

titanic_train$Age[is.na(titanic_train$Age)]
#numeric(0)

#viewing the map again
missmap(titanic_train, main = "Missing Values vs. Observed(Train)")

#plotting the survival and non survival points
ggplot() + geom_histogram(data = titanic_train, aes(x= titanic_train$Survived,fill= titanic_train$Sex),
                                                position = 'dodge', 
                                                binwidth = 0.50) +
           ggtitle("Survival and non-survival count")+ xlab("Survival") +ylab("count")
#to view the plot as separate bars for male and female, use position = 'dodge' to unstack
# Most if the male did not survive, female survival rate was high

#let's see if this was related to Pclass
ggplot() + geom_histogram(data = titanic_train, aes(x= titanic_train$Survived,fill=titanic_train$Pclass), 
                                                position = 'dodge',
                                                binwidth = 0.50) +
           ggtitle("Survival and non-survival count")+ xlab("Survival") +ylab("count")
#Analysis:
#Most of the passengers who died were from class 3 and most of the passengers who survived were from class 1


#since the Pclass is not a factor, converting it to a factor using the condition and then will return to plotting
condition_factor = ((is.factor(titanic_train$Pclass)) == FALSE)

for ( j in condition_factor){
    titanic_train$Pclass = as.factor(titanic_train$Pclass)
}

summary(titanic_train)
#output  - can see that the numeric(Sex) is now a factor
#> summary(titanic_train)
# Survived      Pclass      Sex           Age            SibSp           Parch       
# Min.   :0.0000   1:216   female:314   Min.   : 0.42   Min.   :0.000   Min.   :0.0000  
# 1st Qu.:0.0000   2:184   male  :577   1st Qu.:22.00   1st Qu.:0.000   1st Qu.:0.0000  
# Median :0.0000   3:491                Median :29.70   Median :0.000   Median :0.0000  
# Mean   :0.3838                        Mean   :29.70   Mean   :0.523   Mean   :0.3816  
# 3rd Qu.:1.0000                        3rd Qu.:35.00   3rd Qu.:1.000   3rd Qu.:0.0000  
# Max.   :1.0000                        Max.   :80.00   Max.   :8.000   Max.   :6.0000  
# 
# Ticket         Fare                Cabin     Embarked
# 1601    :  7   Min.   :  0.00              :687    :  2   
# 347082  :  7   1st Qu.:  7.91   B96 B98    :  4   C:168   
# CA. 2343:  7   Median : 14.45   C23 C25 C27:  4   Q: 77   
# 3101295 :  6   Mean   : 32.20   G6         :  4   S:644   
# 347088  :  6   3rd Qu.: 31.00   C22 C26    :  3           
# CA 2144 :  6   Max.   :512.33   D          :  3           
# (Other) :852                    (Other)    :186  


#visualizing teh age groups on the ship using ggplot
ggplot() + geom_histogram(data = titanic_train, aes(x= titanic_train$Age), 
                                                fill = "red",
                                                colour = "black")+
           ggtitle("Age groups on the Titanic")+ xlab("Age") +ylab("count")

#number of kids below age 5 is high and 
#largest number of people were in the age of late 20's (could be an effect of imputation with mean(age))

#visualizing the Sibsp to view if it had any impact on survival or non-survival
ggplot() + geom_histogram(data = titanic_train, aes(x= titanic_train$SibSp), 
                          fill = "red",
                          colour="black",
                          bins  = 10)+
           ggtitle("Sibling spouse distribution on Titanic")+ xlab("Siblings/Spouse") +ylab("count")
#Insight:
#Most of teh members had no siblings or spouse on board

#visualizing the Fare to view if it had any impact on survival or non-survival
ggplot() + geom_histogram(data = titanic_train, aes(x= titanic_train$Fare), 
                          fill = "red",
                          colour="black",
                          bins  = 10)+
           ggtitle("Fare distribution on Titanic")+ xlab("Fare") +ylab("count")
#analysis:
#Most of the passengers had low fare tickets,that is below 100 (could relate to the class, check cor()) 



#Visualizing the age based on based pclass in boxplot
ggplot() + geom_boxplot(data = titanic_train, aes(x= titanic_train$Pclass, y=titanic_train$Age,
                                                  fill=titanic_train$Sex))

#Plot for sex wise age in boxplot, most of the class1 male passenger median age is above 35
# max aged male class1 passenger is of age 80
# most of teh class 3 passengers were in teh age group of 20's and 30's

#plotting the data without sex variable
ggplot() + geom_boxplot(data=titanic_train, aes(x= titanic_train$Pclass, y=titanic_train$Age),
                                            fill = "blue")

#dropping teh column, cabin
titanic_train$Cabin <-  NULL
dim(titanic_train)
#OUTPUT
# [1] 891   9

#dummy coding the sex column for the model use
is.factor(titanic_train$Sex)
levels(titanic_train$Sex)
titanic_train$Sex  = factor(titanic_train$Sex,
                       levels = c("female", "male"),
                       labels = c(1, 0))


#dummy coding the Embarked column for the model use
is.factor(titanic_train$Embarked)
levels(titanic_train$Embarked)
#omitting the embarked column with ""
table(titanic_train$Embarked)
table(titanic_train$Embarked == "") 

condition_embarked_blank = ((titanic_train$Embarked == "") == TRUE)
for (a in condition_embarked_blank) {
        titanic_train = na.omit(titanic_train)
}

table(titanic_train$Embarked)

na.omit(titanic_train$Embarked)
titanic_train$Embarked  = factor(titanic_train$Embarked,
                            levels = c("C", "Q", "S"),
                            labels = c(1, 2, 3))
View(titanic_train)
str(titanic_train)
titanic_train$Survived <- factor(titanic_train$Survived)
titanic_train$SibSp <- factor(titanic_train$SibSp)
titanic_train$Parch <- factor(titanic_train$Parch)


titanic_train$Ticket <- NULL

#splitting the train data 
set.seed(101)
split =  sample.split(titanic_train$Survived, SplitRatio = 0.70)
train_model_titanic <-  subset(titanic_train , split == TRUE)
test_model_titanic <-  subset(titanic_train , split != TRUE)

#building the glm model
logi_titanic_model <- glm(Survived ~ .,
                          data= titanic_train,
                          family = binomial)
summary(logi_titanic_model)

logi_titanic_final <- glm(Survived ~ .,
                          data= train_model_titanic,
                          family = binomial)

summary(logi_titanic_final)

# > summary(logi_titanic_model)
# 
# Call:
#   glm(formula = train_model_titanic$Survived ~ ., family = "binomial", 
#       data = train_model_titanic)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.5359  -0.6220  -0.4205   0.6141   2.4560  
# 
# Coefficients:
#                Estimate   Std. Error z value Pr(>|z|)    
# (Intercept)    4.294e+00  5.462e-01   7.861 3.82e-15 ***
#   Pclass2     -9.077e-01  3.333e-01  -2.723  0.00646 ** 
#   Pclass3     -2.106e+00  3.333e-01  -6.317 2.67e-10 ***
#   Sex0        -2.729e+00  2.229e-01 -12.243  < 2e-16 ***
#   Age         -4.030e-02  9.094e-03  -4.432 9.36e-06 ***
#   SibSp       -2.768e-01  1.157e-01  -2.392  0.01677 *  
#   Parch       -5.257e-02  1.346e-01  -0.391  0.69616    
#   Fare        -3.138e-04  2.750e-03  -0.114  0.90914    
#   Embarked2    9.776e-05  4.459e-01   0.000  0.99983    
#   Embarked3   -6.092e-01  2.731e-01  -2.230  0.02572 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 946.06  on 710  degrees of freedom
# Residual deviance: 628.20  on 701  degrees of freedom
# (2 observations deleted due to missingness)
# AIC: 648.2
# 
# Number of Fisher Scoring iterations: 5

#significant variables identified Pclass2, Pclass3, Sex0, Age     

#predicting the survival using the test_model_titanic data
survival_predictor_train <- predict(logi_titanic_model, newdata= test_model_titanic, type = "response")

#creating teh cm by setting teh threshold
train_survival_pred_class= ifelse(survival_predictor_train > 0.5, 1, 0)
misClassified <- mean(train_survival_pred_class != test_model_titanic$Survived)
print(1-misClassified)
# [1] 0.8097015 accuracy
data.frame(train_survival_pred_class)  #
nrow(train_survival_pred_class) 
# > nrow(train_survival_pred_class)
# NULL
cm_train <- table(test_model_titanic$Survived, train_survival_pred_class) 
#    0   1
# 0 143  22
# 1  29  74

acc_train_survivalpred_class = (cm_train [1,1] + cm_train [2,2]) / sum(cm_train)
# > acc_train_survivalpred_class
# [1] 0.8097015

#true positive rate
train_tpr <- cm_train[2,2] / (cm_train[1,2] + cm_train[2,2])
# > train_tpr
# [1] 0.7708333
