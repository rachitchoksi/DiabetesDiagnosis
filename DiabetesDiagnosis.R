#Anjani Nalla
#Ajitesh Janaswamy
#Rachit Choksi
#Akshay Kadari
#Dinesh Kota

#Imputation of missing values
#Pima Indians Dataset - Insulin has many values with 0
#so does SkinThickness
#assume 0 means missing values
#
#assuming random
#using mice to handle the missing values
#
#import dataset
library(mice)

data <-read.csv(file="diabetes.csv",stringsAsFactors=FALSE,header=TRUE)
#data <-read.csv(file="C:\\Users\\Anjani Reddy\\Google Drive\\KDD\\KDD_Project\\diabetes.csv",stringsAsFactors=FALSE,header=TRUE)

#replace missing values in cols 4 and 5 with NA
data[, 4:5][data[, 4:5] == 0] <- NA


View(data)

#check for % of missing data using a simple function
pMiss <- function(x){sum(is.na(x))/length(x)*100}

#shows columns and percent missing
apply(data,2,pMiss)

#row wise
apply(data,1,pMiss)




#skinthickness is 29.6% and insulin is 48.7%
md.pattern(data)

library(VIM)
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#In a box plot red shows distribution of pregnancies with insulin missing
marginplot(data[c(1,5)])
#In this box plot red shows distribution of pregnancies with skin thickness missing

marginplot(data[c(1,4)])

#In this box plot red shows distribution of Insulin with skin thickness missing
marginplot(data[c(4,5)])

#m is number of multiple imputations default is 5, maxit is number of iterations
#with default equal to 5, seed is for offsetting the random number generator
tempData <- mice(data,m=5,maxit=5,meth='norm.predict',seed=500)
summary(tempData)

#It will check the imputed data
tempData$imp$Insulin
tempData$imp$SkinThickness

#now combining data back together
diabetes <- complete(tempData,1)
View(diabetes)

#frequency of a value in each column
ldply(diabetes, function(c)
  sum(c=="0"))

# Recreate the missing value table
diabetes[, 2][diabetes[, 2] == 0] <- NA #Glucose
diabetes[, 3][diabetes[, 3] == 0] <- NA #BP
diabetes[, 6][diabetes[, 6] == 0] <- NA #BMI

# Replace missing values in Glucose with mean
diabetes[, 2][is.na(diabetes[, 2])] <-
  mean(na.omit(diabetes$Glucose))


# Replace missing values in BP with mode
BP_mode_table <- table(diabetes$BloodPressure)
BP_mode <- names(BP_mode_table)[BP_mode_table ==
                                  max(BP_mode_table)]

diabetes[, 3][is.na(diabetes[, 3])] <- BP_mode


#replace each of the missing BMI values with random BMI values
diabetes$BMI[is.na(diabetes$BMI)] <- sample(diabetes$BMI, size=sum(is.na(diabetes$BMI)), replace=F)


library(lattice)
#magenta is imputed
#blue is observed
densityplot(tempData)

#stripplot shows one dimensional scatter plot for individual data points
stripplot(tempData, pch = 20, cex = 1.2)


#Outliers detection section
library(plyr)



count(boxplot.stats(diabetes$BloodPressure)$out)
#Total number of Outliers for BloodPressure :14

count(boxplot.stats(diabetes$Pregnancies)$out)
#Total number of Outliers for Pregnancies 4

count(boxplot.stats(diabetes$Glucose)$out)
#Total number of Outliers for Glucose: 0

count(boxplot.stats(diabetes$SkinThickness)$out)
#Total number of Outliers for SkinThickness 4

count(boxplot.stats(diabetes$Insulin)$out)
#Total number of Outliers for Insulin 24

count(boxplot.stats(diabetes$BMI)$out)
#Total number of Outliers for BMI 8

count(boxplot.stats(diabetes$DiabetesPedigreeFunction)$out)
#Total number of Outliers for DiabetesPedigreeFunction 28

count(boxplot.stats(diabetes$Age)$out)
#Total number of Outliers for Age 9


#univariate analysis


#preliminary analysis
str(diabetes)
diabetes$Outcome <- as.factor(diabetes$Outcome)
str(diabetes)
summary(diabetes$Outcome)
summary(diabetes$Pregnancies) #Dividing based on Median value 
PregnanciesLessThan3 <- subset(diabetes, Pregnancies <= 3)
PregnanciesGreaterThan6 <- subset(diabetes, Pregnancies > 6)
str(PregnanciesLessThan3)
str(PregnanciesGreaterThan6)
by(diabetes$Outcome,diabetes$Pregnancies,summary)
diabetes$BloodPressure<-as.numeric(diabetes$BloodPressure)
library(ggplot2)
qplot(data= diabetes, x= Pregnancies)
qplot(data=PregnanciesLessThan3, x= Outcome)
qplot(data=PregnanciesGreaterThan6, x = Outcome)  
#  women are more likely to get diabetes with no of pregnancies > 6


summary(diabetes$Glucose)
qplot(data = diabetes, x = Glucose) #Has outlier values (below 50) - 6 values
GlucoseGreaterThan50 <- subset(diabetes, Glucose > 50)
str(GlucoseGreaterThan50)
qplot(data = GlucoseGreaterThan50, x = Glucose)        
GlucoseBetween75and150 <- subset(diabetes, Glucose >75 & Glucose<=150)
str(GlucoseBetween75and150) # around 600 members in this range
summary(GlucoseBetween75and150$Outcome)
prop.table(table(GlucoseBetween75and150$Outcome))*100
GlucoseGreaterThan150 <- subset(diabetes,  Glucose>150)
summary(GlucoseGreaterThan150$Outcome)
prop.table(table(GlucoseGreaterThan150$Outcome))*100 
#women with glucose levels > 150 have 75% chance of getting diabetes


summary(diabetes$BloodPressure)
qplot(data = diabetes, x = BloodPressure)  
boxplot(diabetes$BloodPressure) #Has outliers below 40 and above 110
BPbetween40To100 <- subset(diabetes, BloodPressure > 40 & BloodPressure <= 100)
qplot(data = BPbetween40To100, x = BloodPressure) 
summary(BPbetween40To100$Outcome)
prop.table(table(BPbetween40To100$Outcome))*100
BPgreaterThan80 <- subset(diabetes, BloodPressure > 80)
qplot(data = BPgreaterThan80 , x = BloodPressure) 
summary(BPgreaterThan80$Outcome)
prop.table(table(BPgreaterThan80$Outcome))*100
#Women with BP > 80 are 47% more likely to get diabetes
# MOre exploration has to be done

summary(diabetes$SkinThickness)
qplot(data = diabetes, x = SkinThickness)  
SkinThicknessLessThan25 <- subset(diabetes, SkinThickness <= 25)
summary(SkinThicknessLessThan25$Outcome)
prop.table(table(SkinThicknessLessThan25$Outcome))*100
SkinThicknessLessThan50 <- subset(diabetes, SkinThickness >= 25 & SkinThickness<=50 )
summary(SkinThicknessLessThan50$Outcome)
prop.table(table(SkinThicknessLessThan50$Outcome))*100
# Women with skinthickness greater than 25, a women is 43.5% likely to get diabetic. 



qplot(data= diabetes, x= Age)
AgeLessthan30 <- subset(diabetes, Age <= 30 &(Glucose >=100 & Glucose <=150))
summary(AgeLessthan30$Outcome)
prop.table(table(AgeLessthan30$Outcome)) *100
AgeGreaterthan30 <- subset(diabetes, Age > 30 &(Glucose >=100 & Glucose <=150))
summary(AgeGreaterthan30$Outcome)
prop.table(table(AgeGreaterthan30$Outcome)) *100
#WOMEN WITH AGE GREATER THAN 30 YRS AND ABOVE CONDITIONS HAS APPROXIMATELY 50% CHANCE TO GET DIABETES

by(GlucoseGreaterThan150$Outcome,GlucoseGreaterThan150$Pregnancies, summary)
diabetesWith1 <- subset(diabetes, Outcome = 1)
qplot(data = diabetesWith1, x= Pregnancies, y= BloodPressure)

numericData <- subset(diabetes, select = -Outcome)
numericData <- subset(numericData, select = -BloodPressure)
str(numericData)
cor(numericData) #None of them are correlated variables 

ggplot(diabetes, aes(x= Pregnancies, y=Glucose, color=Outcome)) + geom_point()
# 1. If glucose levels is greater than 150 then the number of pregnancies wont matter. You are always prone to diabetes.
# 2. Even though a women has greater 5 pregnancies, she can still not be diabetic if her glucose levels is less than 100

ggplot(diabetes, aes(x= BloodPressure, y= Glucose , color= Outcome)) + geom_point()
# It is evident that for high BP and High glucose, chances of getting diabtetes is also high
ggplot(diabetes, aes(x= BloodPressure, y= Glucose , color= Outcome)) + geom_point()+
  scale_x_continuous(limits = c(40, 100))+
  scale_y_continuous(limits = c(100, 150))

#This portion seems to be equally scattered. Analysing further,

GlucoseBetween100and150 <- subset(diabetes, Glucose >=100 & Glucose <=150)
GlucoseBP <- subset(diabetes, (Glucose >=100 & Glucose <=150) & (BloodPressure > 75 & BloodPressure <= 100))
summary(GlucoseBP$Outcome)
prop.table(table(GlucoseBP$Outcome))*100
# 1.In the above range a women is 40% more likely to get diabetes, which is comparatively high even with medium levels of glucose


ggplot(diabetes, aes(x=factor(Pregnancies) , y= Glucose)) + geom_boxplot(aes(fill = factor(Outcome)))
# For any number of pregnancies, Higher Glucose indicates higher chances of diabetes


ggplot(diabetes, aes(x=factor(Pregnancies) , y= BMI)) + geom_boxplot(aes(fill = factor(Outcome)))
# For any number of pregnancies, Higher BMI indicates higher chances of diabetes

ggplot(diabetes, aes(x=diabetes$Glucose , y= diabetes$Insulin)) + geom_point()

ggplot(diabetes, aes(x=diabetes$BMI , y= diabetes$Insulin)) + geom_point()

AgeBin<-cut(diabetes$Age, c(0,20,40,60,80,100))
ggplot(diabetes, aes(x=AgeBin , y= BloodPressure)) + geom_boxplot(aes(fill = factor(Outcome)))
#Women of age group 60-80 and high BP are highly prone to diabetes


#-----------------------------------------  Data preparation Phase -------------------------------------------------------------------

#Z-score standardization
diabetes$Glucose <- (diabetes$Glucose - mean(diabetes$Glucose))/sd(diabetes$Glucose)
diabetes$BloodPressure <- (diabetes$BloodPressure - mean(diabetes$BloodPressure))/sd(diabetes$BloodPressure)
diabetes$SkinThickness <- (diabetes$SkinThickness - mean(diabetes$SkinThickness))/sd(diabetes$SkinThickness)
diabetes$Insulin <- (diabetes$Insulin - mean(diabetes$Insulin))/sd(diabetes$Insulin)
diabetes$BMI <- (diabetes$BMI - mean(diabetes$BMI))/sd(diabetes$BMI)
diabetes$DiabetesPedigreeFunction <- (diabetes$DiabetesPedigreeFunction - mean(diabetes$DiabetesPedigreeFunction))/sd(diabetes$DiabetesPedigreeFunction)

##rounding off the attributes to 2 digits
diabetes$Glucose
str(diabetes)
diabetes[,2:7] <- as.data.frame(sapply(diabetes[,2:7], as.numeric)) #<- sapply is here
str(diabetes)


#Outlier detection after standardization
Glucose_outliers <- subset(diabetes, diabetes$Glucose > 3 & diabetes$Glucose < -3)
nrow(Glucose_outliers)
#Number of Outliers after standardization : 0
BloodPressure_outliers <- subset(diabetes, diabetes$BloodPressure > 3 & diabetes$BloodPressure < -3)
nrow(BloodPressure_outliers)
#Number of Outliers after standardization : 0
SkinThickness_outliers <- subset(diabetes, diabetes$SkinThickness > 3 & diabetes$SkinThickness < -3)
nrow(SkinThickness_outliers)
#Number of Outliers after standardization : 0
Insulin_outliers <- subset(diabetes, diabetes$Insulin > 3 & diabetes$GlInsulinucose < -3)
nrow(Insulin_outliers)
#Number of Outliers after standardization : 0
BMI_outliers <- subset(diabetes, diabetes$BMI > 3 & diabetes$BMI < -3)
nrow(BMI_outliers)
#Number of Outliers after standardization : 0
DiabetesPedigreeFunction_outliers <- subset(diabetes, diabetes$DiabetesPedigreeFunction > 3 & diabetes$DiabetesPedigreeFunction < -3)
nrow(DiabetesPedigreeFunction_outliers)
#Number of Outliers after standardization : 0


#Binning for Age attribute
AgeBin<-cut(diabetes$Age, c(0,20,40,60,80,100))
levels(AgeBin)

#Binning for Pregnancy attribute
diabetes$PregnancyBin<-cut(diabetes$Pregnancies, c(0,5,10,15,20), include.lowest=TRUE)
diabetes$AgeBin<-cut(diabetes$Age, c(0,20,40,60,80,100), include.lowest=TRUE)


str(diabetes)

##-------------------------------------------------------------------------------------------
indexes = sample(1:nrow(data), size=0.7*nrow(data))

# Split data
testing_model = data[-indexes,]
dim(testing_model) 
training_model = data[indexes,]
dim(training_model) 



# install.package("caTools")
# library(caTools)
# set.seed(2)
# index=sample.split(diabetes, SplitRatio=0.7)
# training=subset(diabetes, index=T)
# testing=subset(diabetes, index=F)

#remove target variable from testing and training dataset: Outcome (9th column)
training_model<-training_model[,-9]
testing_model<-testing_model[,-9]

#remove partition variable from training dataset: $part (9th column)
#training<-training[,-9]

#remove partition variable from testing dataset: $part (9th column)
#testing<-testing[,-9]

#Check validilty of partition for Pregnancies:

testing_age<-table(testing_model$Glucose)
training_age<-table(training_model$Glucose)

table_age <- as.table(rbind(training_age,
                            testing_age))

Xsq_data <- chisq.test(table_age)
# Show the test statistic,
# p-value, expected frequencies
Xsq_data$statistic  #1.013271
Xsq_data$p.value #0.3141205
Xsq_data$expected

#as per the chi square test , training data set represents actual dataset'


#-----------------------------------------  Data Modelling Phase -------------------------------------------------------------------

#Logistic reg model


m1=glm(diabetes$Outcome~., data=diabetes, family='binomial')
summary(m1)
# 
# Call:
#   glm(formula = diabetes$Outcome ~ ., family = "binomial", data = diabetes)
# 
# Deviance Residuals: 
#   Min          1Q      Median          3Q         Max  
# -2.6243085  -0.6983904  -0.3647185   0.6555557   2.5760516  
# 
# Coefficients: (3 not defined because of singularities)
# Estimate   Std. Error  z value                                    Pr(>|z|)    
# (Intercept)               -2.96363544   0.59849695 -4.95180 0.0000007353126 ***
#   Pregnancies                0.10329897   0.06395154  1.61527       0.1062523    
# Glucose                    1.18724881   0.20028194  5.92789 0.0000000030686 ***
#   BloodPressure             -0.15308594   0.10641272 -1.43861       0.1502623    
# SkinThickness              0.04970297   0.12693784  0.39155       0.6953881    
# Insulin                   -0.16040255   0.12711312 -1.26189       0.2069890    
# BMI                        0.40927815   0.16216080  2.52390       0.0116060 *  
#   DiabetesPedigreeFunction   0.29033789   0.10138161  2.86381       0.0041858 ** 
#   Age                        0.03931113   0.01965668  1.99989       0.0455125 *  
#   PregnancyBin(5,10]        -0.13048790   0.39825941 -0.32765       0.7431797    
# PregnancyBin(10,15]       -0.46905458   0.76426048 -0.61374       0.5393894    
# PregnancyBin(15,20]       10.55053744 882.74395837  0.01195       0.9904639    
# AgeBin(40,60]              0.05346419   0.40891635  0.13075       0.8959762    
# AgeBin(60,80]             -1.93548037   0.85629930 -2.26028       0.0238036 *  
#   AgeBin(80,100]           -15.88902538 882.74391439 -0.01800       0.9856392    
# Glucose2                   0.07226117   0.32122957  0.22495       0.8220168    
# BMI2                       0.71049916   0.30149877  2.35656       0.0184452 *  
#   Age40to60                          NA           NA       NA              NA    
# Preglessthan5                      NA           NA       NA              NA    
# Age20to40                          NA           NA       NA              NA    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 993.48391  on 767  degrees of freedom
# Residual deviance: 690.47882  on 751  degrees of freedom
# AIC: 724.47882
# 
# Number of Fisher Scoring iterations: 13


#From the above output it is evident that the significant variables are  Glucose,BMI,DiabetesPedigreeFunction and Age



#Decision Trees


library("rpart"); 
library("rpart.plot"); 
library("C50")
diabfit <- rpart(diabetes$Outcome ~ diabetes$Glucose + AgeBin  + diabetes$BloodPressure +
                   diabetes$BMI ,
                 data = diabetes,
                 method = "class")

my_prediction <- predict(diabfit, diabetes , type = "class")

my_solution <- data.frame(Outcome = diabetes$Outcome, Diabetic =  my_prediction)

fancyRpartPlot(diabfit)

# n= 768 
# 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 768 268 0 (0.6510416667 0.3489583333)  
# 2) diabetes$Glucose< 0.1909990468 485  94 0 (0.8061855670 0.1938144330) *
#   3) diabetes$Glucose>=0.1909990468 283 109 1 (0.3851590106 0.6148409894)  
# 6) diabetes$BMI< -0.3604221538 76  24 0 (0.6842105263 0.3157894737)  
# 12) diabetes$Glucose< 0.7824049556 41   6 0 (0.8536585366 0.1463414634) *
#   13) diabetes$Glucose>=0.7824049556 35  17 1 (0.4857142857 0.5142857143)  
# 26) AgeBin=(20,40],(60,80] 25   9 0 (0.6400000000 0.3600000000) *
#   27) AgeBin=(40,60] 10   1 1 (0.1000000000 0.9000000000) *
#   7) diabetes$BMI>=-0.3604221538 207  57 1 (0.2753623188 0.7246376812) *



diabfit <- rpart(diabetes$Outcome ~ diabetes$Glucose+diabetes$BloodPressure+diabetes$BMI+diabetes$SkinThickness,
                 data = diabetes,
                 method = "class")

rpart.plot(diabfit)
print(diabfit)
fancyRpartPlot(diabfit)

# n= 768 
# 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 768 268 0 (0.6510416667 0.3489583333)  
# 2) diabetes$Glucose< 0.1909990468 485  94 0 (0.8061855670 0.1938144330) *
#   3) diabetes$Glucose>=0.1909990468 283 109 1 (0.3851590106 0.6148409894)  
# 6) diabetes$BMI< -0.3604221538 76  24 0 (0.6842105263 0.3157894737)  
# 12) diabetes$Glucose< 0.7824049556 41   6 0 (0.8536585366 0.1463414634) *
#   13) diabetes$Glucose>=0.7824049556 35  17 1 (0.4857142857 0.5142857143)  
# 26) diabetes$SkinThickness>=-0.2653926752 15   5 0 (0.6666666667 0.3333333333) *
#   27) diabetes$SkinThickness< -0.2653926752 20   7 1 (0.3500000000 0.6500000000) *
#   7) diabetes$BMI>=-0.3604221538 207  57 1 (0.2753623188 0.7246376812) *



diabfit <- rpart(diabetes$Outcome ~ diabetes$Glucose+diabetes$BMI,
                 data = diabetes,
                 method = "class")

rpart.plot(diabfit)
print(diabfit)
fancyRpartPlot(diabfit)

#n= 768 

# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 768 268 0 (0.6510417 0.3489583)  
# 2) diabetes$Glucose< 0.190999 485  94 0 (0.8061856 0.1938144) *
#   3) diabetes$Glucose>=0.190999 283 109 1 (0.3851590 0.6148410)  
# 6) diabetes$BMI< -0.3604222 76  24 0 (0.6842105 0.3157895) *
#   7) diabetes$BMI>=-0.3604222 207  57 1 (0.2753623 0.7246377) *

#





plot(diabfit)
text(diabfit)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(diabfit)

# Put the predictors into 'x', the response into 'y'
names(diabetes)
x <- diabetes[,c(2,3,4,5,6,7,8)]
y <- diabetes$Outcome
c50fit <- C5.0(x, y)
summary(c50fit)

# Call:
#   C5.0.default(x = x, y = y)
# 
# 
# C5.0 [Release 2.07 GPL Edition]  	Sun Apr 23 19:53:52 2017
# -------------------------------
#   
#   Class specified by attribute `outcome'
# 
# Read 768 cases (8 attributes) from undefined.data
# 
# Decision tree:
# 
# Glucose <= 0.1745711:
# :...BMI <= -0.8726218: 0 (124/1)
# :   BMI > -0.8726218:
# :   :...Age <= 28: 0 (186/22)
# :       Age > 28:
# :       :...Glucose > -0.7453936:
# :           :...DiabetesPedigreeFunction <= -0.820564: 0 (21/4)
# :           :   DiabetesPedigreeFunction > -0.820564: 1 (101/42)
# :           Glucose <= -0.7453936:
# :           :...DiabetesPedigreeFunction <= 0.9510912: 0 (45/4)
# :               DiabetesPedigreeFunction > 0.9510912:
# :               :...SkinThickness <= 0.6776816: 1 (5/1)
# :                   SkinThickness > 0.6776816: 0 (3)
# Glucose > 0.1745711:
# :...BMI > -0.3676362:
# :...Glucose > 1.160248: 1 (92/12)
# :   Glucose <= 1.160248:
# :   :...Age > 30:
# :       :...DiabetesPedigreeFunction > -0.1354435: 1 (37/5)
# :       :   DiabetesPedigreeFunction <= -0.1354435:
# :       :   :...BMI <= 1.883156: 0 (23/10)
# :       :       BMI > 1.883156: 1 (5)
# :       Age <= 30:
# :       :...BloodPressure <= -0.02441388:
# :           :...BloodPressure <= -0.9329975: 1 (5)
# :           :   BloodPressure > -0.9329975:
# :           :   :...BloodPressure <= -0.6852019: 0 (4)
# :           :       BloodPressure > -0.6852019: 1 (17/4)
# :           BloodPressure > -0.02441388:
# :           :...BloodPressure <= 1.049367: 0 (17/1)
# :               BloodPressure > 1.049367:
# :               :...BMI <= 1.277174: 0 (3)
# :                   BMI > 1.277174: 1 (4)
# BMI <= -0.3676362:
# :...Glucose <= 0.765977: 0 (41/6)
# Glucose > 0.765977:
# :...Age <= 25: 0 (4)
# Age > 25:
# :...Age > 61: 0 (4)
# Age <= 61:
# :...BMI <= -0.7716247: 1 (12/1)
# BMI > -0.7716247:
# :...BloodPressure > 0.8015712: 0 (4)
# BloodPressure <= 0.8015712:
# :...DiabetesPedigreeFunction <= -0.2290062: 1 (8/1)
# DiabetesPedigreeFunction > -0.2290062: 0 (3)
# 
# 
# Evaluation on training data (768 cases):
# 
# Decision Tree   
# ----------------  
# Size      Errors  
# 
# 24  114(14.8%)   <<
# 
# 
# (a)   (b)    <-classified as
# ----  ----
# 434    66    (a): class 0
# 48   220    (b): class 1
# 
# 
# Attribute usage:
# 
# 100.00%	Glucose
# 100.00%	BMI
# 66.54%	Age
# 32.68%	DiabetesPedigreeFunction
# 8.46%	BloodPressure
# 1.04%	SkinThickness
# 
# 
# Time: 0.0 secs






p1=predict(m1, diabetes, type='response')
p1=round(p1)
table(p1, diabetes$Outcome)


#Neural Networks

library(neuralnet)

# Determine how many Indicator variables are needed
unique(diabetes$Glucose) # One variable for Glucose
unique(diabetes$AgeBin) # 5 variable for sex
unique(diabetes$BMI) # One variables for race
unique(diabetes$BloodPressure) # One variables for workclass
unique(diabetes$PregnancyBin) # One variables for marital.status


# Declare, assign indicator variables


diabetes$Glucose2 <- diabetes$BMI2  <- diabetes$Age20to40 <- diabetes$Age40to60 <- diabetes$Preglessthan5 <- c(rep(0, length(diabetes$Outcome)))

for (i in 1:length(diabetes$Outcome)) {
  diabetes$AgeBin[i]
  if(diabetes$Glucose[i] > 0.19)
    diabetes$Glucose2[i] <-1
  if(diabetes$BMI[i] > -0.36)
    diabetes$BMI2[i] <-1
  if(diabetes$AgeBin[i] == "(20,40]")
    diabetes$Age20to40[i] <- 1
  if(diabetes$AgeBin[i] == "(40,60]")
    diabetes$Age40to60[i] <- 1
  if(diabetes$PregnancyBin[i] == "[0,5]")
    diabetes$Preglessthan5[i] <- 1
}

diabetes1 <- diabetes[,-c(1,2, 3, 4,5, 6, 7, 8, 10, 11)]
diabetes1$Outcome <- as.numeric(diabetes1$Outcome)
str(diabetes1)

# Run the neural net package
library(neuralnet)
print(net.dat <- neuralnet(formula =
                             diabetes1$Outcome ~  diabetes1$Preglessthan5 + diabetes1$Glucose2 + diabetes1$Age20to40 +
                             diabetes1$BMI2  + diabetes1$Age40to60 ,
                           data = diabetes1,
                           rep = 10,
                           hidden = 8,
                           linear.output=FALSE))

plot(net.dat, show.weights = FALSE)


#KNN 




str(diabetes)
#
diabetes <- diabetes[-11]
diabetes <- diabetes[-10]



#remove target variable from testing and training dataset: Outcome (9th column)


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
#
diabetes_n <- as.data.frame(lapply(diabetes[1:8], normalize))
diabetes_n





#creating training labels and
#test labels
str(diabetes)
diabetes_train_labels <- diabetes[1:500, 9]
diabetes_test_labels <- diabetes[501:768, 9]

#testing and training sets
dia_train <- diabetes_n[1:500,]
dia_test <- diabetes_n[501:768,]


#now we get set up to use knn
install.packages("class")
library(class)
#now use knn to train a  model on the data
training1<-dia_train[,-9]
testing1<-dia_test[,-9]
#note the target variable has not
#been included in the training and
#test data sets
str(training1)
diabetes_test_pred <- knn(train = training1, 
                          test = testing1,
                          cl = diabetes_train_labels,
                          k=10)
#now we can evaluate the model performance
#we want to check the accuracy of the
#predicted values in prc_test_pred
#the result file from knn
#to see if they match up with the known
#labels in prc_test_labels

#
#we need to use the crosstable() function
#available in the package gmodels
install.packages("gmodels")
library(gmodels)
CrossTable(x=diabetes_test_labels,
           y=diabetes_test_pred,prop.chisq=FALSE)


#######################################
