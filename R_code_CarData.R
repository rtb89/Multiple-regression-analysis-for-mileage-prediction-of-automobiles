## Get the working directory and set it to the folder where the data lies
getwd()
ls()
## Read the data ## There are options to specify nrows , colClasses , sep , skip, rownames, colnames)
## read.csv for csv data and readLines for text data
## Foriegn Package for reading various data formats

carData <- read.table("car_data.txt", header=T)

## First get some metadata and look over the dataset ##
class(carData)
nrow(carData)
ncol(carData)
dim(carData)
names(carData)
summary(carData)
mean(WT)

## Subsetting 
carData[1,]    ## Gives the first row of the data frame
carData[,1]    ## Gives the first coulumn of the data frame
carData[1:4,]  ## Gives first 4 rows
carData[,c("WT","HP")] ## Extracting columns with colnames


## Referencing an object
class(carData$WT)
attach(carData)
class(WT)
colclass= sapply(carData,class)

## Removing missing values
carData[63,] <- c("GM/GeoMetroXF1", 89, NA, 40, 20, NA) ## Insert a new row with some missing values
attach(carData)
tail(carData)
bad <- is.na(WT)
bad
WT[!bad]
good <- complete.cases(carData)
good
carData[good,]

## Read the cleaned car data again

## WE examine the relation between the response variable and with each explanatory variable separately
## Considering the volume first

plot (VOL,MPG, xlab= "Cubic feet of cab space", ylab =" Average miles per gallon ", pch=19, cex=0.5)

##identify(VOL, MPG, MAKE.MODEL)

## Since there is no linearity, we try to fit the polynomial regression
VOL2=VOL^2
VOL3=VOL^3
VOL4=VOL^4
VOL5=VOL^5
modv5=lm (MPG~VOL+VOL2+VOL3+VOL4+VOL5)
sumv5=summary(modv5)
##Plotting the regression curve
xx= 480:1250/10
y5 = sumv5$coe[1,1] + sumv5$coe[2,1]*xx + sumv5$coe[3,1]*xx^2 + sumv5$coe[4,1]*xx^3 + sumv5$coe[5,1]*xx^4+sumv5$coe[6,1]*xx^5
points (xx, y5, type = "l", col = 2)

## Now the horsepower
plot(HP,MPG, xlab= "Engine Horspower", ylab =" Average miles per gallon ", pch=19, cex=0.5)
##identify(HP, MPG, MAKE.MODEL)
par(mfrow = c(1, 2))
plot(1/(HP),MPG, xlab = "Inverse(Engine Horspower)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/(HP),1/log(MPG), xlab = "Inverse(Engine Horspower)", ylab =" 1/Log(Average miles per gallon)", pch=19, cex=0.5)
HPT = 1/HP
## Now examining the Speed
plot(SP,MPG, xlab= "Top speed (mph)", ylab =" Average miles per gallon ", pch=19, cex=0.5)
plot(1/(SP)^4,MPG, xlab = "(Top speed)^(-4) (mph)", ylab =" Average miles per gallon ", pch=19, cex=0.5)

##identify(1/(SP)^4,MPG,MAKE.MODEL)
plot(1/(SP)^4,1/log(MPG), xlab = "(Top speed)^(-4) (mph)", ylab =" 1/log(Average miles per gallon) ", pch=19, cex=0.5)
SPT= 1/(SP)^4
MPGT = 1/log(MPG)

## Install lattice package
library (lattice)
class= factor(WT)
plot(WT, MPG, pch =19, cex = 0.5)
xyplot( MPG~WT, group = class,auto.key = list (title = "Class"))

## Although we see different categories in the weight in these particular data set and we can keep wieght as a categorical variable
## However weight could vary continously it is not actualy descrete so keep it as a numerical variable


## We consider the following models
MODC= lm(MPG~VOL+HP+SP+WT)
MOD1= lm(MPG~ VOL+HPT+SPT+WT)
MOD2= lm(MPGT~ VOL+HP+SP+WT)
MOD3= lm(MPGT~ VOL+HPT+SPT+WT)
MOD4= lm(MPGT~ VOL+HP+SPT+WT)
MOD5= lm(MPGT~ VOL+VOL2+VOL3+VOL4+VOL5+HPT+SPT+WT)
MOD6= lm(MPGT~ HP+SPT+WT)


##We first look at the adjusted R square value of the model to see the effect of addition of variables on the model
summary(lm(MPGT ~ -0))$adj.r.sq
summary(lm(MPGT ~ WT))$adj.r.sq
summary(lm(MPGT ~ HPT))$adj.r.sq
summary(lm(MPGT ~ SPT))$adj.r.sq
summary(lm(MPGT ~ VOL))$adj.r.sq
##2 Variables models:
summary(lm(MPGT ~ WT+HPT))$adj.r.sq
summary(lm(MPGT ~ WT+SPT))$adj.r.sq
summary(lm(MPGT ~ WT+VOL))$adj.r.sq
summary(lm(MPGT ~ HPT+SPT))$adj.r.sq
summary(lm(MPGT ~ HPT+VOL))$adj.r.sq
summary(lm(MPGT ~ SPT+VOL))$adj.r.sq
summary(lm(MPGT ~ HPT+SPT))$adj.r.sq
summary(lm(MPGT ~ HP+SPT))$adj.r.sq
#3 Variables:
summary(lm(MPGT ~ WT+SPT+VOL))$adj.r.sq
summary(lm(MPGT ~ WT+SPT+HP))$adj.r.sq   
summary(lm(MPGT ~ WT+SPT+HPT))$adj.r.sq
summary(lm(MPGT ~ VOL+HPT+SPT))$adj.r.sq
summary(lm(MPGT ~ WT+VOL+HPT))$adj.r.sq
summary(lm(MPGT ~ VOL+HP+SPT))$adj.r.sq
summary(lm(MPGT ~ VOL+HPT+SP))$adj.r.sq
## 4 variables
summary(lm(MPGT ~ VOL+HPT+SPT+WT))$adj.r.sq
summary(lm(MPGT ~ VOL+HP+SPT+WT))$adj.r.sq     
summary(lm(MPGT ~ VOL+VOL2+VOL3+VOL4+VOL5+HP+SPT+WT))$adj.r.sq

##THE AIC value:
AIC(lm(MPGT ~ -0))
AIC(lm(MPGT ~ WT))
AIC(lm(MPGT ~ HPT))
AIC(lm(MPGT ~ SPT))
AIC(lm(MPGT ~ VOL))
AIC(lm(MPGT ~ WT+HPT))
AIC(lm(MPGT ~ WT+SPT))
AIC(lm(MPGT ~ HPT+SPT))
AIC(lm(MPGT ~ HPT+VOL))
AIC(lm(MPGT ~ SPT+VOL))
AIC(lm(MPGT ~ HPT+SPT))
AIC(lm(MPGT ~ HP+SPT))
AIC(lm(MPGT ~ WT+SPT+VOL))
AIC(lm(MPGT ~ WT+SPT+HP))
AIC(lm(MPGT ~ WT+SPT+HPT))
AIC(lm(MPGT ~ VOL+HPT+SPT))
AIC(lm(MPGT ~ WT+VOL+HPT))
AIC(lm(MPGT ~ VOL+HP+SPT))
AIC(lm(MPGT ~ VOL+HPT+SP))
AIC(lm(MPGT ~ VOL+HP+SP))
AIC(lm(MPGT ~ VOL+HPT+SPT+WT))
AIC(lm(MPGT ~ VOL+HP+SPT+WT))
AIC(lm(MPGT ~ VOL+VOL2+VOL3+VOL4+VOL5+HP+SPT+WT))

## The BIC Value
BIC(lm(MPGT ~ - 0))
BIC(lm(MPGT ~ WT))
BIC(lm(MPGT ~ HPT))
BIC(lm(MPGT ~ SPT))
BIC(lm(MPGT ~ VOL))
BIC(lm(MPGT ~ WT+HPT))
BIC(lm(MPGT ~ WT+SPT))
BIC(lm(MPGT ~ WT+VOL))
BIC(lm(MPGT ~ HPT+SPT))
BIC(lm(MPGT ~ HPT+VOL))
BIC(lm(MPGT ~ SPT+VOL))
BIC(lm(MPGT ~ WT+SPT+VOL))
BIC(lm(MPGT ~ WT+SPT+HP))
BIC(lm(MPGT ~ WT+SPT+HPT))
BIC(lm(MPGT ~ HPT+SPT))
BIC(lm(MPGT ~ HP+SPT))
BIC(lm(MPGT ~ VOL+HPT+SPT))
BIC(lm(MPGT ~ WT+VOL+HPT))
BIC(lm(MPGT ~ VOL+HP+SPT))
BIC(lm(MPGT ~ VOL+HPT+SP))
BIC(lm(MPGT ~ VOL+HP+SP))
BIC(lm(MPGT ~ VOL+HPT+SPT+WT))
BIC(lm(MPGT ~ VOL+HP+SPT+WT))
BIC(lm(MPGT ~ VOL+VOL2+VOL3+VOL4+VOL5+HP+SPT+WT))

## The adjusted R square value and the AIC value suggests the same model, however the BIC values gives a different picture.
##Based on the highest adjusted R value we examine these models
##MOD5= lm (MPG~ VOL+VOL2+VOL3+VOL4+VOL5+HPT+SPT+WT)
##Although the adjusted R value is high for model 5 the p-value for VOL2, VOL3, VOL4, VOL5 are not significant , more over the mod of AIC value is also not the highest for this model. So we drop this model.
## So we take into account the below model with 3 explanatory variable:
##mod6= lm (MPGT~ HP+SPT+WT)             where MPGT= 1/log(MPG) & SPT=SP^(-4)




## Now to check whether the homoscadesty is met, we check the Q-Q plot and residual plot
qqnorm(MOD6$res, pch = 19, cex = 0.5)
qqline(MOD6$res)

## Now the residual plot to check whether the errors are randomly distributed or not.

plot(MOD6$fit, MOD6$res, xlab = "Fitted Values", ylab = "Residual Value", pch = 19, cex = 0.5)

## Now we would do some outlier detection to see whether these ouliers are problematic or not

lev = hatvalues(MOD6)
plot(lev, ylab = "Leverage Value", pch = 19, cex = 0.5)
identify(lev, labels =MAKE.MODEL)


##These plots shows the same outliers that were present in the plot of the residuals versus the fitted values.

##Now to examine the effect of outliers on the coefficients values, we calculate DFBETAS and plot it for all the points. 
#The below plots the deletion effect on the coefficients of the regression line
dif_betas = dfbeta(MOD6)
par(mfrow = c(1, 1))
plot(dif_betas[,1], ylab = "Change in Intercept Value", pch = 19, cex = 0.5)
identify(dif_betas[,1], labels = MAKE.MODEL)
plot(dif_betas[,2], ylab = "Change in beta1 Value", pch = 19, cex = 0.5)
identify(dif_betas[,2], labels = MAKE.MODEL)
plot(dif_betas[,3], ylab = "Change in beta2 Value", pch = 19, cex = 0.5)
identify(dif_betas[,3], labels = MAKE.MODEL)
plot(dif_betas[,4], ylab = "Change in beta3 Value", pch = 19, cex = 0.5)
identify(dif_betas[,4], labels = MAKE.MODEL)


## change in fitted values by the deletion of various index and the cook’s distance

dif_fits = dffits(MOD6)
plot(dif_fits, ylab = "Change in Fitted Value", pch = 19, cex = 0.5)
identify(dif_fits, labels = MAKE.MODEL)

##Lastly, Cook's distances

cooks = cooks.distance(MOD6)
plot(cooks, ylab = "Cook's Distances", pch = 19, cex = 0.5)
identify(cooks , labels = MAKE.MODEL)

##Though these plots are not perfect, the model deviations exhibited by them is not egregious enough to warrant further transformations

## So we keep our final model as  
##  MPG=exp (1/ (0.268 -3.5*10-4 HP – 4.59*106 SP-4 + 3.111*10-1WT))

