setwd("C:/Users/IIM")
library("psych", lib.loc="~/R/win-library/3.4")
asd<-read.csv(paste("kc_house_data.csv", sep = ""))
View(asd)
library("corrgram", lib.loc="~/R/win-library/3.4")
library("corrplot", lib.loc="~/R/win-library/3.4")
library("car", lib.loc="~/R/win-library/3.4")
library("ggplot2", lib.loc="~/R/win-library/3.4")
library("MASS", lib.loc="C:/Program Files/R/R-3.4.0/library")
corrgram(asd,upper.panel = panel.pie)
summary(asd)

#Now evaluating the three important variables
boxplot(asd$price~asd$sqft_living, xlab="sqft_living", ylab="Price")
boxplot(asd$price~asd$grade, xlab="grade", ylab="Price")
boxplot(asd$price~asd$sqft_above, xlab="sqft_above", ylab="Price")


scatterplot(price~sqft_living,data = asd, xlab = "sqft_living",ylab = "Price")
scatterplot(price~grade,data = asd, xlab = "grade",ylab = "Price")
scatterplot(price~sqft_above,data = asd, xlab = "sqft_above",ylab = "Price")

dat<-cor(asd[,c(3,6,12,13)])
corrgram(dat,upper.panel = panel.pie)

cov(dat)
cor(dat)

#Now checkng for different factors individually

cor.test(asd$price,asd$bedrooms) #1

cor.test(asd$price,asd$bathrooms) #2

cor.test(asd$price,asd$sqft_living) #3

cor.test(asd$price,asd$sqft_lot) #4

cor.test(asd$price,asd$floors) #5

scatterplot(price~waterfront,data=asd) #6
mytable<-xtabs(~price+waterfront, data=asd)
chisq.test(mytable, simulate.p.value = TRUE)
cor.test(asd$price,asd$waterfront)

cor.test(asd$price,asd$view) #7
qq<- asd$view[which(asd$view!=0)]
aa<-asd$price[which(asd$view!=0)]
cor.test(aa,qq)

cor.test(asd$price,asd$condition) #8

cor.test(asd$price,asd$grade) #9

cor.test(asd$price,asd$sqft_above) #10

cor.test(asd$price,asd$sqft_basement) #11

cor.test(asd$price,asd$yr_built) #12

cor.test(asd$price,asd$yr_renovated) #13
qq<-asd$price[which(asd$yr_renovated!=0)]
aa<-asd$yr_renovated[which(asd$yr_renovated!=0)]
cor.test(aa,qq)

cor.test(asd$price,asd$sqft_living15) #14

cor.test(asd$price,asd$sqft_lot15) #15

#Now checking for various regression models:
model1<-lm(price~.-id-date-zipcode-lat-long,data = asd)
summary(model1)
model2<-lm(price~.-id-date-zipcode-lat-long+I(sqft_lot/sqft_lot15),data = asd)
summary(model2)
model3<-lm(price~.-id-date-zipcode-lat-long+sqft_lot*sqft_lot15+sqft_living*sqft_living15,data = asd)
summary(model3)
model4<-lm(price~.-id-date-zipcode-lat-long-condition+sqft_lot*sqft_lot15+sqft_living*sqft_living15,data = asd)
summary(model4)
model5<-lm(price~.-id-date-zipcode-lat-long+I(sqft_lot/sqft_lot15)+I(sqft_living/sqft_living15),data = asd)
summary(model5)
model6<-lm(price~.-id-date-zipcode-lat-long-yr_renovated+sqft_lot*sqft_lot15+sqft_living*sqft_living15,data = asd)
summary(model6)
model7<-lm(price~bathrooms+sqft_living+sqft_living15+grade+sqft_above,data = asd)
summary(model7)
model8<-lm(price~bathrooms+sqft_living+sqft_living15+grade+sqft_above+bedrooms+view+sqft_basement,data = asd)
summary(model8)

#Model 3 gives the best Adjusted R-square value (0.6719). Hence is the best fit.