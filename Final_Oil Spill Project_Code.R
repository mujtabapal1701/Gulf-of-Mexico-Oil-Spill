#Loading Data
setwd("~/Documents/Math 301/Oil Spill Project")
oilspill = read.csv("oil_data.csv", header=T)
attach(oilspill)
plot(day,oil, xlab = "Days", ylab = "Oil (barrels)")

#Our Best Model (Three Parts )
#(Residual standard error: 2943 on 4 degrees of freedom), (Residual standard error: 7099 on 33 degrees of freedom),
# and (Residual standard error: 7461 on 40 degrees of freedom) (17,503 Total)
#Partition
#Linear model from 2 to 43, day 1 not full day (Residual standard error: 7461 on 40 degrees of freedom)
oilspill43 = subset(oilspill,  (day < 44 & day>1), select = c(day,oil)) #Data from day 2 until 43
day43 = oilspill43$day
oil43 = oilspill43$oil
plot(day43,oil43,main = "First Linear Model",xlab = "Day 1 to 43", ylab = "Oil (barrels)")
modelin = lm(oil43 ~ day43)
summary(modelin)
abline(modelin, col="red") 

#Separating the Remaining Data
#Three way partition (2nd part)
oilspill44t79 = subset(oilspill,  (day > 43 & day < 80), select = c(day,oil))
day44t79 = oilspill44t79$day
oil44t79 = oilspill44t79$oil
plot(day44t79, oil44t79,xlab = "Days from 43 to 79", ylab = "Oil (barrels)")

#Three way partition (3rd part)
oilspill80t85 = subset(oilspill,  (day > 79 ), select = c(day,oil))
day80t85 = oilspill80t85$day
oil80t85 = oilspill80t85$oil
plot(day80t85, oil80t85, xlab = "Day 80 to 85", ylab = "Oil  (barrels)")

#Middle part 3ac of Best Model (Residual standard error: 7099 on 33 degrees of freedom)
modelcm= function(t,a,d,c) (a*t-(d*t^2)/2+c)
modelcmfit = nls(oil ~ modelcm(day,mya,myd,myc), data=oilspill44t79, start=c(mya=60000, myd=10, myc=1))
summary(modelcmfit)
points(day44t79,predict(modelcmfit), col="red")
plot(day44t79,oil44t79,main = "Middle Quadratic Model", xlab = "Day 44 to 79", ylab = "Oil (barrels)")
lines(day44t79,predict(modelcmfit),col = "blue")

#3rd part linear of Best Model (Residual standard error: 2943 on 4 degrees of freedom)
plot(day80t85,oil80t85, main = "End Linear Model",xlab = "Days 80 to 85", ylab = "Oil (barrels)")
modelin3 = lm(oil80t85 ~ day80t85)
summary(modelin3)
abline(modelin3, col="red") 

#All Models on one graph
plot(day,oil,main = "Entire Model", xlab = "Days", ylab = "Oil (barrels)")
lines(day43,predict(modelin), col="darkorchid1")
lines(day44t79,predict(modelcmfit),col = "darkorange1")
lines(day80t85,predict(modelin3),col = "darkturquoise")

#Inferior Models
#Middle part of other inferior threeway model
#The first and last linear sections remained the same (modelin and modelin3)
#(Residual standard error: 2943 on 4 degrees of freedom), (Residual standard error: 8361 on 33 degrees of freedom),
# and (Residual standard error: 7461 on 40 degrees of freedom) (18,765 Total)
#Middle part 3bc (Residual standard error: 8361 on 33 degrees of freedom)
modelbcmt= function(t,a,d,c) (a*t-(d*t^3)/3+c)
modelbcmtfit = nls(oil ~ modelbcmt(day,mya,myd,myc), data=oilspill44t79, start=c(mya=60000, myd=5, myc=1))
summary(modelbcmtfit)
points(day44t79,predict(modelbcmtfit), col="red")
plot(day44t79,oil44t79, xlab = "Day 44 to 79", ylab = "Oil (barrels)")
lines(day44t79,predict(modelbcmtfit),col = "blue")

#Two Step Models
#Both of these use the first linear model (modelin)
#Second Half of Data partition
oilspill44t85 = subset(oilspill,  day > 43, select = c(day,oil))
day44t85 = oilspill44t85$day
oil44t85 = oilspill44t85$oil
plot(day44t85, oil44t85)

#Second half model with 3ac (Quadratic) (7461 on 40 degrees of freedom) and (18360 on 39 degrees of freedom)
modelct= function(t,a,d,c) (a*t-(d*t^2)/2+c)
modelctfit = nls(oil ~ modelct(day,mya,myd,myc), data=oilspill44t85, start=c(mya=60000, myd=10, myc=1))
summary(modelctfit)
points(day44t85,predict(modelctfit), col="red")
plot(day44t85,oil44t85, xlab = "Day 44 to 85", ylab = "Oil (barrels)")
lines(day44t85,predict(modelctfit),col = "blue")

#Second half model with 3bc (Cubed) (7461 on 40 degrees of freedom) and (19480 on 39 degrees of freedom)
modelbct= function(t,a,d,c) (a*t-(d*t^3)/3+c)
modelbctfit = nls(oil ~ modelbct(day,mya,myd,myc), data=oilspill44t85, start=c(mya=60000, myd=5, myc=1))
summary(modelbctfit)
points(day44t85,predict(modelbctfit), col="red")
plot(day44t85,oil44t85, xlab = "Day 44 to 85", ylab = "Oil (barrels)")
lines(day44t85,predict(modelbctfit),col = "blue")

#Single model for all of the data
#Model from 3ac (Quadratic) (Residual standard error: 33640 on 82 degrees of freedom)
modelc= function(t,a,d,c) (a*t-(d*t^2)/2+c)
modelcfit = nls(oil ~ modelc(day,mya,myd,myc), data=oilspill, start=c(mya=60000, myd=10, myc=1))
summary(modelcfit)
points(day,predict(modelcfit), col="red")
plot(day,oil,xlab = "Days", ylab = "Oil (barrels)")
lines(day,predict(modelcfit),col = "blue")

#Model from 3bc (Cubed) (Residual standard error: 30350 on 82 degrees of freedom)
plot(day,oil)
modelbc= function(t,a,d,c) (a*t-(d*t^3)/3+c)
modelbcfit = nls(oil ~ modelbc(day,mya,myd,myc), data=oilspill, start=c(mya=60000, myd=5, myc=1))
summary(modelbcfit)
points(day,predict(modelbcfit), col="red")
plot(day,oil, xlab = "Days", ylab = "Oil (barrels)")
lines(day,predict(modelbcfit),col = "blue")

#Linear Model (Residual standard error: 156800 on 83 degrees of freedom)
plot(day,oil,xlab = "Days", ylab = "Oil (barrels)")
modellinb = lm(oil~day)
summary(modellinb)
abline(modellinb, col="red")

#All Model Summaries
#Our Best Model
summary(modelin)
summary(modelcmfit)
summary(modelin3)
#Inferior 3 Step Model
summary(modelin)
summary(modelbcmtfit)
summary(modelin3)
#Two Step Model 1
summary(modelin)
summary(modelctfit)
#Two Step Model 2
summary(modelin)
summary(modelbctfit)
#One Step Model 1 
summary(modelcfit)
#One Step Model 2 
summary(modelbcfit)
#One Step Model 3
summary(modellinb)

#Redrawing our best model
plot(day,oil,main = "Entire Model", xlab = "Days", ylab = "Oil (barrels)")
lines(day43,predict(modelin), col="darkorchid1")
lines(day44t79,predict(modelcmfit),col = "darkorange1")
lines(day80t85,predict(modelin3),col = "darkturquoise")


#Failed Models
#Model from 2e
#plot(day,oil)
#modele = function(t,a,b,c) (a*(t/b-1/b^2)+1+c/(exp(b*t)))
#modelefit= nls(oil~modele(day,mya,myb,myc), data=oilspill, start=c(mya=60000,myb=2000,myc=-10000))

#Model from 2a
#plot(day,oil)
#modela = function(t,a,b,c) (a*t-b*t+c)
#modelafit = nls(oil~ modela(day,mya,myb,myc),data=oilspill, start=c(mya=2000000,myb=2000,myc=-10000))

#Model from 2b
#plot(day,oil)
#modelb = function(t,a,b,c) (a/b+c/(exp(b*t)))
#modelbfit = nls(oil ~modelb(day,mya,myb,myc),data=oilspill, start=c(mya=120000, myb=2, myc=10))                           

#Model 3bd
#plot(day,oil)
#modelbd =function(t,a,k,c,d) ((a-d*t^2)/k+((2*d)/k^2)*t-(2*d)/k^3+c*exp(-k*t))
#modelbdfit = nls(oil ~ modelbd(day,mya,myk,myc,myd), data=oilspill, start=c(mya=50000, myk=10, myc=1000, myd=2000))

#Model from 3ad
#plot(day,oil)
#modeld = function(t,a,b,c,d) (a/b-d*(t/b-1/b^2)+(c/exp(b*t)))
#modeldfit = nls(oil ~ modeld(day,mya,myb,myc,myd), data=oilspill, start=c(mya=600000, myb=12, myc=2000, myd=5000))
