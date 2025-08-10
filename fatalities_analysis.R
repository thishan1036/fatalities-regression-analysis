###############################################################################
# Final presentation
#
# Young Han
#
# Analyzing high-risk factors in fatalities
#
################################################################################
# External Functions
################################################################################
library(faraway)
library(smallstuff)
library(broom)
library(sur)
library(AER)
################################################################################
# Internal Functions
################################################################################
source("smallstuff2.R")
################################################################################
# Save the environment 
################################################################################
parSave=par(no.readonly = TRUE)
#par(parSave)
################################################################################
# Processing 
################################################################################

# Load data and conduct simple data check 
data("Fatalities")
?Fatalities
summary(Fatalities)
dim(Fatalities)
head(Fatalities)

# predictors
head(Fatalities[c('unemp','income','spirits','beertax')])

sum(is.na(Fatalities))
# There are 2 NAs in the dataset
# One NA in jail variable
# One NA in service variable

which(!complete.cases(Fatalities))
# observation 28 has both NAs

# Creating a new dataset without NAs
Fatalities=na.omit(Fatalities)
dim(Fatalities)
sum(is.na(Fatalities))
summary(Fatalities)

# checking categorical variables
# All of them are boolean variables
contrasts(Fatalities$breath)
contrasts(Fatalities$jail)
contrasts(Fatalities$service)


# Performing linear regression on dataset with pre-selected predictors
# first model ##################################################################
lmod1=lm(fatal~unemp+income+spirits+beertax+miles,Fatalities)
summary(lmod1)
coef(lmod1)

plot(residuals(lmod1)~fitted(lmod1),
     xlab='Fitted',
     ylab = 'Residuals',
     main='Residual Plot')
abline(h=0,col=2)
# funnel shape

summary(lm(sqrt(abs(residuals(lmod1)))~fitted(lmod1)))
glance(lmod1)$p.value
qqnorm(resid(lmod1));qqline(resid(lmod1))
# p-value is too small (4.36e-18 < 0.05). We reject the null hypothesis of 
# homoscedasticity
# Also, qqplot shows long tailed distribution.
shapiro.test(residuals(lmod1))
glance(shapiro.test(residuals(lmod1)))$p.value
# p-value is too small (2.51e-22<0.05). reject null hypothesis of normality
# We reject first model and move on to the next model.


# second model: Apply transformation to the response and remove a predictor ####
lmod2=update(lmod1,log(fatal)~.-miles)
summary(lmod2)
coef(lmod2)
# fatal = .482+.216*unemp+.000360*income-.610*spirits+.844*beertax+eps

plot(residuals(lmod2)~fitted(lmod2),
     xlab='Fitted',
     ylab = 'Residuals',
     main='Residual Plot')
abline(h=0,col=2)
# no funnel shape

summary(lm(sqrt(abs(residuals(lmod2)))~fitted(lmod2)))
# p-value is 0.953>0.05. We retain the null hypothesis of homoscedasticity.
qqnorm(resid(lmod2));qqline(resid(lmod2))
# looks normally distributed
shapiro.test(residuals(lmod2))
# p-value is 0.536>0.05. We retain the null hypothesis of normality and conclude
# that the residuals are normally distributed.


# Checking presence of leverages, outliers, and influential points
# High leverages ###############################################################
hatv=hatvalues(lmod2)
(hi=hatv[hatv>2*mean(hatv)])
length(hi)
# 33 possible high leverage values

# highlighting 4 highest leverages
(hiorder=names(hi[order(hi,decreasing = T)[1:4]]))
halfnorm(hatv,4,ylab='Leverages');qqlineHalf(hatv)
Fatalities[hiorder,c('state','unemp','income','spirits','beertax')]
summary(Fatalities[c('unemp','income','spirits','beertax')])
# States with high spirits consumption (Nevada and New Hampshire) and a state 
# with high beertax (Georgia) seem to have extreme predictor values

# Create standardized residual plot and compare to the regular residual plot
# indicate high leverage indices
idx1=order(hi,decreasing = T)[1:33]
par(mfrow=c(1,2))
plot(resid(lmod2)~fitted(lmod2),
     xlab='Fitted values',
     ylab='Residuals')
abline(h=0,col=2)
points(resid(lmod2)[idx1]~fitted(lmod2)[idx1],col=3,pch=16)
plot(rstandard(lmod2)~fitted(lmod2),
     xlab='Fitted values',
     ylab='Standardized residuals')
abline(h=0,col=2)
points(rstandard(lmod2)[idx1]~fitted(lmod2)[idx1],col=3,pch=16)
par(parSave)
# Shows that the absolute values of the residuals is smaller in the regular plot
# Fitted values are closer to the response

# Create a new model without high leverages and compare it to the original model
idx2=as.numeric(names(hi))
lmod3=update(lmod2,subset=-idx2)

summary(lmod2) # R2: 46.8%, RSE: 0.679
summary(lmod3) # R2: 48.6%, RSE: 0.670
# not much difference after removing high leverages, so continue working with 
# our second model, lmod2

# Outliers #####################################################################
(out=rstudent(lmod2)[abs(rstudent(lmod2))>3])
Fatalities[282,c('state','unemp','income','spirits','beertax')]
# 1 outlier
(idx3=order(abs(rstudent(lmod2)),decreasing = T)[1])
lmod4=update(lmod2,subset=-idx3)

summary(lmod2) # R2: 46.7%, RSE: 0.679
summary(lmod4) # R2: 47.6%, RSE: 0.671
# not much difference after taking out an outlier, so we continue working with
# model 2

# Influential observations #####################################################
cook=cooks.distance(lmod2)
idx4=order(cook,decreasing = T)[1:6]
cook[idx4] # all of them are very low (less than 0.5)
plot(cook,ylab = "Cook's Distance",
     main = "Cook's Distance vs. Index")

unname(cook[idx4])
points(idx4,unname(cook[idx4]),pch=16,col=3)
# Several looks apart from the others

halfnorm(cook,6,ylab = "Cook's Distance",
         main="Cook's Distance vs. Half-normal Quantiles")
qqlineHalf(cook)

# final model without 6 highest influential observations #######################
# (observations were removed for the purpose of academical presentation)
lmod5=update(lmod2,subset=-idx4)
par(mfrow=c(1,2))
plot(lmod2,5)
plot(lmod5,5)
par(parSave)

summary(lmod2) # R2: 46.8%, RSE: 0.679
summary(lmod5) # R2: 52.0%, RSE: 0.650
# Slightly better result by removing 6 highest influential observations

# final model ##################################################################
plot(residuals(lmod5)~fitted(lmod5),
     xlab='Fitted',
     ylab = 'Residuals',
     main='Residual Plot')
abline(h=0,col=2)

summary(lm(sqrt(abs(residuals(lmod5)))~fitted(lmod5)))
# p-value is 0.992>0.05. We retain the null hypothesis of homoscedasticity and
# work with model 8.
qqnorm(resid(lmod5));qqline(resid(lmod5))
# looks normal
shapiro.test(residuals(lmod5))
# p-value is 0.0611>0.05. We retain the null hypothesis of normality and conclude
# that the residuals are normally distributed.


# Checking Collinearity ########################################################
X_1=model.matrix(lmod5)[,-1]
cor(X_1)
# No significant correlation in the model
vif(lmod5)
# Low VIFs for all predictors


# Conclusion ###################################################################
# making prediction on synthetic dataset with the best model

# Best model: lmod5
coef(lmod5)
# fatal = -.192 +.248*unemp+.000395*income-.647*spirits+.920*beertax + eps
(synthetic_data=data.frame(unemp=c(4.5,4.5,4.5,9),
                           income=c(14000,20000,14000,14000),
                           spirits=c(2.5,2.5,4.5,2.5),
                           beertax=c(.25,.15,.15,.15)))

pred2=predict(lmod5,synthetic_data,interval = 'pred')
exp(pred2[1,])
# For the first observation the 95% PI is (44, 578) and the fitted value is
# 159.
# We are 95% confident that the states with a 4.5% unemployment, $14,000 income 
# per person, 2.5% spirits consumption, and 0.25% tax on case of beer will have 
# a total number of vehicle fatalities between 44 and 578.

exp(pred2[2,])
# For the second observation, the 95% PI is (426, 5702) and the fitted value is 
# 1558.
# We are 95% confident that the states with a 4.5% unemployment, $20,000 income 
# per person, 2.5% spirits consumption, and 0.15% tax on case of beer will have 
# a total number of vehicle fatalities between 63 and 5439.

exp(pred2[3,])
# For the second observation, the 95% PI is (11, 150) and the fitted value is 
# 40.
# We are 95% confident that the states with a 4.5% unemployment, $14,000 income 
# per person, 4.5% spirits consumption, and 0.15% tax on case of beer will have 
# a total number of vehicle fatalities between 11 and 150.

exp(pred2[4,])
# For the second observation, the 95% PI is (122, 1601) and the fitted value is 
# 443.
# We are 95% confident that the states with a 9.0% unemployment, $14,000 income 
# per person, 2.5% spirits consumption, and 0.15% tax on case of beer will have 
# a total number of vehicle fatalities between 122 and 1601.

