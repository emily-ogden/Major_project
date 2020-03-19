##Major Project BI612##
##Emily Ogden##
##March 16, 2020##

#library
library(stats)
library(vegan) 
library(car)
library(MASS)
library(moments)
library(ggplot2)
library(ggfortify)
library(bestNormalize)
library(dplyr)
library(caret)
library(effects)
#import data
fire=read.csv("All_site_edited_2020.csv")

#build full model
mod1=lm(burn.depth~moisture+slope+Dom+age,data=fire)

####Check assumptions####
#normality
shapiro.test(mod1$residuals)
#visualize assumptions
autoplot(mod1,c(1,2,3,6))

vif(mod1)
#try to transform data
best=bestNormalize(fire$burn.depth)
best
trans=sqrt_x(fire$burn.depth, standardize = T)
trans
#re-test normality with transformed data
mod2=lm(trans$x~moisture+slope+Dom+age,data=fire)
shapiro.test(mod2$residuals)

####Model selection####
mod3=step(mod1)
summary(mod1)
summary(mod3)
plot(allEffects(mod3))

####K-fold cross validation####
ctrl=trainControl(method="cv",number=10)
model_caret=train(burn.depth~moisture+slope+Dom+age,data=fire,trControl=ctrl,method="lm")
model_caret
model_caret$finalModel
