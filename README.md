# PLS-HighDimension
Powerpoint and code for PLS and High Dimensional Regression


rm(list=ls())
library(pls)
library(ISLR)
library(MASS)

x=model.matrix(Grad.Rate~.,College)[,-1]
y=College$Grad.Rate

set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

# Partial Least Squares
pls.fit=plsr(Grad.Rate~., data=College,subset=train,scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
pls.pred=predict(pls.fit,x[test,],ncomp=4)
mean((pls.pred-y.test)^2)
pls.fit=plsr(Grad.Rate~., data=College,scale=TRUE,ncomp=4)
summary(pls.fit)
