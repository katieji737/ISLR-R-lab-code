##Lab: Linear Regression
library(MASS)
library(ISLR)
names(Boston)
?Boston
plot(medv~lstat,Boston)
#simple linear regression
fit1 = lm(medv~lstat,data = Boston)
fit1
summary(fit1)
abline(fit1,col = "red")
names(fit1)
confint(fit1)
predict(fit1,data.frame(lstat = c(5,10,15)),interval = "confidence")
#multiple linear regression
fit2 = lm(medv~lstat+age, data = Boston)
summary(fit2)
fit3 = lm(medv~., data = Boston)
summary(fit3)
par(mfrow = c(2,2))
plot(fit3)
fit4 = update(fit3,~.-age-indus)
summary(fit4)
fit5 = lm(medv ~lstat*age,Boston)
summary(fit5)
fit6 = lm(medv~lstat+I(lstat^2),Boston);summary(fit6)
?I
attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)
points(lstat,fitted(fit6),col="red",pch=20)
?points
#pch: plotting character looks like round ball
fit7 = lm(medv~poly(lstat,4))
?poly
points(lstat,fitted(fit7),col="blue",pch=20)
plot(1:20,1:20,pch=1:20,cex=2)
names(Carseats)
summary(Carseats)
fit1 = lm(Sales ~ . +Income:Advertising+Age:Price,Carseats)
summary(fit1)
contrasts(Carseats$ShelveLoc)
regplot = function(x,y,...){
  fit = lm(y~x)
  plot(x,y,...)
  abline(fit,col = "red")
}
attach(Carseats)
regplot(Price,Sales,xlab="Price",ylab="Sales",
        col ="blue",pch=20)

##logistic regression
require(ISLR)
names(Smarket)
summary(Smarket)
?Smarket
pairs(Smarket,col=Smarket$Direction)
glm.fit = glm(Direction ~ Lag1+Lag2+Lag3+Lag4
              +Lag5+Volume,data = Smarket,family = binomial)

summary(glm.fit)
glm.probs = predict(glm.fit,type="response")
glm.probs[1:5]
glm.pred = ifelse(glm.probs>0.5,"Up","Down")
attach(Smarket)
table(glm.pred,Direction)
mean(glm.pred==Direction)
train=Year < 2005
glm.fit = glm(Direction ~ Lag1+Lag2+Lag3+Lag4
              +Lag5+Volume,data = Smarket,
              family = binomial,subset = train)
glm.probs = predict(glm.fit,newdata = Smarket[!train,],type="response")
glm.pred = ifelse(glm.probs>0.5,"Up","Down")
Direction.2005 = Smarket$Direction[!train]
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)

##linear discriminant analysis
lda.fit=lda(Direction~Lag1+Lag2,data = Smarket,subset = train)
lda.fit
plot(lda.fit)
Smarket.2005 = subset(Smarket,Year=2005)
lda.pred = predict(lda.fit,Smarket.2005)
lda.pred[1:5,]
class(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class,Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)

##K-Nearest Neighbors
library(class)
Xlag = cbind(Lag1,Lag1)
knn.pred = knn(Xlag[train,],Xlag[!train,],Direction[train],k=1)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])

##cross-validation
require(boot)
plot(mpg~horsepower,data = Auto)
#LOOCV
glm.fit = glm(mpg~horsepower,data = Auto)
cv.glm(Auto,glm.fit)$delta#1st:raw one 2nd:bias-corrected
loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}
loocv(glm.fit)
cv.error=rep(0,5)#set up a vector to collect errors
degree=1:5
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d),data = Auto)
  cv.error[d]=loocv(glm.fit)
}
plot(degree,cv.error,type = "b")
#10-fold cv
cv.error10=rep(0,5)
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d),data = Auto)
  cv.error10[d]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type = "b",col="red")

##The bootstrap - re-sample from the training observations
alpha = function(x,y){
  vx=var(x)
  vy=var(y)
  cxy=cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}
alpha(Portfolio$X,Portfolio$Y)
alpha.fn=function(data,index){
  with(data[index,],alpha(X,Y))#using the data in the dataframe to fit the second command
}
alpha.fn(Portfolio,1:100)
set.seed(1)
alpha.fn(Portfolio,sample(1:100,100,replace=TRUE))
boot.out=boot(Portfolio,alpha.fn,R=1000)
boot.out
plot(boot.out)

## Model Selection
Hitters = na.omit(Hitters)
with(Hitters,sum(is.na(Salary)))
#Best subset selection
library(leaps)#package can evaluate all the best-subset models
regfit.full = regsubsets(Salary~.,data = Hitters,nvmax = 19)
summary(regfit.full)
reg.summary=summary(regfit.full)
names(reg.summary)
#cp is an estimate of predcition error
plot(reg.summary$cp,xlab = "Number of variables",ylab = "cp")
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],pch=20,col="red")

plot(regfit.full,scale = "Cp")
coef(regfit.full,10)

#Forward Stepwise Selection
regfit.fwd=regsubsets(Salary~.,data = Hitters,nvmax = 19,method = "forward") #nvmax =19 means that we use full number variables
summary(regfit.fwd)
plot(regfit.fwd,scale = "Cp")
dim(Hitters)
set.seed(1)
train=sample(seq(263),180,replace = TRUE)#sample 180 index
regfit.fwd=regsubsets(Salary~.,data = Hitters[train,],nvmax = 19,method = "forward")
val.errors=rep(NA,19)#set up a vector having 19 slots
x.test=model.matrix(Salary~.,data = Hitters[-train,])#test model matrix
for (i in 1:19){
  coefi=coef(regfit.fwd,id=i)
  pred=x.test[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[-train]-pred)^2)
}
plot(sqrt(val.errors),ylab = "Root MSE",ylim = c(300,400),pch = 19, type = "b")
points(sqrt(regfit.fwd$rss[-1]/180),col="blue",pch=19,type="b")#-1 means exclude validation
legend("topright",legend = c("Training","Validation"),col = c("blue","black"))

predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  mat[,names(coefi)]%*%coefi
}

#Model selection by cross-validation
set.seed(1)
folds=sample(rep(1:10,length=nrow(Hitters)))#each observation will be assigned to a fold
table(folds)                                 
cv.errors=matrix(NA,10,19)#19 subsets 10 rows
for (k in 1:10){
  best.fit= regsubsets(Salary~.,data = Hitters[folds!=k,],nvmax=19,method="forward")
  for (i in 1:19){
    pred=predict(best.fit,Hitters[folds==k,],id=i)
    cv.errors[k,i]=mean((Hitters$Salary[folds==k]-pred)^2)
  }
}
rmse.cv=sqrt(apply(cv.errors,2,mean))
plot(rmse.cv,pch=19,type="b")

# Ridge and Lasso
library(glmnet)
x=model.matrix(Salary~.-1,data=Hitters)
y=Hitters$Salary
fit.ridge=glmnet(x,y,alpha = 0)
plot(fit.ridge,xvar = "lambda",label = TRUE)
cv.ridge=cv.glmnet(x,y,alpha=0)
plot(cv.ridge)

fit.lasso=glmnet(x,y)
plot(fit.lasso,xvar = "lambda",label = TRUE)
cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)
coef(cv.lasso)

lasso.tr = glmnet(x[train,],y[train])
lasso.tr
pred=predict(lasso.tr,x[-train,])
dim(pred)
rmse=sqrt(apply((y[-train]-pred)^2, 2, mean))
plot(log(lasso.tr$lambda),rmse,type = "b",xlab = "log(lambda)")
lam.best=lasso.tr$lambda[order(rmse)[1]]
lam.best
coef(lasso.tr,s=lam.best)

## Nonlinear Models
library(ISLR)
attach(Wage)
#Polynomials
fit=lm(wage~poly(age,4),data = Wage)
summary(fit)
#make a plot of the fitted function, along with the standard errors of the fit
agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata = list(age=age.grid),se=TRUE)#we want standart error
se.bands=cbind(preds$fit+2*preds$se,preds$fit-2*preds$se)
plot(age,wage,col="darkgrey")
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,col = "blue",lty=2)

fita=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data = Wage)#I()is a wrapper to protect age^2
summary(fita)#different basis for polynominals->different coef and p-value
plot(fitted(fit),fitted(fita)) #the fit is the same

fita=lm(wage~education,data = Wage)
fitb=lm(wage~education+age,data = Wage)
fitc=lm(wage~education+poly(age,2),data = Wage)
fitd=lm(wage~education+poly(age,3),data = Wage)
anova(fita,fitb,fitc,fitd)3#model 2,3 is certainly needed.

#Polynomial logistic regression
fit=glm(I(wage>250)~poly(age,3),data = Wage,family = binomial)
summary(fit)
preds=predict(fit,list(age=age.grid),se=T)
se.bands=preds$fit+cbind(fit=0,lower=-2*preds$se,upper=2*preds$se)
se.bands[1:5,]
#if you are in markdown, you can use LaTeX to embed equation
prob.bands=exp(se.bands)/(1+exp(se.bands))#return a matrix
matplot(age.grid,prob.bands,col = "blue",lwd = c(2,1,1),lty = c(1,2,2),type = "l",ylim = c(0,1))
points(jitter(age),I(wage>250)/10,pch="|",cex=.5)#jitter:give a sense of density

#Splines-more flexible than polynomials,but the idea is rather similar
require(splines)
fit=lm(wage~bs(age,knots = c(25,40,60)),data = Wage)
#cubic polynomials in each of the regions, but they constrain to
#be continuous at the knots and they constrain to have continuous first
#and second derivatives, which makes them really smooth
plot(age,wage,col="darkgrey")
lines(age.grid,predict(fit,list(age=age.grid)),col="darkgreen",lwd=2)
abline(v=c(25,40,60),lty=2,col="darkgreen")#specify where the knot is 

#the smoothing splines does not require knot selection, but it does
#have a smoothing parameter,which can conveniently be specified via 
#the effective degrees of freedom.
fit=smooth.spline(age,wage,df=16)
lines(fit,col="red",lwd=2)

#or use LOO cv to select smoothing parameter
fit=smooth.spline(age,wage,cv=T)
lines(fit,col="purple",lwd=2)

#Generalized Additive Models - works for multiple nonlineat terms
require(gam)
gam1=gam(wage~s(age,df=4)+s(year,df=4)+education,data = Wage)
par(mfrow=c(1,3))
plot(gam1,se=T)
gam2=gam(I(wage>250)~s(age,df=4)+s(year,df=4)+education,data = Wage,family = binomial)
plot(gam2)

gam2a=gam(I(wage>250)~s(age,df=4)+year+education,data = Wage,family = binomial)
anova(gam2a,gam2)

#nice plot feature of gam package,also works for lm and glm
par(mfrow=c(1,3))
lm1=lm(wage~ns(age,df=4)+ns(year,df=4)+education,data = Wage)
plot.Gam(lm1,se=T)

## Decision Trees
require(ISLR)
require(tree)
attach(Carseats)
hist(Sales)
High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats,High)
tree.carseats=tree(High~.-Sales,data = Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty = 0)
tree.carseats

set.seed(1011)
train=sample(1:nrow(Carseats),250)
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
plot(tree.carseats);text(tree.carseats,pretty = 0)
tree.pred=predict(tree.carseats,Carseats[-train,],type = "class")
with(Carseats[-train,],table(tree.pred,High))
cv.carseats=cv.tree(tree.carseats,FUN = prune.misclass)
cv.carseats
plot(cv.carseats)
prune.carseats=prune.misclass(tree.carseats,best = 13)
plot(prune.carseats);text(prune.carseats,pretty = 0)
tree.pred=predict(prune.carseats,Carseats[-train,],type = "class")
with(Carseats[-train,],table(tree.pred,High))

## Random Forests
require(randomForest)
require(MASS)
set.seed(101)
dim(Boston)
train=sample(1:nrow(Boston),300)
rf.boston = randomForest(medv~.,data = Boston,subset = train)
rf.boston#gives you out-of-bag mean of squared residuals
#parameter 'mtry'-e.g.4 which is the number of variables randomly chosen at each split
obb.err=double(13)#to record errors
test.err=double(13)
for (mtry in 1:13){
  fit = randomForest(medv~.,data=Boston,subset=train,mtry=mtry,ntree=400)
  obb.err[mtry]=fit$mse[400]
  pred=predict(fit,Boston[-train,])
  test.err[mtry]=with(Boston[-train,],mean((medv-pred)^2))
  cat(mtry," ")#print out we try mtry
} 
matplot(1:mtry,cbind(test.err,obb.err),pch = 19,col = c("red","blue"),type ="b",
        ylab="Mean Squared Error")#notice that points at the end with "mtry=13" correspond to bagging.
legend("topright",legend =c("OOB","Test"),pch=19,col=c("red","blue"))

## Boosting trees
require(gbm)
boost.boston=gbm(medv~.,data = Boston[train,],distribution = "gaussian",n.trees = 10000,
                 shrinkage = 0.01,interaction.depth = 4)#it will stop after 4 splits
summary(boost.boston)
plot(boost.boston,i="lstat")
plot(boost.boston,i="rm")

n.trees=seq(from=100,to=10000,by=100)
predmat=predict(boost.boston,newdata = Boston[-train,],n.trees = n.trees)
dim(predmat)#206 test observations
berr=with(Boston[-train,],apply((predmat-medv)^2, 2, mean))
plot(n.trees,berr,pch=19,ylab = "Mean Squared Error",xlab = "# Trees",
     main = "Boosting Test Error")
abline(h=min(test.err),col="red")#include the best test error form random forest

## SVM
#linear SVM classifier
set.seed(10111)
x=matrix(rnorm(40),20,2)#normly distributed:make 20 observations in two classes on two variables
y=rep(c(-1,1),c(10,10))
x[y==1,]=x[y==1,]+1#move the mean from 0 to 1
plot(x,col=y+3,pch=19)#character 19 nice colored
library(e1071)
dat=data.frame(x,y=as.factor(y))
#we want linear for the support vector classifier
#tuning parameter is the cost;scale=false: not to standardize the variables
svmfit=svm(y~.,data = dat,kernel="linear",cost=10,scale = FALSE)
print(svmfit)
plot(svmfit,dat)#crude plot
make.grid=function(x,n=75){#75*75 grid
  grange=apply(x, 2, range)
  x1=seq(from=grange[1,1],to=grange[2,1],length=n)
  x2=seq(from=grange[1,2],to=grange[2,2],length=n)
  expand.grid(X1=x1,X2=x2)
  }
 xgrid=make.grid(x)
 ygrid=predict(svmfit,xgrid)
 plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],pch=20,cex=.2)
 points(x,col=y+3,pch=19)  
 points(x[svmfit$index,],pch=5,cex=2)
#we extract the linear coefficients and then using simple algebra,
#we include the decision boundary and the two margins
beta=drop(t(svmfit$coefs)%*%x[svmfit$index,])
beta0=svmfit$rho
plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],pch=20,cex=.2)
points(x,col=y+3,pch=19)  
points(x[svmfit$index,],pch=5,cex=2)
abline(beta0/beta[2],-beta[1]/beta[2])#boundary
abline((beta0-1)/beta[2],-beta[1]/beta[2],lty=2)#upper margin
abline((beta0+1)/beta[2],-beta[1]/beta[2],lty=2)

#Nonlinear SVM
load(url("http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/ESL.mixture.rda"))
names(ESL.mixture)
rm(x,y)
attach(ESL.mixture)
plot(x,col=y+1)
dat=data.frame(y=factor(y),x)
fit=svm(factor(y)~.,data=dat,scale=FALSE,kernel="radial",cost=5)
#create grid as before;but these data have the grid points for each variable included
#on the data frame: px1,px2 see from the names
xgrid=expand.grid(X1=px1,X2=px2)
ygrid=predict(fit,xgrid)
plot(xgrid,col=as.numeric(ygrid),pch=20,cex=.2)
points(x,col=y+1,pch=19)

#https://www.youtube.com/watch?v=L3n2VF7yKkk&index=6&list=PL5-da3qGB5IDl6MkmovVdZwyYOhpCxo5o
func=predict(fit,xgrid,decision.values = TRUE)
func=attributes(func)$decision
xgrid=expand.grid(X1=px1,X2=px2)
ygrid=predict(fit,xgrid)
plot(xgrid,col=as.numeric(ygrid),pch=20,cex=.2)
points(x,col=y+1,pch=19)
contour(px1,px2,matrix(func,69,99),level=0,add=TRUE)
contour(px1,px2,matrix(prob,69,99),level=.5,add=TRUE,col = "blue",lwd = 2)

## Principal Components
dimnames(USArrests)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)
pca.out=prcomp(USArrests,scale=TRUE)
pca.out
names(pca.out)
biplot(pca.out,scale = 0,cex=.6)

## K-means clustering
set.seed(101)
x=matrix(rnorm(100*2),100,2)
xmean=matrix(rnorm(8,sd=4),4,2)#4 rows and 2 columns
which=sample(1:4,100,replace = TRUE)
x=x+xmean[which,]
plot(x,col=which,pch=19)
km.out=kmeans(x,4,nstart = 15)
km.out
plot(x,col=km.out$cluster,cex=2,pch=1,lwd=2)
points(x,col=which,pch=19)
points(x,col=c(4,3,2,1)[which],pch=19)

## Hierarchical Clustering
hc.complete=hclust(dist(x),method = "complete")#use the largest distance
plot(hc.complete)
hc.single=hclust(dist(x),method = "single")#use the smallest distance
plot(hc.single)
hc.average=hclust(dist(x),method = "average")
plot(hc.average)
#cut the tree
hc.cut=cutree(hc.complete,4)
table(hc.cut,which)
table(hc.cut,km.out$cluster)
plot(hc.complete,labels = which)
