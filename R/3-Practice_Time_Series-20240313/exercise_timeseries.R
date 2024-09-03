###############################
#######################
#Dynamic Model, basics 
#######################
#################################
install.packages('dynlm')

library('zoo')
library('dynlm')

splevel<-read.csv("/Users/lyndonbin/Downloads/VS Code/R/Practice_Time_Series-20240313/splevel.csv")

sp<-ts(splevel$spindx,start=20070103)

plot(sp)
acf(sp)



#define stock return
ret<-log(sp)-log(lag(sp,-1))

#AR1 model 
model1<-dynlm(ret~lag(ret,-1))


#################################################################
#it may be that the expected value of the return at time t, given
#past returns, is a quadratic function of return_{t-1}. To check this possibility, use the
#data in splevel.csv to estimate
#ret_t = b0 + b1ret_{t-1} + b2ret_{t-1}^2 + u_t;
#report the results in standard form.
##################################################

ret2<-ret^2



###########################################################################
#2) State and test the null hypothesis that E(ret_t|ret_{t-1}) does not depend on
# ret_{t-1}.(Hint: There are two restrictions to test here.) What do you conclude?
#######################################################################




###############################################################
#3) Drop ret_t from the model, but add the interaction term
#ret_{t-1} ret_{t-2}. Now test the efficient markets hypothesis.
##############################################################

ret12<-ret*lag(ret,-1)


#####################################################
#4) What do you conclude about predicting daily stock returns based on past stock
#returns?
###########################################################
  
  



################################
#Motivating Unit-Root Tests
###############################

x<-y<-list()
xtemp<-ytemp<-1 

for (i in 1:100){
  xtemp<-xtemp+rnorm(1)
  ytemp<-ytemp+rnorm(1,0,0.5)
  x<-append(x,xtemp)
  y<-append(y,ytemp)
}

tempmodel<-lm(unlist(y)~unlist(x))




#######################
#Unit-Root Tests
########################

adf<- function(x,k = 0, int = TRUE, trend = FALSE){
  # NB:  returns conventional lm summary so p-values for adf test are wrong!
  require(dynlm)
  dx <- diff(x)
  formula <- paste("dx ~ L(x)")
  if(k > 0)
    formula <- paste(formula," + L(dx,1:k)")
  if(trend){
    s <- time(x)
    t <- ts(s - s[1],start = s[1],freq = frequency(x))
    formula <- paste(formula," + t")
  }
  if(!int) formula <- paste(formula," - 1")
  summary(dynlm(as.formula(formula)))
}




########################
#Serial Correlation
######################

#Durbin-Watson Test
library(lmtest)
dwtest(model1)


#Breusch-Godfrey Test

model_bg<-lm(ret~lag(ret,-1)) #Run an OLS in your original equation:
uhat<-model_bg$resid  #Obtain the estimated residuals:
uhat<-ts(uhat,start=20070104) #Regress the estimated residuals (uhat) on the explanatory variables of the original model and lagged residuals (L.uhat). Call this the auxiliary regression.

model_adj<-dynlm(uhat~lag(uhat,-1)+lag(ret,-1))
R2<-summary(model_adj)$r.squared
R2*3018  #test statistic, should follow chi-squared distribution.


  
################
#Heteroskedasticity
##################

####White Test


#Run the regression

#Get the residuals

#Generate squared residuals

#Generate new explanatory variables, in the form of the squares of the explanatory variables and the cross-product of the explanatory variables:

#Regress the squared residuals into a constant, the original explanatory variables, and the set of auxiliary explanatory variables (squares and cross-products) you've just created:

#Get the sample size (N) and the R-squared (R2), and construct the test statistic N*R2:



####Breusch-Pagan





