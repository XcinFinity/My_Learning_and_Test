
install.packages('MASS')
install.packages('plm')

library(MASS)
library(plm)


greene<-read.table("/Users/lyndonbin/Downloads/VS_Code/R/4-Panel_Data_Exercise-20240201/greene.csv", header=T,sep=',')


#################
#Pooled OLS
####################
attach(greene)

pols<-lm(log(Cost)~log(Output)) 
summary(pols)
 
detach(greene)


################
################
#FIXED EFFECTS##
################
################

###########################################
#PQ: a function calculates time-demeaned data
#############################################
PQ <-function(h, id){
  if(is.vector(h)) h <- matrix(h, ncol = 1)
  Ph <- unique(id)
  Ph <- cbind(Ph, table(id))
  for(i in 1:ncol(h)) Ph <- cbind(Ph, tapply(h[, i], id, mean))
  is <- tapply(id, id)
  Ph <- Ph[is,  - (1:2)]
  Qh <- h - Ph
  list(Ph=as.matrix(Ph), Qh=as.matrix(Qh), is=is)
}  

######################################
#within estimator
####################################
attach(greene)

lnc_we<-PQ(log(Cost),Firm)$Qh 
lny_we<-PQ(log(Output),Firm)$Qh

within<-lm(lnc_we~lny_we-1) 
summary(within)

detach(greene)

###################################
#within estimator, using plm package
####################################

within_plm<-plm(log(Cost)~log(Output), model="within",data=greene, index = c("Firm","Year"))
summary(within_plm)

########################################
#Recovering Alphas with dummy variables
##########################################

attach(greene)

summary(lm(log(Cost) ~ log(Output) + D1 + D2 + D3 + D4 + D5 + D6 - 1))

detach(greene)


###################################
#between estimator
##################################

attach(greene)

lnc_be<-PQ(log(Cost),Firm)$Ph 
lny_be<-PQ(log(Output),Firm)$Ph
between<-lm(lnc_be~lny_be) 
summary(between) 

detach(greene)


####################################
#between estimator, using plm package
######################################

between_plm<-plm(log(Cost)~log(Output), data=greene, model="between",  index = c("Firm","Year"))
summary(between_plm)    


################
################
#RANDOM EFFECTS#
################
################

random_plm<-plm(log(Cost)~log(Output), data=greene, model="random",  index = c("Firm","Year"))
summary(random_plm)

###################
#lambda calculation(by hand)
#####################

eu<-within$residual - mean(within$residual)
eb<-between$residual

sig_u2<-var(eu)
sig_a2<-var(eb)- 1/4*var(eu)

lambda<- 1-sqrt(sig_u2/(sig_u2+4*sig_a2))

attach(greene)

lnc_G<- log(Cost)-lambda*PQ(log(Cost), Firm)$Ph
lny_G<- log(Output)-lambda*PQ(log(Output), Firm)$Ph
gls<- lm (lnc_G~lny_G)
summary(gls)

detach(greene)

####################################################
#GLS a Combination of Within and Between Estimators
####################################################

Bb<-coef(summary(between_plm))[2,"Estimate"]
Vb<-vcov(between_plm)[2,2]
Bw<-coef(summary(within_plm))[,"Estimate"]
Vw<-vcov(within_plm)

beta_gls<-((1/Vb)+(1/Vw))^(-1)*(Bb/Vb+Bw/Vw)
beta_gls


###########################
#############################
#Hausman Test
##############################
##############################

w1 <- coef(within_plm)-coef(random_plm)[2]
V1 <- vcov(within_plm) - vcov(random_plm)[2,2]
H1 <- (w1^2)/V1  
H1


#################################
#Hausman Test with plm package
#################################

phtest(within_plm, random_plm)



