

install.packages("AER")
install.packages("MatchIt")
install.packages("sampleSelection")


library(AER)
library(MatchIt)
library(sampleSelection)

# Load the dataset (assuming it is in CSV format)

macro_merged<- read.csv("/Users/lyndonbin/Downloads/VS Code/R/5-Endogeneity Exercise-20240314/macro_merged.csv")


#################################
# Preliminary data preparation
################################

macro_merged$CPIAUCSL<-ts(macro_merged$CPIAUCSL)

                                                                            
###################
#############################
#Question 1 
############################
######################
############
#################################
# (i) Estimate the OLS regression
######################################

ols_model <- lm(T10Y3M ~ CPIchange_annual, data=macro_merged)
summary(ols_model)

#change to "CPIAUCSL"
ols_model <- lm(T10Y3M ~ CPIAUCSL, data=macro_merged)
summary(ols_model)

##################################################################
# (ii) Re-estimate using CPIAUCSL lagged by one period as an IV
####################################################################
iv_model <- ivreg(T10Y3M ~ CPIchange_annual | lag(CPIchange_annual, -1), data=macro_merged)
summary(iv_model,  df=Inf, diagnostics = TRUE)

#change to "CPIAUCSL"
iv_model <- ivreg(T10Y3M ~ CPIAUCSL | lag(CPIAUCSL, -1), data=macro_merged)
summary(iv_model,  df=Inf, diagnostics = TRUE)

###########################################################
# (iii) First difference the equation and estimate by OLS
######################################################

macro_merged$diff_T10Y3M <- c(NA,diff(macro_merged$T10Y3M))
macro_merged$diff_CPIchange<- c(NA,diff(macro_merged$CPIchange_annual))

diff_ols_model <- lm(diff_T10Y3M ~ diff_CPIchange, data=macro_merged)
summary(diff_ols_model)

names(macro_merged)

####change to "CPIAUCSL"
macro_merged$diff_T10Y3M <- c(NA,diff(macro_merged$T10Y3M))
macro_merged$diff_CPIAUCSL<- c(NA,diff(macro_merged$CPIAUCSL))

diff_ols_model <- lm(diff_T10Y3M ~ diff_CPIAUCSL, data=macro_merged)
summary(diff_ols_model)



##############################################################################
# (iv) Discuss the possibility of using the lagged first difference as an IV
#################################################################
# Check the correlation between the lagged first difference of CPIAUCSL and the current first difference

macro_merged$diff_CPIchange<-ts(macro_merged$diff_CPIchange)
macro_merged<-macro_merged[-1,]

cor(macro_merged$diff_CPIchange[-1], macro_merged$diff_CPIchange[-(dim(macro_merged)[1])])

###change to "CPIAUCSL"
macro_merged$diff_CPIAUCSL<-ts(macro_merged$diff_CPIAUCSL)
macro_merged<-macro_merged[-1,]

cor(macro_merged$diff_CPIAUCSL[-1], macro_merged$diff_CPIAUCSL[-(dim(macro_merged)[1])])


#######################
################################
### Question 2 
############################### 

##############################
# (i) & (ii) 2SLS estimation
#############################

iv_formula <- GDPC1 ~ T10Y3M | CPIAUCSL + CPIchange_annual  #(enter your instrument_variable)
iv_model <- ivreg(iv_formula, data=macro_merged)
summary(iv_model)


#################################
# (iii) Test for endogeneity
##############################

waldtest(iv_model, test = "Wu")


#######################
############################
###Question 3
#########################
####################


# Define the binary treatment variable based on UNRATE
high_unemployment_threshold <- 6 # Example threshold
macro_merged$treatment <- ifelse(macro_merged$UNRATE > high_unemployment_threshold, 1, 0)

# Estimate propensity scores and perform matching
psm_formula <- treatment ~ INDPRO + CPIAUCSL + GDPC1
psm_model <- matchit(psm_formula, data= macro_merged, method = "nearest")

# Evaluate the effect of high unemployment on T10Y3M using matched sample
matched_data <- match.data(psm_model)
ate <- with(matched_data, t.test(T10Y3M[treatment == 1], T10Y3M[treatment == 0]))
ate

############# change 
high_unemployment_threshold <- 6 # Example threshold
macro_merged$treatment <- ifelse(macro_merged$UNRATE > high_unemployment_threshold, 1, 0)
psm_formula <- treatment ~ INDPRO + CPIAUCSL + GDPC1
psm_model <- matchit(psm_formula, data= macro_merged, method = "nearest")
matched_data <- match.data(psm_model)
ate <- with(matched_data, t.test(T10Y3M[treatment == 1], T10Y3M[treatment == 0]))
ate
#############

#################################
#############################################
### Question 4
###################################################
###################################

# Assuming that 'selection_variable' is a variable that influences the selection process but not T10Y3M
selection_equation <- treatment ~ UNRATE + CPIAUCSL + GDPC1
outcome_equation <- INDPRO ~ T10Y3M


# Heckman two-step estimation
probit<-glm(selection_equation, data=macro_merged, family=binomial(link="probit"))
summary(probit)

macro_merged$lambda <- dnorm(cbind(1,macro_merged$UNRATE, macro_merged$CPIAUCSL,macro_merged$GDPC1)%*%(probit$coef))/pnorm(cbind(1,macro_merged$UNRATE, macro_merged$CPIAUCSL,macro_merged$GDPC1)%*%(probit$coef))

macro_merged_nk<-macro_merged[which(macro_merged$treatment==1),]
lm<-lm(INDPRO ~ T10Y3M+lambda, data=macro_merged_nk) 
summary(lm)



