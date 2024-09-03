import delimited using "C:\Users\shenru\Dropbox\Teaching\202402\Research Methodology\eTA\greene.csv", clear 

gen lnc=log(cost) 
gen lny=log(output)

xtset firm year

********************
*pooled OLS;
*********************
reg lnc lny 

**********************************
*Fixed effect, within estimator
******************************

xtreg lnc lny, fe 


*within estimator (time-demeaned data calculation)
egen lnc_bar = mean(lnc), by(firm)
egen lny_bar = mean(lny), by(firm)
gen lnc_we = lnc-lnc_bar
gen lny_we = lny-lny_bar
reg lnc_we lny_we

**********************
*Between estimator
************************
xtreg lnc lny, be 

*between estimator by hand:
reg lnc_bar lny_bar


*****************
*Random Effects
********************
xtreg lnc lny, re


**************************
*Hausman Test
****************************

xtreg lnc lny, fe 
estimates store fe 
xtreg lnc lny, re 
estimates store re
hausman fe re




