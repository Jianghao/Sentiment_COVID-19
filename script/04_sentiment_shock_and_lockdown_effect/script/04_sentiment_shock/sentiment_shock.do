clear all
set more off

* set scheme, font, and title size
set scheme s1color 
graph set window fontface default
global title "title(" ", size(vhuge))"
	
cd "~/Sentiment_COVID-19/"

* 1. RD regression
* ------------------------------------------------------------------------------

local nlp "bert"
local quan "mean"

import delim using input/`nlp'_world_admin1.csv, clear
drop if tweet_av<500 

vallist country
local values `r(list)'
foreach country in `values' {
preserve
capture{
	keep if country=="`country'"

	gen t = date(date, "YMD",2020)
	gen int year = year(t)
	gen byte month = month(t)
	gen byte day = day(t)
	gen byte dayofwk=dow(t)

	drop if t<date(drop_senti_bert_`quan',"YMD")-28
	drop if t>date(min_senti_bert_`quan',"YMD")+28
	drop if date=="2020-02-14"| t<date("2020-01-04","YMD",2020)

	sum t if donut_`quan'==0
	local a=r(min)
	local b=r(max)
	gen date_resid = t - date(min_senti_bert_`quan',"YMD")+1
	gen date_inter = date_resid*treat_`quan'
	drop if donut_`quan'==0
	egen admin1_t=group(admin1 t)
	egen admin1_id=group(admin1)

	* partial out day-of-week and admin1 FE
	xi i.dayofwk i.admin1 
	reg `quan' _I* [aw=n]
	predict sentiment_resid,resid
	sum `quan' [aw=n]
	replace sentiment_resid = sentiment_resid + r(mean)
	
	label var treat_`quan' "COVID"
	label var date_resid "Date"
	label var date_inter "Date × COVID"
	
	sum `quan' [aw=n] if treat_`quan'==0
	local mean_pre= r(mean)
	local sd_pre=r(sd)
	
	local cluster_dimension admin1_id 
	capture drop running
	summarize t	
	capture gen running=t-r(min)+1
	replace running=(t-r(min)+1)-(`b'-`a'+1) if t>`b'
	local tem=(`b'+1-r(min)+1)-(`b'-`a'+1)
	capture gen relative_run=running-`tem'
	
	* RD analysis: triangular kernel
	rdrobust sentiment_resid relative_run, c(0) p(1) weights(n) h(28) vce(cluster `cluster_dimension') kernel(triangular)
	outreg2 using pipeline/rdrobust/triangular_`nlp'_`quan'_`cluster_dimension'.xls, excel append label nocons addtext(DOW FE, YES, Country, `country',Pre-covid Mean,`mean_pre', Pre-covid SD, `sd_pre',kernel, triangular,bandwidth, 28d) addnote(Parentheses contain clustered standard errors that are robust to within-admin1 serial correlation. Triangular kernel is used.)

	* RD analysis: epanechnikov kernel
	rdrobust sentiment_resid relative_run, c(0) p(1) weights(n) h(28) vce(cluster `cluster_dimension') kernel(epanechnikov)
	outreg2 using pipeline/rdrobust/epanechnikov_`nlp'_`quan'_`cluster_dimension'.xls, excel append label nocons addtext(DOW FE, YES,  Country, `country',Pre-covid Mean,`mean_pre',Pre-covid SD, `sd_pre',kernel, epanechnikov,bandwidth, 28d) addnote(Parentheses contain clustered standard errors that are robust to within-admin1 serial correlation. Triangular kernel is used.)
	
	* RD analysis: uniform kernel (ITS equivalent)
	rdrobust sentiment_resid relative_run, c(0) p(1) weights(n) h(28) vce(cluster `cluster_dimension') kernel(uniform)
	outreg2 using pipeline/rdrobust/uniform_`nlp'_`quan'_`cluster_dimension'.xls, excel append label nocons addtext(DOW FE, YES,  Country, `country',Pre-covid Mean,`mean_pre',Pre-covid SD, `sd_pre',kernel, uniform,bandwidth, 28d) addnote(Parentheses contain clustered standard errors that are robust to within-admin1 serial correlation. Triangular kernel is used.)
	

	grstyle init
	grstyle set symbol Oh	
	rdplot sentiment_resid relative_run, c(0) p(1) weights(n) h(28) kernel(triangular) vce(cluster `cluster_dimension') graph_options(xlabel(-28(4)28) legend(off) xtitle("Relative Date") ytitle("Sentiment (`nlp')") ) 
	gr export pipeline/plot_rdrobust/rd_`nlp'_`quan'_`country'.pdf, as(pdf) replace

	}
restore
}



* 2. weekday and weekend variations
* ------------------------------------------------------------------------------

local nlp "bert"
local quan "mean"

import delim using input/`nlp'_world_admin1.csv, clear
drop if tweet_av<500 
vallist country
local values `r(list)'

foreach country in `values' {
preserve
	keep if country=="`country'"
	gen t = date(date, "YMD",2020)
	gen int year = year(t)
	gen byte month = month(t)
	gen byte day = day(t)
	gen byte dayofwk=dow(t)
	drop if date=="2020-02-14"| t<date("2020-01-04","YMD",2020)

	egen admin1_t=group(admin1 t)
	egen admin1_id=group(admin1)

	reghdfe `quan' i.dayofwk if t<date(drop_senti_bert_`quan',"YMD") , absorb(admin1_id) 
	outreg2 using pipeline/rdrobust/weekday_weekend.xls, excel append addtext(Country, `country')
	
restore
}



* 3. RD regression： Robustness checks (changing the days from 7 to 35)
* ------------------------------------------------------------------------------

local nlp "bert"
local quan "mean"
local days=35

import delim using input/`nlp'_world_admin1.csv, clear
drop if tweet_av<500 
vallist country
local values `r(list)'

foreach country in `values' {
preserve
capture{
	keep if country=="`country'"

	gen t = date(date, "YMD",2020)
	gen int year = year(t)
	gen byte month = month(t)
	gen byte day = day(t)
	gen byte dayofwk=dow(t)

	drop if t<date(drop_senti_bert_`quan',"YMD")-`days'
	drop if t>date(min_senti_bert_`quan',"YMD")+`days'
	drop if date=="2020-02-14"| t<date("2020-01-04","YMD",2020) 

	sum t if donut_`quan'==0
	local a=r(min)
	local b=r(max)
	gen date_resid = t - date(min_senti_bert_`quan',"YMD")+1
	gen date_inter = date_resid*treat_`quan'
	drop if donut_`quan'==0
	egen admin1_t=group(admin1 t)
	egen admin1_id=group(admin1)

	* partial out day-of-week and admin1 FE
	xi i.dayofwk i.admin1 
	reg `quan' _I* [aw=n]
	predict sentiment_resid,resid
	sum `quan' [aw=n]
	replace sentiment_resid = sentiment_resid + r(mean)
	
	label var treat_`quan' "COVID"
	label var date_resid "Date"
	label var date_inter "Date × COVID"
	
	sum `quan' [aw=n] if treat_`quan'==0
	local mean_pre= r(mean)
	local sd_pre=r(sd)
	
	local cluster_dimension admin1_id 
	capture drop running
	summarize t	
	capture gen running=t-r(min)+1
	replace running=(t-r(min)+1)-(`b'-`a'+1) if t>`b'
	local tem=(`b'+1-r(min)+1)-(`b'-`a'+1)
	capture gen relative_run=running-`tem'
	
	rdrobust sentiment_resid relative_run, c(0) p(1) weights(n) h(`days') vce(cluster `cluster_dimension') kernel(triangular)
	outreg2 using pipeline/rdrobust/robustness_check/triangular_`nlp'_`quan'_`days'.xls, excel append label nocons addtext(DOW FE, YES, Country, `country',Pre-covid Mean,`mean_pre', Pre-covid SD, `sd_pre',kernel, triangular,bandwidth, `days') addnote(Parentheses contain clustered standard errors that are robust to within-admin1 serial correlation. Triangular kernel is used.)

	}
restore
}


* 4. RD regression： Robustness checks (optimal bandwidth)
* ------------------------------------------------------------------------------

local nlp "bert"
local quan "mean"

import delim using input/`nlp'_world_admin1.csv, clear
drop if tweet_av<500 
vallist country
local values `r(list)'

foreach country in `values' {
preserve
capture{
	keep if country=="`country'"

	gen t = date(date, "YMD",2020)
	gen int year = year(t)
	gen byte month = month(t)
	gen byte day = day(t)
	gen byte dayofwk=dow(t)

	drop if date=="2020-02-14"| t<date("2020-01-04","YMD",2020) 

	sum t if donut_`quan'==0
	local a=r(min)
	local b=r(max)
	gen date_resid = t - date(min_senti_bert_`quan',"YMD")+1
	gen date_inter = date_resid*treat_`quan'
	drop if donut_`quan'==0
	egen admin1_t=group(admin1 t)
	egen admin1_id=group(admin1)

	* partial out day-of-week and admin1 FE
	xi i.dayofwk i.admin1 
	reg `quan' _I* [aw=n]
	predict sentiment_resid,resid
	sum `quan' [aw=n]
	replace sentiment_resid = sentiment_resid + r(mean)
	
	label var treat_`quan' "COVID"
	label var date_resid "Date"
	label var date_inter "Date × COVID"
	
	sum `quan' [aw=n] if treat_`quan'==0
	local mean_pre= r(mean)
	local sd_pre=r(sd)
	
	local cluster_dimension admin1_id 
	
	capture drop running
	summarize t	
	capture gen running=t-r(min)+1
	replace running=(t-r(min)+1)-(`b'-`a'+1) if t>`b'
	local tem=(`b'+1-r(min)+1)-(`b'-`a'+1)
	capture gen relative_run=running-`tem'
	
	rdrobust sentiment_resid relative_run, c(0) p(1) weights(n) bwselect(mserd) vce(cluster `cluster_dimension') kernel(triangular)
	outreg2 using pipeline/rdrobust/robustness_check/triangular_`nlp'_`quan'_mserd.xls, excel append label nocons addtext(DOW FE, YES, Country, `country',Pre-covid Mean,`mean_pre', Pre-covid SD, `sd_pre',kernel, triangular,bandwidth,` e(h_r)') addnote(Parentheses contain clustered standard errors that are robust to within-admin1 serial correlation. Triangular kernel is used.)

	}
restore
}
