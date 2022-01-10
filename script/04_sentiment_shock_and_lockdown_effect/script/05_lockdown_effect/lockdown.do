set scheme s1color 
graph set window fontface default
global title "title(" ", size(vhuge))"


clear all
cd "~/Sentiment_COVID-19/"

local nlp "bert"
local senti_quan "mean"
local senti_quan "quan75"
local type "loose"
local trend "std_trend"
local tweet_threshold=500


********************************
	** Data preparation
*********************************
import delimited input\lockdown\regv2_`type'_lockdown_`trend'_`nlp'.csv, varnames(1) encoding(ISO-8859-2) clear 

* delete the countries which we have no case data
destring cum_confirm, replace force
bysort iso3: egen case_merged = mean(cum_confirm)
drop if case_merged==.
drop if mean_trend=="NA"

* set up time variables
gen t = date(date, "YMD",2020)
lab var t "date"
gen dow = dow(t)
gen month = month(t)
gen year = year(t)
gen day = day(t)
encode iso3, gen(country_c)
duplicates tag country_c t, gen(dup)
drop if dup>0
duplicates report country_c t
drop if date=="2020-02-14"| t<date("2020-01-04","YMD",2020) 

gen lockdown_start_t = date(date_start, "YMD",2020)
gen lockdown_end_t = date(date_end, "YMD",2020)
gen lockdown_treat=(t>=lockdown_start_t & t<=lockdown_end_t)

* sentiment and cases variables： 7-day moving average
tsset country_c t, daily

* ------------------------------------------------------------------------------
* Synthetic control
* ------------------------------------------------------------------------------
destring pop_density_2018 percap_income_2018 polity2_index hospital_bed individuals_using_the_internet population urban_rate,replace force
destring average_n,replace force
keep if average_n>`tweet_threshold'

* use sentiment before Feb 15 as baseline (Feb 14 excluded), exclude countries with lots of sub-national mandates
drop if iso3=="CHN" | iso3=="USA"| iso3=="BRA"| iso3=="NGA"|iso3=="DEU"|iso3=="SRB"
* relative sentiment
gen baseline_date=0
	replace baseline_date=1 if t<date("2020-02-15","YMD",2020)
bysort iso3: egen baseline_sentiment=mean(mean) if baseline_date==1
bysort iso3: egen base_sentiment=mean(baseline_sentiment)
drop baseline_sentiment 
gen relative_sentiment= mean-base_sentiment
keep if t>=date("2020-02-15","YMD",2020) & t<date("2020-05-20","YMD",2020)
	
* Construct balanced panel
	* case data
bysort iso3: egen mean_case=mean(cum_confirm)
	drop if mean_case==.
	drop mean_case
*sentiment
bysort iso3: egen mean_sentiment=mean(mean)
	drop if mean_sentiment==.
	drop mean_sentiment
* case rate
gen case_rate=cum_confirm/(population/100000)
gen l_case_rate=log(case_rate+1)

*other covariates
gen l_pop_density=log(pop_density_2018)
	drop if l_pop_density==.
gen l_income=log(percap_income_2018)
	drop if l_income==.
drop if polity2_index==.
drop if hospital_bed==.
drop if lockdown_end_t-lockdown_start_t<7
gen internet_index=individuals_using_the_internet/10
gen urban_rate_index=urban_rate/10


/* 
   Main results ---------------------------------
*/
	
	* treatment preparation
vallist iso3 if lockdown_start_t!=. & lockdown_start_t<date("2020-05-01","YMD",2020)
local values `r(list)'
foreach country in `values'{
	preserve
			* treatment unit
		sum country_c if iso3=="`country'"
		local treatnum=`r(mean)'
			* treatment date
		sum lockdown_start_t if iso3=="`country'"
		local treatdate=`r(mean)'
		local predate=`treatdate'-1
		local predate2=`treatdate'-2
		local middate=`treatdate'-3
		local middate2=`treatdate'-5
		local middate3=`treatdate'-7
		local middate4=`treatdate'-14
			*result period
		sum t
		local mindate=`r(min)'
		local maxdate=`treatdate'+7
			* delete other treatment units
		*drop if lockdown_start_t!=. & iso3!="`country'" 
	    drop if lockdown_start_t<`treatdate'+7 & iso3!="`country'" 

		
		tsset country_c t 
		label var relative_sentiment "Relative sentiment change"
		label var t "Date"
		synth relative_sentiment l_income l_pop_density urban_rate_index polity2_index hospital_bed  internet_index l_case_rate(`predate') relative_sentiment(`predate') relative_sentiment(`predate2')  relative_sentiment(`middate') relative_sentiment(`middate2')  relative_sentiment(`middate3') relative_sentiment(`middate4')  ,trunit(`treatnum') trperiod(`treatdate') resultsperiod(`middate4'(1)`maxdate') keep(pipeline/lockdown/store/synth_`country'.dta) replace 
		
		*make the figures
		use pipeline/lockdown/store/synth_`country'.dta,clear
		drop if _time==.
		rename _time date
		rename _Y_treated  treat
		rename _Y_synthetic counterfact
		gen gap=treat-counterfact
		gen rel_date=date-`treatdate'
		gsort rel_date
		save pipeline/lockdown/store/synth_result_`country'.dta,replace

		twoway (line treat rel_date,lp(solid)lw(vmedthick)lcolor(maroon*.8)) (line counterfact rel_date,lp(solid)lw(vthin)lcolor(black)), yline(0, lpattern(solid) lw(vthin) lcolor(black)) xline(0, lpattern(shortdash) lcolor(black)) xtitle("Date relative to lockdown",si(medsmall)) xlabel(-14(3)7) ylabel(,format(%03.1f)) ytitle("Change in sentiment", size(medsmall)) legend(label(1 "`country'") label(2 "Synthetic `country'"))	
		graph export pipeline/lockdown/plots/synth_`country'.pdf, replace
	restore
}
	
/* 
   Inference: placebo test ---------------------------------
*/
	
vallist iso3 if lockdown_start_t!=. & lockdown_start_t<date("2020-05-01","YMD",2020)
	local values `r(list)'
	foreach country in `values'{
	di "`country' beginning"

	preserve
			* treatment unit
		quietly sum country_c if iso3=="`country'"
		local treatnum=`r(mean)'
			* treatment date
		quietly sum lockdown_start_t if iso3=="`country'"
		local treatdate=`r(mean)'
		local predate=`treatdate'-1
		local predate2=`treatdate'-2
		local middate=`treatdate'-3
		local middate2=`treatdate'-5
		local middate3=`treatdate'-7
		local middate4=`treatdate'-14
			*result period
		quietly sum t
		local mindate=`r(min)'
		local maxdate=`treatdate'+7
			* delete other treatment units
	   *drop if lockdown_start_t!=. & iso3!="`country'" 
	    drop if lockdown_start_t<`treatdate'+7 & iso3!="`country'" 
		
		vallist iso3 if iso3!="`country'"
		local controls `r(list)'
		local i=1
		foreach placebo_country in `controls'{
		di "`country' control `i'"

		* change treatment unit
		quietly sum country_c if iso3=="`placebo_country'"
		local treatnum=`r(mean)'
		
		quietly tsset country_c t 
		label var relative_sentiment "Relative sentiment change"
		label var t "Date"
		quietly synth relative_sentiment l_income l_pop_density urban_rate_index polity2_index hospital_bed  internet_index l_case_rate(`predate') relative_sentiment(`predate') relative_sentiment(`predate2')  relative_sentiment(`middate') relative_sentiment(`middate2')  relative_sentiment(`middate3') relative_sentiment(`middate4')  ,trunit(`treatnum') trperiod(`treatdate') resultsperiod(`middate4'(1)`maxdate') keep(pipeline/lockdown/store/synth_placebo_`country'_`i'.dta) replace  
		local i=`i'+1
		}
	restore
} 
	
	* output the results
vallist iso3 if lockdown_start_t!=. & lockdown_start_t<date("2020-05-01","YMD",2020)
local values `r(list)'
foreach country in `values'{	
	di "`country' beginning"
	preserve
			* treatment date
		quietly sum lockdown_start_t if iso3=="`country'"
		local treatdate=`r(mean)'
			* delete other treatment units
		*drop if lockdown_start_t!=. & iso3!="`country'" 
	    drop if lockdown_start_t<`treatdate'+7 & iso3!="`country'" 
		
		vallist iso3 if iso3!="`country'"
		local controls `r(list)'
		local i=1
		foreach placebo_country in `controls'{
		** store the results
			use pipeline/lockdown/store/synth_placebo_`country'_`i'.dta,clear
			drop if _time==.
			rename _time date
			rename _Y_treated  treat`i'
			rename _Y_synthetic counterfact`i'
			gen gap`i'=treat`i'-counterfact`i'
			gen rel_date=date-`treatdate'
			gsort rel_date
			save pipeline/lockdown/store/synth_result_`country'_`i'.dta,replace
			local i=`i'+1
		}
	restore	

	preserve
		* treatment date
		quietly sum lockdown_start_t if iso3=="`country'"
		local treatdate=`r(mean)'
			* delete other treatment units
	    drop if lockdown_start_t<`treatdate'+7 & iso3!="`country'" 
		vallist iso3 if iso3!="`country'"
		local controls `r(list)'
		local i=`i'-1
		foreach placebo_country in `controls'{
			use pipeline/lockdown/store/synth_result_`country'.dta, clear		
			forvalues j=1(1)`i'{
				merge rel_date using pipeline/lockdown/store/synth_result_`country'_`j'.dta
				drop _merge
				gsort rel_date
				save pipeline/lockdown/placebo/placebo_result_`country'.dta, replace
			} 
		}
	restore
}



