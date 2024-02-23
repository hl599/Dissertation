***********************************************************
* Regress change in rebate rate on previous markup growth *
***********************************************************
clear all
set more off
use "output05-06.dta"
tempfile temp
****** Data Cleaning ******
drop if mum1 ==. | outputr==. | cic_adj==. | export == . | ownership == . | employment == . | real_cap == .

drop if missing(mum1)
drop if missing(outputr)

****** Generate Controls ******

for var outputr real_cap: egen TX=sum(X), by(cic_adj year)
egen T1output=sum(outputr*(ownership==1)), by(cic_adj year)
egen T3output=sum(outputr*(ownership==3)), by(cic_adj year)
egen TDoutput=sum(outputr*(export == 0)), by(cic_adj year)
egen TDweighted_mum1 = sum((outputr * mum1) * (export == 0)), by(cic_adj year)

gen SOEshare=T1output/Toutputr
gen PRIshare=T3output/Toutputr
gen kl_ratio = Treal_cap / Toutputr
gen weighted_mum1 = TDweighted_mum1/TDoutput

collapse (mean) employment SOEshare PRIshare kl_ratio mum1 = weighted_mum1 (sum) export output = outputr, by(cic_adj year)
keep kl_ratio SOEshare PRIshare employment export mum1 year cic_adj

xtset cic_adj year
gen lmum1_03 = L1.mum1 							if year == 2004
gen dmum1_0103 = (L1.mum1 - L4.mum1)/3 			if year == 2004
gen INT0103 = lmum1_03 * dmum1_0103     			if year == 2004
gen dexp_0103 = log(L1.export / L4.export)/3 	if year == 2004

gen dmum1_9803 = (L1.mum1 - L6.mum1)/5 			if year == 2004
gen dexp_9803 = log(L1.export / L6.export)/5     	if year == 2004

for var kl_ratio employment: gen lX=log(X)
gen lkl01 = L4.lkl_ratio 						if year == 2004
gen lkl98 = L6.lkl_ratio 						if year == 2004
gen emp01 = L4.lemployment 						if year == 2004
gen emp98 = L6.lemployment 						if year == 2004
gen SOE01 = L4.SOEshare 						if year == 2004
gen SOE98 = L6.SOEshare 						if year == 2004
save `temp'

****** Process and merge with rebate data ******
use "rebate_2001-2007.dta"
keep if (year < 2005) & (year > 2002)

drop consumption_tax HS06 hs06
collapse (mean) rebate_rate, by(cic_adj year)
bysort cic_adj year: gen count_year = _N
gen flag_duplicate_year = count_year > 1
drop if flag_duplicate_year == 1
drop flag* count*

reshape wide rebate_rate, i(cic_adj) j(year)
gen change_rebate = rebate_rate2004 - rebate_rate2003
gen year = 2004
keep cic_adj year change_rebate
rename change_rebate dreb03
merge 1:m year cic_adj using `temp'
replace dreb03 = dreb03/100
gen cic2 = floor(cic_adj/100)

gen Treb03 = dreb03 < 0
**********************************
* rebate rate change endogeneity *
**********************************

areg dreb03 lmum1_03                  					if year==2004, a(cic2) vce(robust)
estimates store col1
areg dreb03        		dmum1_0103          			if year==2004, a(cic2) vce(robust)
estimates store col2
areg dreb03 lmum1_03	dmum1_0103 	INT0103 			if year==2004, a(cic2) vce(robust)
estimates store col3
areg dreb03 lmum1_03	dmum1_0103 	INT0103 dexp_0103 	if year==2004, a(cic2) vce(robust)
estimates store col4
areg dreb03 lmum1_03	dmum1_0103 	INT0103 dexp_0103 	lkl01	emp01	SOE01	if year==2004, a(cic2) vce(robust)
estimates store col5

**********************************
* Treatment dummy endogeneity *
**********************************

probit dreb03 lmum1_03                  					if year==2004
estimates store col1p
probit dreb03        		dmum1_0103          			if year==2004
estimates store col2p
probit dreb03 lmum1_03	dmum1_0103 	INT0103 				if year==2004
estimates store col3p
probit dreb03 lmum1_03	dmum1_0103 	INT0103 dexp_0103 		if year==2004
estimates store col4p
probit dreb03 lmum1_03	dmum1_0103 	INT0103 dexp_0103 	lkl01	emp01	SOE01	if year==2004
estimates store col5p

estimates table col1 col2 col3 col4 col5, drop(_cons) b(%5.3f) se(%5.3f)
estimates table col1 col2 col3 col4 col5, drop(_cons) b(%5.3f) star(.1 .05 .001) stats(N r2)

estimates table col1p col2p col3p col4p col5p, drop(_cons) b(%5.3f) se(%5.3f)
estimates table col1p col2p col3p col4p col5p, drop(_cons) b(%5.3f) star(.1 .05 .001) stats(N r2)