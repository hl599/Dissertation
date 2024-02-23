clear
clear matrix
set more off

tempfile temp temp2 original temp3

*******************************
* Merge with rebate rate data *
*******************************
use "rebate_2001-2007", clear
keep cic_adj year rebate_rate
collapse (mean) rebate_rate, by(cic_adj year)
drop if missing(cic_adj)

* Generate data up to 1998
tempfile 
save `original'
keep cic_adj
duplicates drop

tempfile tempdata
gen rebate_rate = .
expand 3
bysort cic_adj: gen id = _n
gen year = id + 1997
drop id 
save `temp3'

use `original', clear
append using `temp3'

* Generate treatment dummy
sort cic_adj year
xtset cic_adj year
by cic_adj: gen rate_decrease = (rebate_rate < L.rebate_rate) & (year == 2004) & (L.year == 2003)
by cic_adj: gen rate_increase = (rebate_rate > L.rebate_rate) & (year == 2004) & (L.year == 2003)
by cic_adj: gen rebate_change = rebate_rate - L.rebate_rate if year == 2004
by cic_adj: egen control_increase = max(rate_increase)
by cic_adj: egen treatment = max(rate_decrease)
by cic_adj: gen treatment_post = treatment & (year >= 2004)

* Merge
merge 1:m cic_adj year using "output05-06.dta"
keep if _merge == 3
drop _merge

keep if export == 0

******************
* Specifications *
******************

* Generate Control Variables
rename 资产总计千元 asset
keep firm year cic_adj cic2 tfp* mum1 mum2 phi ownership bdat outputr employment deflator_output_4d deflator_input_4d export province l k w rebate* treatment* control* asset
for var asset: replace X = log(X)

egen sdcic4=sd(cic_adj), by(firm)
keep if sdcic4==0

* DID
xtset firm year
xtdidregress (mum1 l k w asset bdat tfpA)(treatment_post) , group(cic_adj) time(year) vce(cluster cic_adj)
estimates store DID


* LP-DID
lpdid mum1, unit(firm) time(year) treat(treatment_post) pre_window(5) post_window(3) controls(l k w asset tfpA) cluster(cic_adj)
graph save "Event Study Specification.gph", replace


* FE
replace rebate_rate = rebate_rate / 100
xtdidregress (mum1 l k w asset tfpA)(rebate_rate, continuous) if year == 2003 | 2004, group(cic_adj) time(year) vce(cluster cic_adj)

* xtreg mum1 rebate_rate l k w asset tfpA i.year if year == 2003|2004, fe vce(cluster cic_adj)
estimates store FE

estimates table DID FE, b(%5.3f) drop(i.year) se(%5.3f) stats(N r2)


