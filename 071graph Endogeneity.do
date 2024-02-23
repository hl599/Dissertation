*******************************************************
* Graph change in rebate rate on initial rebate level *
*******************************************************

clear all
set more off
use "rebate_2001-2007.dta"
keep if year == 2003 | year == 2004

****** HS06 ******
preserve
drop consumption_tax HS06 cic_adj

* Get rid of missing data
bysort hs06 year: gen count_year = _N
gen flag_duplicate_year = count_year > 1
drop if flag_duplicate_year == 1
drop flag* count*

reshape wide rebate_rate, i(hs06) j(year)
gen change_rebate = rebate_rate2004 - rebate_rate2003
summarize rebate_rate2003, detail
local min_rebate = r(min)
local max_rebate = r(max)

twoway  (scatter change_rebate rebate_rate2003) ///
        (lfit change_rebate rebate_rate2003, color(black) lpattern(dash)) ///
		(function y = -x + 13, range(`min_rebate' `max_rebate') color(blue)) ///
        , xtitle("Rebate Rate 2003") ///
          ytitle("Change in VAT Rebate")
graph save "Endogeneity at HS06 level.gph", replace

histogram change_rebate, percent width(0.5)
graph save "Distribution of rebate change at HS06 level.gph", replace

gen hs02 = floor(hs06/10000)
gen treatment_increase = change_rebate > 0
collapse (sum) treatment_increase, by(hs02)
sort hs02
scatter treatment_increase hs02
graph save "Industries that experience a rise in rebate rate (HS06 level).gph", replace

restore

****** CIC ******


drop consumption_tax HS06 hs06
collapse (mean) rebate_rate, by(cic_adj year)
bysort cic_adj year: gen count_year = _N
gen flag_duplicate_year = count_year > 1
drop if flag_duplicate_year == 1
drop flag* count*

reshape wide rebate_rate, i(cic_adj) j(year)
gen change_rebate = rebate_rate2004 - rebate_rate2003
summarize rebate_rate2003, detail
local min_rebate = r(min)
local max_rebate = r(max)

twoway  (scatter change_rebate rebate_rate2003) ///
        (lfit change_rebate rebate_rate2003, color(black) lpattern(dash)) ///
		(function y = -x + 13, range(`min_rebate' `max_rebate') color(blue)) ///
        , xtitle("Rebate Rate 2003") ///
          ytitle("Change in VAT Rebate")
graph save "Endogeneity at CIC level.gph", replace

histogram change_rebate, percent width(0.5)
graph save "Distribution of rebate change at CIC level.gph", replace // Proportion of CIC industries that experience positive/no/negative rebate change are respectively 9%/19%/62%

gen cic2 = floor(cic_adj/100)
gen treatment_increase = change_rebate > 0
collapse (sum) treatment_increase, by(cic2)
sort cic2
scatter treatment_increase cic2
graph save "Industries that experience a rise in rebate rate (CIC level).gph", replace



