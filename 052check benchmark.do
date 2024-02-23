clear
clear matrix
set more off

tempfile temp temp2 original temp3

***************************
* Draw historical graphs *
***************************
****** Average Markup Trend ******
use "output05-06.dta", clear
preserve

drop if missing(mum1)
drop if missing(outputr)

gen weighted_var = mum1 * outputr
collapse (sum) weighted_var outputr (mean) mum1, by(year)
gen weighted_mum1 = weighted_var / outputr

sort year
twoway (line weighted_mum1 year, lc(`specialColor')) ///
       (line mum1 year, lc(`defaultColor')), ///
    title("Average Markup over Time") ///
    xlabel(1999(1)2007) ylabel(, format(%9.0g)) ///
    legend(label(1 "Weighted markup") label(2 "Mean markup")) ///
    name(avg_mum, replace)
graph save "Average markup trend.gph", replace

restore 

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
expand 2
bysort cic_adj: gen id = _n
gen year = id + 1998
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
by cic_adj: gen bad_control = (sum(rate_increase) > 0)
by cic_adj: gen rebate_treatment = (sum(rate_decrease) > 0) & (year >= 2004)
by cic_adj: egen treatment = max(rate_decrease)
drop rate_decrease rate_increase

* Merge
merge 1:m cic_adj year using "output05-06.dta"
keep if _merge == 3
drop _merge

****** Firm margin in 2003 ******
gen margin = exp(mum1)-1
sum margin if (year == 2003)&(treatment ==1)&(export == 0)

****** Share of output in 2003 ******
gen domestic = (export == 0)
gen domestic_treatment = domestic * treatment
gen category = "Domestic firm in treatment group" if domestic_treatment == 1
replace category = "Domestic firm in control group" if domestic_treatment == 0
replace category = "Exporting firm" if export > 0
gen domestic_output = output - export


graph pie output if year == 2003, over(category) pie(2,explode) plabel(2 percent)
graph save "Share of output from domestic firms in treatment group.gph", replace
graph pie domestic_output if year == 2003, over(category) pie(2,explode) plabel(2 percent)
drop domestic* category
graph save "Share of domestic output from domestic firms in treatment group.gph", replace

****** Share of output in 2003 ******

****** Treatment v.s. Control Graph******
/*
preserve 
drop if bad_control == 1
drop if missing(mum1)
drop if missing(outputr)

* Define colors for the graph
local specialColor "red"   // Color for industries where rebate_avg_rate_change == 0
local defaultColor "blue"  // Default color for other industries

gen weighted_var = mum1 * outputr
collapse (sum) weighted_var outputr mum1, by(year treatment)
gen weighted_mum1 = weighted_var / outputr

sort treatment year
twoway (line weighted_mum1 year if treatment == 1, lc(`specialColor')) ///
       (line weighted_mum1 year if treatment == 0, lc(`defaultColor')), ///
    title("Weighted Average Markup over Time by Industry") ///
    xlabel(1999(1)2007) ylabel(, format(%9.0g)) ///
    legend(label(1 "Treatment Group") label(2 "Control Group")) ///
    name(avg_mum1_graph, replace)
graph save "Average markup, control v.s. treatment.gph", replace
restore 