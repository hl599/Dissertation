clear
clear matrix
set more off
* Generate market-switching exposure measure

use "output05-06.dta", clear
drop if export == 0
collapse (sum) export output, by(cic_adj year)
xtset cic_adj year
gen Eshare = export/output
gen dEshare = Eshare - L.Eshare if year == 2004
keep if year == 2004
drop year export output
rename dEshare intensity
save "output08-08.dta", replace