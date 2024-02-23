use "rebate_2022A", clear
tempfile temp temp2 temp3

*********************************
* Prepare 2005-2007 database
*********************************
* Ensure there's a unique identifier for each observation if not already present
gen long id = _n

* Duplicate each observation 3 times to account for years 2005 to 2007
expand 3, gen(year)

* Assign each duplicate a year from 2005 to 2007
bysort id (year): replace year = 2005 + _n - 1

* Convert start_date and end_date to Stata date format assuming they are in YYYYMMDD format
gen start_date_str = string(start, "%12.0f")
gen end_date_str = string(end, "%12.0f")
gen start_date_fmt = date(start_date_str, "YMD")
gen end_date_fmt = date(end_date_str, "YMD")

* Flag observations based on whether the year is within the start and end dates
gen year_str = string(year, "%12.0f")
gen year_start_str = year_str + "0101"
gen year_start = date(year_start_str, "YMD")
gen flag = (year_start >= start_date_fmt) & (year_start < end_date_fmt)

keep if flag == 1


drop flag start* end* year_start*

sort HS year

* Collapse to ensure unique HS code-year observations, keeping the first occurrence
bysort HS year: gen first_occ = _n == 1
keep if first_occ
drop first_occ

* Generate the new hs06 variable based on the length of the HS code
gen hs06 = ""
replace hs06 = substr(HS, 1, 6) 

* Collapse the dataset to calculate the average consumptiontax and rebatetax for each year and hs06
drop if missing(consumptiontax)|missing(rebatetax)

* Drop the Name variable before collapsing
drop Name

* Collapse the dataset to calculate the average consumptiontax and rebatetax for each year and hs06, keeping the same variable names
collapse (mean) consumptiontax (mean) rebatetax, by(year hs06)
rename consumptiontax consumption_tax
rename rebatetax rebate_rate
save `temp'

*********************************
* Prepare 2004 database
*********************************

use "rebate_2004.dta", clear
rename 增值税率 consumption_tax
rename 出口退税率 rebate_rate
gen year = 2004
gen hs06 = substr(HS, 1, 6)
drop if missing(consumption_tax) | missing(rebate_rate)
collapse (mean) consumption_tax (mean) rebate_rate, by(hs06 year)
append using `temp'
sort year hs06
save `temp2'
save "check.dta", replace

*********************************
* Prepare 2001-2003 database
*********************************
use "rebate_2001-2003.dta", clear
rename ProductCode HS
reshape long rbt, i(HS) j(year)
rename rbt rebate_rate 
gen hs06 = substr(HS, 1, 6)
append using `temp2'
sort year hs06
save `temp3'

*********************************
* Convert to HS_96 version
*********************************
* Load the concordance dataset for years before 2003
tempfile concordance_2007
use "HS_07-96.dta", clear
save `concordance_2007'

* Merge based on cic02 for years before 2003
use `temp3', clear
tempfile 2007
keep if year == 2007
rename hs06 hs06_07
merge m:1 hs06_07 using `concordance_2007', nogen
rename hs06_96 hs06
save `2007'

* Load the concordance dataset for years 2003 and onwards
tempfile concordance_2002
use "HS_02-96.dta", clear
save `concordance_2002'

* Merge based on cic03 for years 2003 and onwards
use `temp3', clear
tempfile 2002
keep if (year >= 2002) & (year < 2007)
rename hs06 hs06_02
merge m:1 hs06_02 using `concordance_2002', nogen
rename hs06_96 hs06
save `2002'

use `temp3', clear
tempfile 2000
keep if year < 2002
save `2000'

* Combine the datasets
use `2007', clear
append using `2002'
append using `2000'
drop hs06_02 hs06_07
drop if missing(hs06)
drop if missing(rebate_rate)

rename hs06 HS06
destring HS06, generate(hs06)
merge m:m hs06 using "HS_CIC.dta"
keep if _merge == 3 // 1,346 observations deleted
drop _merge
drop HS
duplicates drop // 971 observations deleted

*********************
* Clean the dataset *
*********************
* Drop all hs06 that does not have a complete observation over the 7 years
bysort hs06: egen year_count = total(inrange(year, 2001, 2007))
gen complete_set = year_count == 7
drop if complete_set == 0
drop year_count complete_set

* Change all rebate rate that equals 0 before 2003 to rebate rate at 2004
bysort hs06: egen rebate_rate_2004 = max(cond(year == 2004, rebate_rate, .))
replace rebate_rate = rebate_rate_2004 if year < 2004 & rebate_rate == 0
drop rebate_rate_2004 // 42 changes

* Drop all falsely reported lines of hs06
merge m:1 hs06 using "List of positive rebate changes.dta", nogen
drop if true == 1
drop true

summarize rebate_rate if year == 2003

save "rebate_2001-2007.dta", replace



